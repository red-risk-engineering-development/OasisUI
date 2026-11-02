# Results cache for DataHub ----
#
# Analysis input/output bundles (<uuid>.tar.gz) used to be downloaded over HTTP
# and gunzipped from the start for every single operation (list, header, row
# count, size, content ...). The helpers below:
#   - locate the bundle directly on the shared volume (read-only mount),
#   - extract it ONCE into a shared cache directory (parallel gunzip if pigz
#     is available),
#   - keep only the N most recently used analyses (LRU),
#   - let DataHub read plain files from the cache.
#
# Everything is switchable and falls back to the original behaviour:
#   OASISUI_RESULTS_CACHE   "1" (default) / "0" to disable
#   OASISUI_SHARED_FS       path of the shared volume (default /shared-fs)
#   OASISUI_CACHE_DIR       cache directory (default /cache/results, else tempdir)
#   OASISUI_CACHE_MAX_ANALYSES  analyses kept (default 5). Each analysis has an
#                           input and an output bundle, so 2 x this many cache entries
#   OASISUI_CACHE_MAX_GB    total size cap in GB (default 60)
#   OASISUI_VIEW_MAX_ROWS   rows loaded in the file viewer (default 20000)
#   OASISUI_PERF_LOG        "1" to log timings to the console


.rc_env <- function(name, default) {
  val <- Sys.getenv(name, unset = NA_character_)
  if (is.na(val) || !nzchar(val)) default else val
}

.rc_enabled <- function() {
  .rc_env("OASISUI_RESULTS_CACHE", "1") != "0"
}

.rc_shared_fs <- function() {
  .rc_env("OASISUI_SHARED_FS", "/shared-fs")
}

.rc_max_analyses <- function() {
  max(1L, suppressWarnings(as.integer(.rc_env("OASISUI_CACHE_MAX_ANALYSES", "5"))), na.rm = TRUE)
}

# entries (extracted bundles) kept: an analysis can have an input and an output bundle
.rc_max_entries <- function() {
  2L * .rc_max_analyses()
}

# run the LRU eviction at most every 5 minutes when the cache is hit (it otherwise
# only runs after an extraction, so rapid navigation could leave it above its limit)
.rc_state <- new.env()
.rc_maybe_evict <- function(keep = character(0)) {
  last <- .rc_state$last_evict
  if (is.null(last) || difftime(Sys.time(), last, units = "secs") > 300) {
    .rc_state$last_evict <- Sys.time()
    .rc_evict(keep = keep)
  }
  invisible()
}

.rc_max_bytes <- function() {
  gb <- suppressWarnings(as.numeric(.rc_env("OASISUI_CACHE_MAX_GB", "60")))
  if (is.na(gb) || gb <= 0) gb <- 60
  gb * 1024^3
}

#' maximum rows loaded in the file viewer
#' @noRd
.rc_view_max_rows <- function() {
  n <- suppressWarnings(as.numeric(.rc_env("OASISUI_VIEW_MAX_ROWS", "20000")))
  if (is.na(n) || n <= 0) Inf else n
}

.rc_cache_dir <- function() {
  dir <- .rc_env("OASISUI_CACHE_DIR", "/cache/results")
  ok <- dir.exists(dir) || suppressWarnings(dir.create(dir, recursive = TRUE, showWarnings = FALSE))
  if (!ok || file.access(dir, 2) != 0) {
    dir <- file.path(tempdir(), "results_cache")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  dir
}

# Perf logging ----

.rc_perf_on <- function() {
  .rc_env("OASISUI_PERF_LOG", "0") == "1"
}

#' time an expression and log it when OASISUI_PERF_LOG=1
#' @noRd
.rc_timed <- function(label, expr) {
  if (!.rc_perf_on()) {
    return(expr)
  }
  t0 <- proc.time()[["elapsed"]]
  on.exit(message(sprintf("[PERF] %-48s %8.2f s", label, proc.time()[["elapsed"]] - t0)), add = TRUE)
  expr
}

# Bundle location ----

#' Resolve the on-disk path of an analysis bundle, or NULL.
#'
#' Asks the API (authenticated) for `analyses/{id}/storage_links/`, which on
#' local-fs storage returns the stored file names. The file is then looked up
#' in the read-only shared volume. NULL means "use the HTTP download instead".
#' @noRd
.rc_locate_bundle <- function(oasisapi, id, label) {
  tryCatch({
    resp <- oasisapi$api_get_query(paste("analyses", id, "storage_links", sep = "/"))
    if (is.null(resp$result) || httr::status_code(resp$result) != 200) {
      return(NULL)
    }
    links <- httr::content(resp$result, as = "parsed", type = "application/json")
    fname <- links[[paste0(label, "_file")]]
    if (is.null(fname) || !is.character(fname) || length(fname) != 1 || !nzchar(fname)) {
      return(NULL)
    }
    # local-fs returns a bare file name; anything else (URL, S3 key) is not usable here
    if (grepl("/|\\\\", fname)) {
      return(NULL)
    }
    path <- file.path(.rc_shared_fs(), fname)
    if (!file.exists(path)) {
      return(NULL)
    }
    path
  }, error = function(e) {
    NULL
  })
}

# Extraction ----

.rc_touch <- function(dir) {
  f <- file.path(dir, ".last_used")
  if (!file.exists(f)) {
    file.create(f)
  }
  Sys.setFileTime(f, Sys.time())
  invisible()
}

.rc_dir_size <- function(dir) {
  files <- list.files(dir, recursive = TRUE, all.files = TRUE, full.names = TRUE)
  sum(file.size(files), na.rm = TRUE)
}

.rc_extract_cmd <- function(archive, dest) {
  is_gz <- grepl("\\.(tar\\.gz|tgz|gz)$", archive)
  if (is_gz && nzchar(Sys.which("pigz"))) {
    sprintf("set -o pipefail; pigz -dc %s | tar -xf - -C %s", shQuote(archive), shQuote(dest))
  } else {
    sprintf("set -o pipefail; tar -xf %s -C %s", shQuote(archive), shQuote(dest))
  }
}

#' Extract an archive once into the shared cache and return the directory.
#'
#' Extraction goes to a temporary directory that is renamed into place only when
#' complete, so a crash or a concurrent request never exposes a half-extracted
#' analysis. A lock directory makes concurrent requests for the same bundle wait
#' instead of extracting twice. Returns NULL on failure (caller falls back).
#' @noRd
.rc_ensure_extracted <- function(archive, key, wait_secs = 3600) {
  cache <- .rc_cache_dir()
  final <- file.path(cache, key)
  if (dir.exists(final)) {
    .rc_touch(final)
    .rc_maybe_evict(keep = key)
    return(final)
  }

  lock <- file.path(cache, paste0(".lock-", key))
  deadline <- Sys.time() + wait_secs
  repeat {
    if (dir.exists(final)) {
      .rc_touch(final)
      return(final)
    }
    if (dir.create(lock, showWarnings = FALSE)) {
      break
    }
    # steal stale locks (crashed extraction)
    age <- as.numeric(difftime(Sys.time(), file.mtime(lock), units = "secs"))
    if (!is.na(age) && age > 6 * 3600) {
      unlink(lock, recursive = TRUE)
      next
    }
    if (Sys.time() > deadline) {
      return(NULL)
    }
    Sys.sleep(1)
  }
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)

  if (dir.exists(final)) {
    .rc_touch(final)
    return(final)
  }

  tmp <- file.path(cache, paste0(".tmp-", key, "-", Sys.getpid(), "-", format(Sys.time(), "%H%M%S")))
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  status <- .rc_timed(
    paste0("extract ", key),
    system2("bash", c("-c", shQuote(.rc_extract_cmd(archive, tmp))), stdout = FALSE, stderr = FALSE)
  )
  if (!identical(as.integer(status), 0L)) {
    unlink(tmp, recursive = TRUE)
    return(NULL)
  }
  .rc_touch(tmp)
  if (!file.rename(tmp, final)) {
    unlink(tmp, recursive = TRUE)
    if (!dir.exists(final)) {
      return(NULL)
    }
  }
  .rc_evict(keep = key)
  final
}

#' LRU eviction: keep the N most recently used analyses and stay below the size cap.
#' Never evicts `keep`, nor anything used within the last 10 minutes.
#' @noRd
.rc_evict <- function(keep = character(0)) {
  tryCatch({
    cache <- .rc_cache_dir()
    entries <- list.dirs(cache, recursive = FALSE, full.names = FALSE)
    entries <- entries[!startsWith(entries, ".")]
    if (length(entries) == 0) {
      return(invisible())
    }
    last_used <- vapply(entries, function(e) {
      f <- file.path(cache, e, ".last_used")
      if (file.exists(f)) as.numeric(file.mtime(f)) else as.numeric(file.mtime(file.path(cache, e)))
    }, numeric(1))
    entries <- entries[order(last_used, decreasing = TRUE)]
    last_used <- sort(last_used, decreasing = TRUE)
    recent <- last_used > as.numeric(Sys.time()) - 600

    drop <- function(e) unlink(file.path(cache, e), recursive = TRUE)

    # 1. count limit
    max_n <- .rc_max_entries()
    if (length(entries) > max_n) {
      for (i in seq(max_n + 1, length(entries))) {
        if (!(entries[i] %in% keep) && !recent[i]) drop(entries[i])
      }
    }
    # 2. size limit (oldest first)
    entries <- list.dirs(cache, recursive = FALSE, full.names = FALSE)
    entries <- entries[!startsWith(entries, ".")]
    sizes <- vapply(entries, function(e) .rc_dir_size(file.path(cache, e)), numeric(1))
    lu <- vapply(entries, function(e) {
      f <- file.path(cache, e, ".last_used")
      if (file.exists(f)) as.numeric(file.mtime(f)) else 0
    }, numeric(1))
    ord <- order(lu)
    total <- sum(sizes)
    for (i in ord) {
      if (total <= .rc_max_bytes()) break
      e <- entries[i]
      if (e %in% keep || lu[i] > as.numeric(Sys.time()) - 600) next
      drop(e)
      total <- total - sizes[i]
    }
  }, error = function(e) NULL)
  invisible()
}

# Reading ----

#' Read a dataset file from disk (csv, json, parquet or text).
#'
#' @param path file to read.
#' @param nrows rows to read for csv (Inf for all).
#' @param select columns to read for csv (NULL for all). If the selected columns
#'   do not exist, the full file is read instead.
#' @noRd
.rc_read_file <- function(path, nrows = Inf, select = NULL) {
  if (!file.exists(path)) {
    return(NULL)
  }
  extension <- tolower(tools::file_ext(path))
  if (extension == "csv") {
    nr <- if (is.finite(nrows)) nrows else Inf
    if (!is.null(select)) {
      data <- tryCatch(
        data.table::fread(path, nrows = nr, select = select),
        error = function(e) NULL,
        warning = function(w) NULL
      )
      if (!is.null(data)) {
        return(data)
      }
    }
    data.table::fread(path, nrows = nr)
  } else if (extension == "json") {
    jsonlite::read_json(path)
  } else if (extension == "parquet") {
    arrow::read_parquet(path)
  } else {
    scan(path, what = "", sep = "\n", quiet = TRUE)
  }
}

#' Line count of a csv (minus header), cached in a sidecar file next to it.
#' @noRd
.rc_nrow <- function(path) {
  sidecar <- paste0(path, ".nrow")
  if (file.exists(sidecar)) {
    n <- suppressWarnings(as.numeric(readLines(sidecar, n = 1, warn = FALSE)))
    if (!is.na(n)) {
      return(n)
    }
  }
  n <- .rc_timed(paste0("wc -l ", basename(path)), {
    out <- system2("wc", c("-l", shQuote(path)), stdout = TRUE)
    as.numeric(strsplit(trimws(out), " ")[[1]][1]) - 1 # remove header
  })
  try(writeLines(as.character(n), sidecar), silent = TRUE)
  n
}

#' List dataset files of an extracted bundle (relative paths, no directories).
#' @noRd
.rc_list_files <- function(dir) {
  files <- list.files(dir, recursive = TRUE, all.files = FALSE, full.names = FALSE)
  files[!grepl("\\.nrow$", files)]
}

#' Resolve a dataset identifier inside an extracted bundle.
#' @noRd
.rc_resolve <- function(dir, dataset_identifier, type = "") {
  cand <- c(file.path(dir, dataset_identifier),
            if (nzchar(type)) file.path(dir, type, dataset_identifier))
  cand <- cand[file.exists(cand)]
  if (length(cand) == 0) NULL else cand[1]
}
