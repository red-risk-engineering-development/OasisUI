#!/usr/bin/env bash
# Extract an analysis result bundle ONCE into the shared results cache.
#
#   rc_extract.sh <archive> <cache_dir> <key> [<pidfile>]
#
# - extracts into a temporary directory and renames it into place only when
#   complete, so a half-extracted analysis is never visible
# - a lock directory (with the owner pid) makes concurrent requests for the same
#   bundle wait for the first one instead of extracting twice; locks of dead
#   owners are taken over
# - on TERM/INT (cancel) the temporary directory and the lock are removed
# - after extraction, row counts of the csv files are written next to them
#   (<file>.nrow) so the viewer never has to count lines of a huge file
#
# Used both blocking (R waits) and in the background (R polls the cache folder).
# Exit status 0: bundle is in the cache; 1: failed.

set -u
archive="$1"; cache="$2"; key="$3"; pidfile="${4:-}"
final="$cache/$key"
lock="$cache/.lock-$key"
tmp=""
have_lock=0

[ -n "$pidfile" ] && echo $$ > "$pidfile"

# pid exists and is not a zombie
alive() {
  local pid="$1" state
  [ -n "$pid" ] || return 1
  state=$(awk '{print $3}' "/proc/$pid/stat" 2>/dev/null) || return 1
  [ -n "$state" ] && [ "$state" != "Z" ]
}

cleanup() {
  [ -n "$tmp" ] && rm -rf "$tmp"
  [ "$have_lock" = 1 ] && rm -rf "$lock"
  [ -n "$pidfile" ] && rm -f "$pidfile"
}
trap cleanup EXIT
trap 'exit 143' TERM INT

[ -d "$final" ] && exit 0
[ -f "$archive" ] || exit 1
mkdir -p "$cache" || exit 1

# acquire the lock (or notice that someone else finished the extraction)
while true; do
  [ -d "$final" ] && exit 0
  if mkdir "$lock" 2>/dev/null; then
    echo $$ > "$lock/pid"
    have_lock=1
    break
  fi
  owner=$(cat "$lock/pid" 2>/dev/null || true)
  if [ -n "$owner" ]; then
    if ! alive "$owner"; then rm -rf "$lock"; continue; fi
  else
    # lock created but pid not written yet (or owner died in between): give it a minute
    if [ -n "$(find "$lock" -maxdepth 0 -mmin +1 2>/dev/null)" ]; then rm -rf "$lock"; continue; fi
  fi
  sleep 1
done

[ -d "$final" ] && exit 0

tmp="$cache/.tmp-$key-$$"
rm -rf "$tmp"
mkdir -p "$tmp" || exit 1

set -o pipefail
case "$archive" in
  *.gz)
    if command -v pigz >/dev/null 2>&1; then
      pigz -dc "$archive" | tar -xf - -C "$tmp" 2>/dev/null
    else
      tar -xzf "$archive" -C "$tmp" 2>/dev/null
    fi
    ;;
  *)
    tar -xf "$archive" -C "$tmp" 2>/dev/null
    ;;
esac
status=$?
# "Removing leading /" warnings give status 0; real failures do not
[ "$status" -eq 0 ] || exit 1

touch "$tmp/.last_used"
if mv -T "$tmp" "$final" 2>/dev/null; then
  tmp=""
elif [ ! -d "$final" ]; then
  exit 1
fi

# release the lock early: waiters can proceed, the counts below are best effort
rm -rf "$lock"; have_lock=0

find "$final" -type f -name '*.csv' -print0 | while IFS= read -r -d '' f; do
  [ -e "$f.nrow" ] && continue
  n=$(wc -l < "$f" 2>/dev/null) || continue
  d=$(dirname "$f"); b=$(basename "$f")
  echo $(( n > 0 ? n - 1 : 0 )) > "$d/.$b.nrow.tmp" && mv -f "$d/.$b.nrow.tmp" "$f.nrow"
done
exit 0
