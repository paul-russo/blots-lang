#!/bin/sh
set -u

usage() {
  cat <<'EOF'
Usage: scripts/time_mem.sh [--quiet] [--passthrough] [--log FILE] -- <command> [args...]
       TIME_MEM_LOG=FILE scripts/time_mem.sh <command> [args...]

Options:
  --quiet        Suppress command stdout/stderr.
  --passthrough  Re-emit captured stderr (includes /usr/bin/time -l output).
  --log FILE     Append "max_rss_bytes<TAB>real_s<TAB>command" to FILE.
EOF
}

quiet=0
passthrough=0
log_file="${TIME_MEM_LOG:-}"

while [ "$#" -gt 0 ]; do
  case "$1" in
    --quiet)
      quiet=1
      shift
      ;;
    --passthrough)
      passthrough=1
      shift
      ;;
    --log)
      log_file="${2:-}"
      shift 2
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    --)
      shift
      break
      ;;
    *)
      break
      ;;
  esac
done

if [ "$#" -eq 0 ]; then
  usage
  exit 2
fi

tmp="$(mktemp -t blots-time-mem.XXXXXX)"

if [ "$quiet" -eq 1 ]; then
  /usr/bin/time -l "$@" >/dev/null 2> "$tmp"
else
  /usr/bin/time -l "$@" 2> "$tmp"
fi
status=$?

max_rss="$(awk '/maximum resident set size/ {rss=$1} END {print rss}' "$tmp")"
real_s="$(awk '$2=="real" {t=$1} END {print t}' "$tmp")"

if [ "$passthrough" -eq 1 ] && [ "$quiet" -eq 0 ]; then
  cat "$tmp" >&2
fi

if [ -n "$log_file" ]; then
  printf "%s\t%s\t%s\n" "${max_rss:-0}" "${real_s:-0}" "$*" >> "$log_file"
else
  printf "max_rss_bytes=%s real_s=%s\n" "${max_rss:-0}" "${real_s:-0}"
fi

rm -f "$tmp"
exit "$status"
