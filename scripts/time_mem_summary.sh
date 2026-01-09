#!/bin/sh
set -u

usage() {
  cat <<'EOF'
Usage: scripts/time_mem_summary.sh [--table|--raw] LOGFILE
       scripts/time_mem_summary.sh [--table|--raw] - < LOGFILE

Prints per-command count, mean/median RSS (MiB), and mean/median real time (ms).
EOF
}

format=""

while [ "$#" -gt 0 ]; do
  case "$1" in
    --help|-h)
      usage
      exit 0
      ;;
    --table)
      format="table"
      shift
      ;;
    --raw)
      format="raw"
      shift
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

if [ -z "$format" ] && [ -t 1 ]; then
  format="table"
elif [ -z "$format" ]; then
  format="raw"
fi

if [ "${1:-}" = "--help" ] || [ "${1:-}" = "-h" ]; then
  usage
  exit 0
fi

if [ "$#" -ne 1 ]; then
  usage
  exit 2
fi

log="$1"
tmp=""

if [ "$log" = "-" ]; then
  tmp="$(mktemp -t blots-time-mem-summary.XXXXXX)"
  cat > "$tmp"
  log="$tmp"
fi

if [ ! -f "$log" ]; then
  printf "log file not found: %s\n" "$log" >&2
  [ -n "$tmp" ] && rm -f "$tmp"
  exit 1
fi

out_tmp="$(mktemp -t blots-time-mem-summary-out.XXXXXX)"
printf "command\tcount\tmean_rss_mib\tmedian_rss_mib\tmean_real_ms\tmedian_real_ms\n" >> "$out_tmp"

awk -F '\t' 'NF>=3 {print $3}' "$log" | sort -u | while IFS= read -r cmd; do
  count="$(awk -F '\t' -v cmd="$cmd" '$3==cmd {n++} END {print n+0}' "$log")"
  if [ "$count" -eq 0 ]; then
    continue
  fi

  mean_rss="$(awk -F '\t' -v cmd="$cmd" '$3==cmd {sum+=$1; n++} END {if (n) printf "%.3f", sum/n/1048576.0; else print 0}' "$log")"
  mean_real="$(awk -F '\t' -v cmd="$cmd" '$3==cmd {sum+=$2; n++} END {if (n) printf "%.3f", (sum/n)*1000.0; else print 0}' "$log")"

  median_rss="$(awk -F '\t' -v cmd="$cmd" '$3==cmd {print $1}' "$log" | sort -n | \
    awk -v n="$count" 'NR==int((n+1)/2) {m1=$1} NR==int((n+2)/2) {m2=$1} END {if (n%2) printf "%.3f", m1/1048576.0; else printf "%.3f", (m1+m2)/2/1048576.0}')"

  median_real="$(awk -F '\t' -v cmd="$cmd" '$3==cmd {print $2}' "$log" | sort -n | \
    awk -v n="$count" 'NR==int((n+1)/2) {m1=$1} NR==int((n+2)/2) {m2=$1} END {if (n%2) printf "%.3f", m1*1000.0; else printf "%.3f", (m1+m2)/2*1000.0}')"

  printf "%s\t%s\t%s\t%s\t%s\t%s\n" "$cmd" "$count" "$mean_rss" "$median_rss" "$mean_real" "$median_real" >> "$out_tmp"
done

[ "$format" = "table" ] && command -v column >/dev/null 2>&1 && column -t -s "$(printf '\t')" "$out_tmp" || cat "$out_tmp"

[ -n "$out_tmp" ] && rm -f "$out_tmp"
[ -n "$tmp" ] && rm -f "$tmp"
