#!/usr/bin/env bash
set -euo pipefail

RUNS=${RUNS:-4}

sum="0"
decimals=6
got_decimals=0

for ((i=1; i<=RUNS; i++)); do
  out=$(dune exec --profile release camelfuck -- bfprograms/mandelbrot.bf)

  # From the last two lines, pick the one that ends with "to run",
  # take the token before "to" (e.g., "5.827005s"), strip the trailing "s".
  t=$(
    printf "%s\n" "$out" \
    | tail -n 2 \
    | awk '/to run$/ { v=$(NF-2); sub(/s$/,"",v); print v }' \
    | tail -n 1
  )

  if [[ -z "${t:-}" ]]; then
    echo "Failed to parse run time from program output." >&2
    exit 2
  fi

  if [[ $got_decimals -eq 0 ]]; then
    if [[ "$t" == *.* ]]; then
      frac=${t#*.}
      decimals=${#frac}
    else
      decimals=0
    fi
    got_decimals=1
  fi

  echo "run $i: ${t}s"

  # accumulate with awk for reliable floating-point math
  sum=$(awk -v s="$sum" -v x="$t" 'BEGIN{printf "%.15f", s + x}')
done

avg=$(awk -v s="$sum" -v n="$RUNS" 'BEGIN{printf "%.15f", s/n}')
# print only the average in the same "Xs" format (no label)
printf "%.*fs\n" "$decimals" "$avg"
