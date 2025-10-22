#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

# ==== фиксированные гриды (примеры) ====
GRID_SQRTS0="1000"                                        # одно значение или "180:20:260,300"
GRID_MH2="126:5:150,160:10:200,210:10:300,400:100:600"   # интервалы через запятую
GRID_MHC="400, 600, 800"                                              # пусто => не перебирать mhc

PID="dd"
PROC="ZWW"

# флаги только для pID/proc
while [[ $# -gt 0 ]]; do
  case "$1" in
    --pid)  PID="$2"; shift 2;;
    --proc) PROC="$2"; shift 2;;
    -h|--help) echo "Flags: --pid <dd|uu|...>  --proc <ZWW|ZZZ|...>"; exit 0;;
    *) echo "Unknown arg: $1"; exit 1;;
  esac
done

# парсер спецификации: "a:b:c, x, y:z:w"
parse_grid() {
  local spec="${1:-}" tok start step stop out=()
  [[ -z "$spec" ]] && { echo ""; return; }
  IFS=',' read -r -a parts <<< "$spec"
  for tok in "${parts[@]}"; do
    tok="${tok//[[:space:]]/}"
    if [[ "$tok" == *:*:* ]]; then
      IFS=':' read -r start step stop <<< "$tok"
      while IFS= read -r v; do out+=("$v"); done < <(seq "$start" "$step" "$stop")
    elif [[ -n "$tok" ]]; then
      out+=("$tok")
    fi
  done
  echo "${out[@]}"
}

# calculate_f4mean sqrts0 mh2 mhc
calculate_f4mean() {
  local sqrts0="$1" mh2="$2" mhc="${3:-None}"
  echo "[f4mean] sqrts0=${sqrts0}  mh2=${mh2}  mhc=${mhc}  pid=${PID}  proc=${PROC}"
  wolframscript -file SCRIPT_f4meanvalue.wls \
    "$sqrts0" "$mh2" "$mhc" "$PID" "$PROC" False
}

# построение осей
vals_sqrts0=( $(parse_grid "$GRID_SQRTS0") )
vals_mh2=(    $(parse_grid "$GRID_MH2") )
tmp_mhc="$(parse_grid "$GRID_MHC")"
if [[ -z "$tmp_mhc" ]]; then vals_mhc=("None"); else vals_mhc=( $tmp_mhc ); fi

# запуск по декартову произведению
for s in "${vals_sqrts0[@]}"; do
  for c in "${vals_mhc[@]}"; do
    for m in "${vals_mh2[@]}"; do
      calculate_f4mean "$s" "$m" "$c"
    done
  done
done

echo "Done."
