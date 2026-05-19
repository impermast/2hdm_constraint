#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

# ==== фиксированные гриды (примеры) ====
GRID_SQRTS0="240, 1000, 10000, 13000"
GRID_MH2="200:5:800"
GRID_MHC="400"

PID="dd"
PROC="ZZZ"

# флаги только для pID/proc
while [[ $# -gt 0 ]]; do
  case "$1" in
    --pid)  PID="$2"; shift 2;;
    --proc) PROC="$2"; shift 2;;
    -h|--help) echo "Flags: --pid <dd|uu|...>  --proc <ZWW|ZZZ|...>"; exit 0;;
    *) echo "Unknown arg: $1"; exit 1;;
  esac
done


parse_grid() {
  local spec="${1:-}" arr_name="${2:-}" cnt_var="${3:-}"
  local tok start step stop inc v
  local -a out=()

  if [[ -z "$spec" ]]; then
    [[ -n "$arr_name" ]] && eval "$arr_name=()"
    [[ -n "$cnt_var"  ]] && eval "$cnt_var=0"
    return
  fi

  local IFS=','; local parts
  read -r -a parts <<< "$spec"

  for tok in "${parts[@]}"; do
    tok="${tok//[[:space:]]/}"
    if [[ "$tok" == *:*:* ]]; then
      IFS=':' read -r start step stop <<< "$tok"
      [[ "$step" == "0" || -z "$step" ]] && continue
      if (( start <= stop )); then inc="$step"; else inc="-$step"; fi
      while IFS= read -r v; do out+=("$v"); done < <(seq "$start" "$inc" "$stop")
    elif [[ -n "$tok" ]]; then
      out+=("$tok")
    fi
  done

  eval "$arr_name=(\"\${out[@]}\")"
  eval "$cnt_var=${#out[@]}"
}


# --- быстрый индекс CSV (O(1) проверка) ---
CSV_FILE="buffer/f4mean_${PID}${PROC}.csv"
declare -A HAVE

build_index() {
  local file="$1"
  [[ -f "$file" ]] || return 0
  while IFS= read -r k; do
    HAVE["$k"]=1
  done < <(
    awk -F',' '
      function trim(s){gsub(/^[ \t"]+|[ \t"]+$/,"",s); return s}
      function norm(x){x=trim(x); return (x==""||x=="None")?"None":sprintf("%.12g", x+0)}
      NR==1{
        for(i=1;i<=NF;i++){
          h=trim($i);
          if(h=="sqrts0") iS=i; else if(h=="mh2") iH2=i; else if(h=="mhc") iHC=i;
        }
        next
      }
      { print norm($iS) "|" norm($iH2) "|" norm($iHC) }
    ' "$file"
  )
}

key_for() {
  local s="$1" m="$2" c="${3:-None}" kc
  [[ -z "$c" || "$c" == "None" ]] && kc="None" || kc=$(printf '%.12g' "$c")
  printf '%.12g|%.12g|%s' "$s" "$m" "$kc"
}

# calculate_f4mean sqrts0 mh2 mhc
calculate_f4mean() {
  local sqrts0="$1" mh2="$2" mhc="${3:-None}"
  echo "[f4mean] sqrts0=${sqrts0}  mh2=${mh2}  mhc=${mhc}  pid=${PID}  proc=${PROC}"
  wolframscript -file SCRIPT_f4meanvalue.wls \
    "$sqrts0" "$mh2" "$mhc" "$PID" "$PROC" False
}




__PROG_TOTAL=0
__PROG_START_TS=0

progress_init() {
    # $1 = total_steps
    __PROG_TOTAL="$1"
    __PROG_START_TS="$(date +%s)"
    progress_update 0
}

progress_update() {
    # $1 = current_step
    cur="$1"
    total="$__PROG_TOTAL"

    # проценты целые: pct = 100 * cur / total
    if [ "$total" -gt 0 ]; then
        pct=$(( 100 * cur / total ))
    else
        pct=0
    fi

    printf '[progress] step %s/%s (%s%%)\n' "$cur" "$total" "$pct" >&2
}

progress_done() {
    end_ts="$(date +%s)"
    dur=$(( end_ts - __PROG_START_TS ))
    printf '[progress] total runtime %ss\n' "$dur" >&2
    progress_update "$__PROG_TOTAL"
    printf '[progress] done\n' >&2
}






parse_grid "$GRID_SQRTS0" vals_sqrts0 n_sqrts0
parse_grid "$GRID_MH2"    vals_mh2    n_mh2
parse_grid "$GRID_MHC"    vals_mhc    n_mhc

total_steps=$(( n_sqrts0 * n_mh2 * n_mhc )) 
# построить индекс существующих точек один раз
build_index "$CSV_FILE"

progress_init "$total_steps"
step=0

for s in "${vals_sqrts0[@]}"; do
  for c in "${vals_mhc[@]}"; do
    for m in "${vals_mh2[@]}"; do
      k=$(key_for "$s" "$m" "$c")
      if [[ -n "${HAVE[$k]:-}" ]]; then
        echo "[skip] exists: $k in $CSV_FILE"
        continue
      fi
      step=$(( step + 1 ))
      calculate_f4mean "$s" "$m" "$c"
      HAVE["$k"]=1
      progress_update "$step"
    done
  done
done
progress_done
echo "Done."
