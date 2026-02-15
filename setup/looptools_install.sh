#!/usr/bin/env bash
set -euo pipefail

VER="${VER:-2.16}"

ROOT="$(cd "$(dirname "$0")" && pwd)"
DL="$ROOT/downloads"
SRC="$ROOT/src"
TMP="$ROOT/tmp"
mkdir -p "$DL" "$SRC" "$TMP"

BASE_URL="http://feynarts.de/looptools"
SRC_URL="$BASE_URL/LoopTools-$VER.tar.gz"
EXE_URL="$BASE_URL/LoopTools-Cygwin.exe.gz"

need() { command -v "$1" >/dev/null 2>&1 || { echo "ERROR: need '$1'"; exit 1; }; }
dl()   { local url="$1" out="$2"; [ -f "$out" ] || curl -L --fail -o "$out" "$url"; }

to_mma_path() { # /c/Users/x -> C:/Users/x
  local p="${1//\\//}"
  if [[ "$p" =~ ^/([a-zA-Z])/(.*)$ ]]; then
    local d="${BASH_REMATCH[1]}"
    echo "${d^^}:/${BASH_REMATCH[2]}"
  else
    echo "$p"
  fi
}

MATH_BIN="$(command -v math || true)"
if [ -z "${MATH_BIN:-}" ]; then
  echo "ERROR: 'math' not found in PATH (Mathematica kernel)."
  exit 1
fi

OS="$(uname -s)"

# ---------- Windows block (MSYS2/Git Bash/Cygwin bash) ----------
install_windows() {
  need curl; need tar; need gzip; need xz; need awk

  # Derive Mathematica root from math path:
  # .../Mathematica/<ver>/SystemFiles/Kernel/Binaries/Windows-x86-64/math
  local mma
  mma="$(cd "$(dirname "$MATH_BIN")/../../../../" && pwd)"

  # User Applications from %APPDATA%
  if [ -z "${APPDATA:-}" ]; then
    echo "ERROR: APPDATA is empty (Windows env)."
    exit 1
  fi
  local roaming="$APPDATA"
  roaming="${roaming//\\//}"
  if [[ "$roaming" =~ ^([A-Za-z]):/(.*)$ ]]; then
    local d="${BASH_REMATCH[1],,}"
    roaming="/$d/${BASH_REMATCH[2]}"
  fi

  local apps="$roaming/Mathematica/Applications"
  local lt="$apps/LoopTools"
  local bin="$lt/Windows-x86-64"
  mkdir -p "$bin"

  echo "[win] download..."
  dl "$SRC_URL" "$DL/LoopTools-$VER.tar.gz"
  dl "$EXE_URL" "$DL/LoopTools-Cygwin.exe.gz"

  echo "[win] unpack (source optional)..."
  tar -xzf "$DL/LoopTools-$VER.tar.gz" -C "$SRC" >/dev/null 2>&1 || true

  echo "[win] install LoopTools.exe..."
  gzip -cd "$DL/LoopTools-Cygwin.exe.gz" > "$bin/LoopTools.exe"

  echo "[win] copy Mathematica DLLs..."
  local dll="$mma/SystemFiles/Libraries/Windows-x86-64"
  cp -f "$dll/ml64i4.dll"   "$bin/" 2>/dev/null || true
  cp -f "$dll/wstp64i4.dll" "$bin/" 2>/dev/null || true

  echo "[win] ensure cygwin1.dll (no installer, from cygwin.com/ftp)..."
  if [ -f "$bin/cygwin1.dll" ]; then
    : # ok
  elif [ -f "/c/cygwin64/bin/cygwin1.dll" ]; then
    cp -f "/c/cygwin64/bin/cygwin1.dll" "$bin/"
  elif [ -f "/c/cygwin/bin/cygwin1.dll" ]; then
    cp -f "/c/cygwin/bin/cygwin1.dll" "$bin/"
  else
    # get latest cygwin package path from setup.xz
    dl "https://cygwin.com/ftp/cygwin/x86_64/setup.xz" "$DL/setup.xz"
    local relpath
    relpath="$(xz -dc "$DL/setup.xz" | awk '
      $1=="@" && $2=="cygwin" {in=1; next}
      in && $1=="install:" {print $2; exit}
    ')"
    if [ -z "$relpath" ]; then
      echo "ERROR: cannot parse setup.xz for cygwin package"
      exit 1
    fi
    local cyg_url="https://cygwin.com/ftp/cygwin/$relpath"
    local cyg_txz="$DL/$(basename "$relpath")"
    dl "$cyg_url" "$cyg_txz"
    rm -rf "$TMP/cyg" && mkdir -p "$TMP/cyg"
    tar -xJf "$cyg_txz" -C "$TMP/cyg" usr/bin/cygwin1.dll
    cp -f "$TMP/cyg/usr/bin/cygwin1.dll" "$bin/"
  fi

  echo "[win] write LoopTools.m wrapper..."
  cat > "$lt/LoopTools.m" <<EOF
BeginPackage["LoopTools`"];
LoopToolsInstall::usage="LoopToolsInstall[] installs the LoopTools WSTP executable.";
Begin["\`Private\`"];
LoopToolsInstall[] := Install[FileNameJoin[{DirectoryName[\$InputFileName],"Windows-x86-64","LoopTools.exe"}]];
End[];
EndPackage[];
EOF

  echo "[win] short test..."
  local exe_mma
  exe_mma="$(to_mma_path "$bin/LoopTools.exe")"
  "$MATH_BIN" -noprompt -run "TimeConstrained[(l=Install[\"$exe_mma\"]; Print[\"B0=\", N@B0[1000,50,80]]; Uninstall[l];),10,Print[\"FAIL\"]]; Quit[]"
  echo "[win] DONE -> $lt"
}

# ---------- Linux block ----------
install_linux() {
  need curl; need tar; need make; need gcc; need gfortran; need awk

  local apps
  apps="$("$MATH_BIN" -noprompt -run 'Print[FileNameJoin[{$UserBaseDirectory,"Applications"}]]; Quit[]' 2>/dev/null | tr -d '\r')"
  if [ -z "$apps" ]; then
    echo "ERROR: cannot get Applications dir from Mathematica"
    exit 1
  fi

  local lt="$apps/LoopTools"
  mkdir -p "$lt"

  echo "[linux] download..."
  dl "$SRC_URL" "$DL/LoopTools-$VER.tar.gz"

  echo "[linux] unpack..."
  rm -rf "$SRC/LoopTools-$VER"
  tar -xzf "$DL/LoopTools-$VER.tar.gz" -C "$SRC"

  echo "[linux] build+install..."
  local srcdir="$SRC/LoopTools-$VER"
  cd "$srcdir"

  # add WSTP CompilerAdditions to PATH (so mcc/mprep are found)
  local cadds
  cadds="$("$MATH_BIN" -noprompt -run 'Print[FileNameJoin[{$InstallationDirectory,"SystemFiles","Links","WSTP","DeveloperKit",$SystemID,"CompilerAdditions"}]]; Quit[]' 2>/dev/null | tr -d '\r')"
  export PATH="$cadds:$PATH"

  ./configure --prefix="$lt"
  make -j"$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 2)"
  make install

  echo "[linux] write LoopTools.m wrapper..."
  cat > "$lt/LoopTools.m" <<EOF
BeginPackage["LoopTools`"];
LoopToolsInstall::usage="LoopToolsInstall[] installs the LoopTools WSTP executable.";
Begin["\`Private\`"];
LoopToolsInstall[] := Install["$lt/bin/LoopTools"];
End[];
EndPackage[];
EOF

  echo "[linux] short test..."
  "$MATH_BIN" -noprompt -run "TimeConstrained[(l=Install[\"$lt/bin/LoopTools\"]; Print[\"B0=\", N@B0[1000,50,80]]; Uninstall[l];),10,Print[\"FAIL\"]]; Quit[]"
  echo "[linux] DONE -> $lt"
}

case "$OS" in
  Linux*)  install_linux ;;
  MINGW*|MSYS*|CYGWIN*) install_windows ;;
  *) echo "ERROR: unsupported OS '$OS'"; exit 1 ;;
esac
