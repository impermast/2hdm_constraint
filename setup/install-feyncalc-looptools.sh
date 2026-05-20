#!/usr/bin/env bash
set -euo pipefail

VER="${VER:-2.16}"

MATH_BIN="$(
  command -v math 2>/dev/null || \
  command -v WolframKernel 2>/dev/null || \
  command -v MathKernel 2>/dev/null || \
  true
)"

if [[ -z "${MATH_BIN}" ]]; then
  echo "ERROR: Mathematica kernel not found in PATH"
  echo "PATH=${PATH}"
  ls -lah /usr/local/bin | grep -E 'math|Wolfram|Kernel' || true
  exit 1
fi

echo "Mathematica kernel: ${MATH_BIN}"

echo "=== Mathematica activation check ==="
"${MATH_BIN}" -noprompt -run 'If[1 + 1 === 2, Print["Mathematica OK: 1+1=2"], Quit[1]]; Quit[]'

echo "=== Install FeynCalc + FeynArts from local archives ==="
"${MATH_BIN}" -noprompt -run '
  Get["/opt/dist/feyncalc-install.m"];

  FeynCalcInstaller`$PathToFCArc = "/opt/dist/FeynCalc-hotfix-stable.zip";
  FeynCalcInstaller`$PathToFAArc = "/opt/dist/FeynArts-master.zip";

  FeynCalcInstaller`InstallFeynCalc[
    FeynCalcInstaller`AutoOverwriteFeynCalcDirectory -> True,
    FeynCalcInstaller`AutoInstallFeynArts -> True,
    FeynCalcInstaller`AutoEnableTraditionalForm -> False,
    FeynCalcInstaller`AutoDisableInsufficientVersionWarning -> True
  ];

  Quit[]
'

echo "=== Install LoopTools ==="

need() {
  command -v "$1" >/dev/null 2>&1 || {
    echo "ERROR: need '$1'"
    exit 1
  }
}

need tar
need make
need gcc
need gfortran

APPS="$("${MATH_BIN}" -noprompt -run 'Print[FileNameJoin[{$UserBaseDirectory, "Applications"}]]; Quit[]' 2>/dev/null | tr -d '\r')"

if [[ -z "${APPS}" ]]; then
  echo "ERROR: cannot get Mathematica Applications dir"
  exit 1
fi

LT="${APPS}/LoopTools"
SRC="/opt/src"

mkdir -p "${LT}" "${SRC}"

rm -rf "${SRC}/LoopTools-${VER}"
tar -xzf "/opt/dist/LoopTools-${VER}.tar.gz" -C "${SRC}"

SRCDIR="${SRC}/LoopTools-${VER}"

if [[ ! -d "${SRCDIR}" ]]; then
  echo "ERROR: LoopTools source directory not found: ${SRCDIR}"
  exit 1
fi

cd "${SRCDIR}"

CADDS="$("${MATH_BIN}" -noprompt -run 'Print[FileNameJoin[{$InstallationDirectory, "SystemFiles", "Links", "WSTP", "DeveloperKit", $SystemID, "CompilerAdditions"}]]; Quit[]' 2>/dev/null | tr -d '\r')"

if [[ -z "${CADDS}" || ! -d "${CADDS}" ]]; then
  echo "ERROR: WSTP CompilerAdditions not found: ${CADDS}"
  exit 1
fi

echo "WSTP CompilerAdditions: ${CADDS}"

export PATH="${CADDS}:${PATH}"
export FFLAGS="-O2 -fallow-argument-mismatch -std=legacy"
export FCFLAGS="${FFLAGS}"

./configure --prefix="${LT}"
make -j1
make install

if [[ ! -x "${LT}/bin/LoopTools" ]]; then
  echo "ERROR: LoopTools executable not found: ${LT}/bin/LoopTools"
  exit 1
fi

echo "=== Write LoopTools wrapper ==="

cat > "${LT}/LoopTools.m" <<EOF
BeginPackage["LoopTools`"];
LoopToolsInstall::usage="LoopToolsInstall[] installs the LoopTools WSTP executable.";
Begin["\`Private\`"];
LoopToolsInstall[] := Install["${LT}/bin/LoopTools"];
End[];
EndPackage[];
EOF

cat > "${APPS}/LoopTools.m" <<EOF
Get[FileNameJoin[{\$UserBaseDirectory, "Applications", "LoopTools", "LoopTools.m"}]]
EOF

chown -R "${USERNAME}:${USERNAME}" "${APPS}/FeynCalc" "${LT}" "${APPS}/LoopTools.m"

echo "=== Final Mathematica package test ==="
"${MATH_BIN}" -noprompt -run '
  Needs["FeynCalc`"];
  Print["FeynCalc OK"];

  Get[FileNameJoin[{$UserBaseDirectory, "Applications", "LoopTools", "LoopTools.m"}]];
  l = LoopTools`LoopToolsInstall[];
  Print["LoopTools OK"];
  Uninstall[l];

  Quit[]
'

echo "=== FeynCalc + LoopTools installed successfully ==="
