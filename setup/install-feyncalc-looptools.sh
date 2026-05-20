#!/usr/bin/env bash
set -euo pipefail

if [[ "${EUID}" -ne 0 ]]; then
  exec sudo -E bash "$0" "$@"
fi

USERNAME="${USERNAME:-worker}"
VER="${VER:-2.16}"
KEY="${WOLFRAM_ACTIVATION_KEY:-3893-9258-K6XJLE}"

KERNEL="/usr/local/bin/WolframKernel"
MATHPASS="/usr/share/Mathematica/Licensing/mathpass"
APPS="/home/${USERNAME}/.Mathematica/Applications"
FC_DIR="${APPS}/FeynCalc"
LT_DIR="${APPS}/LoopTools"
SRC="/opt/src"

test -x "${KERNEL}"

MARKER="/home/${USERNAME}/.Mathematica/.feyncalc-looptools-installed"

if [[ -f "${MARKER}" && "${FORCE_MATH_STACK_INSTALL:-0}" != "1" ]]; then
  echo "=== FeynCalc + LoopTools already installed; skip ==="
  exit 0
fi

echo "=== Activate Mathematica ==="
OUT="$(printf '\n\n\n\n\n' | "${KERNEL}" 2>&1 || true)"
MACHINE_NAME="$(printf '%s\n' "$OUT" | sed -n 's/.*Machine name:[[:space:]]*//p' | tail -n 1 | xargs)"
MATHID="$(printf '%s\n' "$OUT" | sed -n 's/.*MathID:[[:space:]]*//p' | tail -n 1 | xargs)"

test -n "${MACHINE_NAME}"
test -n "${MATHID}"

echo "MACHINE_NAME=${MACHINE_NAME}"
echo "MATHID=${MATHID}"

PASS="$(python3 /opt/dist/activate-mathematica.py --mid "${MATHID}" | tr -d '\r' | tail -n 1 | xargs)"
test -n "${PASS}"

mkdir -p \
  /usr/share/Mathematica/Licensing \
  /root/.Mathematica/Licensing \
  "/home/${USERNAME}/.Mathematica/Licensing" \
  "${APPS}" \
  "${SRC}"

printf '%s\t%s\t%s\t%s\t\t\t\n' \
  "${MACHINE_NAME}" \
  "${MATHID}" \
  "${KEY}" \
  "${PASS}" \
  > "${MATHPASS}"

cp -f "${MATHPASS}" /root/.Mathematica/Licensing/mathpass
cp -f "${MATHPASS}" "/home/${USERNAME}/.Mathematica/Licensing/mathpass"
chmod 0644 "${MATHPASS}" /root/.Mathematica/Licensing/mathpass "/home/${USERNAME}/.Mathematica/Licensing/mathpass"

"${KERNEL}" -pwfile "${MATHPASS}" -noprompt -run 'If[1+1===2, Print["Mathematica OK"], Quit[1]]; Quit[]'

echo "=== Install FeynCalc + FeynArts ==="
export FC_DIR

cat > /tmp/install-feyncalc.m <<'EOF'
Get["/opt/dist/feyncalc-install.m"];

FeynCalcInstaller`$PathToFCArc = "/opt/dist/FeynCalc-hotfix-stable.zip";
FeynCalcInstaller`$PathToFAArc = "/opt/dist/FeynArts-master.zip";

FeynCalcInstaller`InstallFeynCalc[
  FeynCalcInstaller`InstallFeynCalcTo -> Environment["FC_DIR"],
  FeynCalcInstaller`AutoOverwriteFeynCalcDirectory -> True,
  FeynCalcInstaller`AutoInstallFeynArts -> True,
  FeynCalcInstaller`AutoEnableTraditionalForm -> False,
  FeynCalcInstaller`AutoDisableInsufficientVersionWarning -> True
];

Quit[]
EOF

"${KERNEL}" -pwfile "${MATHPASS}" -noprompt -run 'Get["/tmp/install-feyncalc.m"]'

echo "=== Install LoopTools ==="
rm -rf "${SRC}/LoopTools-${VER}"
tar -xzf "/opt/dist/LoopTools-${VER}.tar.gz" -C "${SRC}"
cd "${SRC}/LoopTools-${VER}"

CADDS="$("${KERNEL}" -pwfile "${MATHPASS}" -noprompt -run \
  'Print[FileNameJoin[{$InstallationDirectory,"SystemFiles","Links","WSTP","DeveloperKit",$SystemID,"CompilerAdditions"}]]; Quit[]' \
  2>/dev/null | tr -d '\r' | tail -n 1 | xargs)"

test -d "${CADDS}"

export PATH="${CADDS}:${PATH}"
export FFLAGS="-O2 -fallow-argument-mismatch -std=legacy"
export FCFLAGS="${FFLAGS}"

mkdir -p "${LT_DIR}"
./configure --prefix="${LT_DIR}"
make -j1
make install

cat > "${LT_DIR}/LoopTools.m" <<EOF
BeginPackage["LoopTools\`"];
LoopToolsInstall::usage="LoopToolsInstall[] installs the LoopTools WSTP executable.";
Begin["\`Private\`"];
LoopToolsInstall[] := Install["${LT_DIR}/bin/LoopTools"];
End[];
EndPackage[];
EOF

cat > "${APPS}/LoopTools.m" <<EOF
Get[FileNameJoin[{"${APPS}", "LoopTools", "LoopTools.m"}]]
EOF

chown -R "${USERNAME}:${USERNAME}" "/home/${USERNAME}/.Mathematica"

echo "=== Final test ==="
cat > /tmp/test-fc-lt.m <<EOF
Needs["FeynCalc\`"];
Print["FeynCalc OK"];

Get["${APPS}/LoopTools.m"];
l = LoopTools\`LoopToolsInstall[];
Print["LoopTools OK"];
Uninstall[l];

Quit[]
EOF

"${KERNEL}" -pwfile "${MATHPASS}" -noprompt -run 'Get["/tmp/test-fc-lt.m"]'

echo "=== Done ==="
touch "${MARKER}"
chown "${USERNAME}:${USERNAME}" "${MARKER}"
