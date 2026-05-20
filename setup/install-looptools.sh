#!/usr/bin/env bash
set -euo pipefail

PREFIX="/home/${USERNAME}/.Mathematica/Applications/LoopTools"
ARCHIVE="/opt/dist/LoopTools-2.16.tar.gz"

echo "=== Mathematica smoke test ==="
wolframscript -code 'If[1 + 1 === 2, Print["Mathematica OK: 1+1=2"], Quit[1]]'

echo "=== Locate WSTP CompilerAdditions ==="
WSTP_DIR="$(
  find "${WOLFRAM_INSTALL_DIR}/SystemFiles/Links/WSTP/DeveloperKit" \
    -type d -name CompilerAdditions 2>/dev/null | head -n 1
)"

test -n "${WSTP_DIR}"
echo "WSTP_DIR=${WSTP_DIR}"

export PATH="${WSTP_DIR}:${PATH}"

# Needed for old Fortran code with modern gfortran.
export FFLAGS="-O2 -fallow-argument-mismatch -std=legacy"
export FCFLAGS="${FFLAGS}"

echo "=== Build LoopTools ==="
BUILD_DIR="$(mktemp -d)"
trap 'rm -rf "${BUILD_DIR}"' EXIT

tar -xzf "${ARCHIVE}" -C "${BUILD_DIR}"

SRC_DIR="$(
  find "${BUILD_DIR}" -maxdepth 1 -type d -name 'LoopTools-*' | head -n 1
)"

test -n "${SRC_DIR}"
echo "SRC_DIR=${SRC_DIR}"

mkdir -p "${PREFIX}"

cd "${SRC_DIR}"

./configure --prefix="${PREFIX}"
make -j1
make install

test -x "${PREFIX}/bin/LoopTools"

echo "=== Create Mathematica loader files ==="
printf '%s\n' \
  'Install[FileNameJoin[{DirectoryName[$InputFileName], "bin", "LoopTools"}]]' \
  > "${PREFIX}/LoopTools.m"

printf '%s\n' \
  'Get[FileNameJoin[{DirectoryName[$InputFileName], "LoopTools", "LoopTools.m"}]]' \
  > "/home/${USERNAME}/.Mathematica/Applications/LoopTools.m"

chown -R "${USERNAME}:${USERNAME}" \
  "${PREFIX}" \
  "/home/${USERNAME}/.Mathematica/Applications/LoopTools.m"

echo "=== LoopTools installed successfully ==="
