#!/usr/bin/env bash
set -euo pipefail

log() {
  printf '[entrypoint] %s\n' "$*"
}

WORKER_USER="${WORKER_USER:-worker}"
WORKER_HOME="/home/${WORKER_USER}"
TWOHDM_ROOT="${TWOHDM_ROOT:-${WORKER_HOME}/2hdm}"

VNC_PASSWORD="${VNC_PASSWORD:-kds}"
VNC_GEOMETRY="${VNC_GEOMETRY:-1920x1080}"
VNC_DEPTH="${VNC_DEPTH:-24}"
VNC_PORT="${VNC_PORT:-5901}"
NOVNC_PORT="${NOVNC_PORT:-6080}"
WOLFRAM_VERSION="${WOLFRAM_VERSION:-13.1}"

export HOME="${WORKER_HOME}"
export USER="${WORKER_USER}"
export DISPLAY="${DISPLAY:-:1}"
export WOLFRAM_USERBASE="${WOLFRAM_USERBASE:-${WORKER_HOME}/.Mathematica}"
export MATHEMATICA_USERBASE="${MATHEMATICA_USERBASE:-${WORKER_HOME}/.Mathematica}"

activate_mathematica() {
  local kernel="/usr/local/bin/WolframKernel"
  local key="${WOLFRAM_ACTIVATION_KEY:-3893-9258-K6XJLE}"
  local sys_mathpass="/usr/share/Mathematica/Licensing/mathpass"
  local user_mathpass="${WORKER_HOME}/.Mathematica/Licensing/mathpass"

  mkdir -p /usr/share/Mathematica/Licensing "${WORKER_HOME}/.Mathematica/Licensing"

  if "${kernel}" -noprompt -run 'If[1+1===2, Quit[0], Quit[1]]; Quit[]' >/dev/null 2>&1; then
    log "Mathematica already activated"
    return 0
  fi

  log "activating Mathematica"

  local out machine_name mathid pass
  out="$(timeout 20s bash -lc 'printf "\n\n\n\n\n" | /usr/local/bin/WolframKernel' 2>&1 || true)"

  machine_name="$(printf '%s\n' "$out" | sed -n 's/.*Machine name:[[:space:]]*//p' | tail -n 1 | xargs)"
  mathid="$(printf '%s\n' "$out" | sed -n 's/.*MathID:[[:space:]]*//p' | tail -n 1 | xargs)"

  if [[ -z "${machine_name}" || -z "${mathid}" ]]; then
    log "cannot extract Wolfram MathID"
    printf '%s\n' "$out"
    return 1
  fi

  log "Machine name: ${machine_name}"
  log "MathID: ${mathid}"

  pass="$(python3 /opt/dist/activate-mathematica.py --mid "${mathid}" | tr -d '\r' | tail -n 1 | xargs)"

  if [[ -z "${pass}" ]]; then
    log "activation script returned empty password"
    return 1
  fi

  printf '%s\t%s\t%s\t%s\t\t\t\n' \
    "${machine_name}" \
    "${mathid}" \
    "${key}" \
    "${pass}" \
    > "${user_mathpass}"

  cp -f "${user_mathpass}" "${sys_mathpass}" 2>/dev/null || true
  chmod 0644 "${user_mathpass}" "${sys_mathpass}" 2>/dev/null || true

  if [[ "${EUID}" -eq 0 ]]; then
    chown -R "${WORKER_USER}:${WORKER_USER}" "${WORKER_HOME}/.Mathematica"
  fi

  "${kernel}" -noprompt -run 'If[1+1===2, Print["Mathematica OK"], Quit[1]]; Quit[]'
}

ensure_layout() {
  mkdir -p \
    "${TWOHDM_ROOT}" \
    "${WORKER_HOME}/Desktop" \
    "${WORKER_HOME}/.vnc" \
    "${WORKER_HOME}/.config" \
    "${WORKER_HOME}/.cache" \
    "${WORKER_HOME}/.local/share/applications" \
    "${WORKER_HOME}/.config/xfce4/xfconf/xfce-perchannel-xml" \
    "${WORKER_HOME}/.Mathematica/Applications" \
    "${WORKER_HOME}/.Mathematica/Licensing" \
    "${WORKER_HOME}/.WolframEngine/Applications" \
    /opt/src
}

run_mma() {
  /usr/local/bin/WolframKernel -noprompt -run "$1"
}

install_looptools() {
  local ver="${LOOPTOOLS_VERSION:-2.16}"
  local apps="${WORKER_HOME}/.Mathematica/Applications"
  local lt="${apps}/LoopTools"
  local src="/opt/src"
  local archive="/opt/dist/LoopTools-${ver}.tar.gz"
  local marker="${WORKER_HOME}/.Mathematica/.looptools-installed"

  [[ -f "${marker}" && "${FORCE_LOOPTOOLS_INSTALL:-0}" != "1" ]] && {
    log "LoopTools already installed; skip"
    return 0
  }

  log "installing LoopTools ${ver}"

  command -v tar >/dev/null
  command -v make >/dev/null
  command -v gcc >/dev/null
  command -v gfortran >/dev/null
  [[ -f "${archive}" ]] || { log "ERROR: missing ${archive}"; return 1; }

  run_mma 'If[1+1===2, Print["Mathematica OK"], Quit[1]]; Quit[]'

  rm -rf "${src}/LoopTools-${ver}" "${lt}"
  mkdir -p "${src}" "${apps}"

  tar -xzf "${archive}" -C "${src}"
  cd "${src}/LoopTools-${ver}"

  local cadds
  cadds="$(run_mma 'Print[FileNameJoin[{$InstallationDirectory,"SystemFiles","Links","WSTP","DeveloperKit",$SystemID,"CompilerAdditions"}]]; Quit[]' \
    2>/dev/null | tr -d '\r' | tail -n 1 | xargs)"

  [[ -d "${cadds}" ]] || { log "ERROR: WSTP CompilerAdditions not found: ${cadds}"; return 1; }
  log "WSTP CompilerAdditions: ${cadds}"

  export PATH="${cadds}:${PATH}"
  export FFLAGS="-O2 -fallow-argument-mismatch -std=legacy"
  export FCFLAGS="${FFLAGS}"

  ./configure --prefix="${lt}"
  make -j1
  make install

  [[ -x "${lt}/bin/LoopTools" ]] || { log "ERROR: LoopTools binary not found"; return 1; }

  cat > "${lt}/LoopTools.m" <<EOF
BeginPackage["LoopTools\`"];
LoopToolsInstall::usage="LoopToolsInstall[] installs the LoopTools WSTP executable.";
Begin["\`Private\`"];
LoopToolsInstall[] := Install["${lt}/bin/LoopTools"];
End[];
EndPackage[];
EOF

  cat > "${apps}/LoopTools.m" <<EOF
Get[FileNameJoin[{"${apps}", "LoopTools", "LoopTools.m"}]]
EOF

  cat > /tmp/test-looptools.m <<EOF
Get["${apps}/LoopTools.m"];

link = LoopTools\`LoopToolsInstall[];

b0ref = N[B0[0, 1, 1]];
b0demo = N[B0[1000, 50, 80]];

Print["LoopTools test:"];
Print["  expected: B0(0,1,1) ~= 0"];
Print["  obtained: B0(0,1,1) = ", b0ref];
Print["  abs diff = ", Abs[b0ref]];
Print["  diagnostic: B0(1000,50,80) = ", b0demo];

ok = NumericQ[b0ref] && Abs[b0ref] < 10^-8 && NumericQ[b0demo];

Uninstall[link];

If[ok, Print["LoopTools OK"], Print["LoopTools FAILED"]; Quit[1]];
Quit[]
EOF

  run_mma 'Get["/tmp/test-looptools.m"]'

  touch "${marker}"
  chown -R "${WORKER_USER}:${WORKER_USER}" "${WORKER_HOME}/.Mathematica" || true

  log "LoopTools installed successfully"
}

write_xfce_defaults() {
  cat > "${WORKER_HOME}/.config/xfce4/xfconf/xfce-perchannel-xml/xsettings.xml" <<'XFCE_XSETTINGS'
<?xml version="1.0" encoding="UTF-8"?>

<channel name="xsettings" version="1.0">
  <property name="Net" type="empty">
    <property name="ThemeName" type="string" value="Adwaita"/>
    <property name="IconThemeName" type="string" value="Adwaita"/>
  </property>
</channel>
XFCE_XSETTINGS
}

write_desktop_launchers() {
  local desktop_dir="${WORKER_HOME}/Desktop"
  local apps_dir="${WORKER_HOME}/.local/share/applications"
  local terminal_desktop="${desktop_dir}/Terminal.desktop"
  local mathematica_desktop="${desktop_dir}/Mathematica.desktop"
  local project_desktop="${desktop_dir}/2HDM Project.desktop"

  cat > "${terminal_desktop}" <<EOF_DESKTOP
[Desktop Entry]
Version=1.0
Type=Application
Name=Terminal
Comment=Open terminal in the 2HDM project directory
Exec=xfce4-terminal --working-directory=${TWOHDM_ROOT}
Icon=utilities-terminal
Terminal=false
Categories=System;TerminalEmulator;
EOF_DESKTOP

  cat > "${mathematica_desktop}" <<EOF_DESKTOP
[Desktop Entry]
Version=1.0
Type=Application
Name=Mathematica
Comment=Open Wolfram Mathematica
Exec=mathematica
Icon=accessories-calculator
Terminal=false
Categories=Science;Education;Development;
EOF_DESKTOP

  cat > "${project_desktop}" <<EOF_DESKTOP
[Desktop Entry]
Version=1.0
Type=Application
Name=2HDM Project
Comment=Open the mounted 2HDM project directory
Exec=thunar ${TWOHDM_ROOT}
Icon=folder
Terminal=false
Categories=Utility;FileManager;
EOF_DESKTOP

  chmod +x "${terminal_desktop}" "${mathematica_desktop}" "${project_desktop}"
  cp -f "${terminal_desktop}" "${apps_dir}/2hdm-terminal.desktop"
  cp -f "${mathematica_desktop}" "${apps_dir}/2hdm-mathematica.desktop"
  cp -f "${project_desktop}" "${apps_dir}/2hdm-project.desktop"

  if command -v gio >/dev/null 2>&1; then
    gio set "${terminal_desktop}" metadata::trusted true >/dev/null 2>&1 || true
    gio set "${mathematica_desktop}" metadata::trusted true >/dev/null 2>&1 || true
    gio set "${project_desktop}" metadata::trusted true >/dev/null 2>&1 || true
  fi
}

write_vnc_config() {
  if command -v vncpasswd >/dev/null 2>&1; then
    echo "${VNC_PASSWORD}" | vncpasswd -f > "${WORKER_HOME}/.vnc/passwd"
  elif command -v tigervncpasswd >/dev/null 2>&1; then
    echo "${VNC_PASSWORD}" | tigervncpasswd -f > "${WORKER_HOME}/.vnc/passwd"
  else
    echo "ERROR: neither vncpasswd nor tigervncpasswd found" >&2
    exit 1
  fi

  chmod 600 "${WORKER_HOME}/.vnc/passwd"

  cat > "${WORKER_HOME}/.vnc/xstartup" <<'VNCSTART'
#!/usr/bin/env bash
unset SESSION_MANAGER
unset DBUS_SESSION_BUS_ADDRESS

export XDG_RUNTIME_DIR="/tmp/runtime-${USER:-worker}"
mkdir -p "${XDG_RUNTIME_DIR}"
chmod 700 "${XDG_RUNTIME_DIR}"

if command -v dbus-launch >/dev/null 2>&1; then
  exec dbus-launch --exit-with-session startxfce4
else
  exec startxfce4
fi
VNCSTART

  chmod +x "${WORKER_HOME}/.vnc/xstartup"
}

start_vnc() {
  log "starting VNC on ${DISPLAY}"
  vncserver -kill "${DISPLAY}" >/dev/null 2>&1 || true
  vncserver "${DISPLAY}" -localhost no -geometry "${VNC_GEOMETRY}" -depth "${VNC_DEPTH}"
}

start_novnc() {
  log "starting noVNC on 0.0.0.0:${NOVNC_PORT}"
  exec websockify \
    --web=/usr/share/novnc/ \
    "0.0.0.0:${NOVNC_PORT}" \
    "localhost:${VNC_PORT}"
}

main() {
  if [[ "${1:-start}" != "start" ]]; then
    exec "$@"
  fi

  ensure_layout

  activate_mathematica || {
    log "WARNING: Mathematica activation failed; container will continue"
  }

  if [[ "${AUTO_INSTALL_LOOPTOOLS:-1}" == "1" ]]; then
    install_looptools || {
      log "WARNING: LoopTools install failed; container will continue"
    }
  fi

  write_xfce_defaults
  write_desktop_launchers
  write_vnc_config
  start_vnc

  log "desktop ready"
  log "open noVNC: http://127.0.0.1:${NOVNC_PORT}/vnc_lite.html"
  log "VNC password: ${VNC_PASSWORD}"
  log "inside XFCE terminal run: mathematica"

  start_novnc
}

main "$@"
