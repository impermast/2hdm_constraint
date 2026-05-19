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
    "${WORKER_HOME}/.WolframEngine/Applications"
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
Exec=Thunar ${TWOHDM_ROOT}
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
