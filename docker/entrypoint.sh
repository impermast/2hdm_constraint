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

export HOME="${WORKER_HOME}"
export USER="${WORKER_USER}"
export DISPLAY="${DISPLAY:-:1}"
export WOLFRAM_USERBASE="${WOLFRAM_USERBASE:-${WORKER_HOME}/.Mathematica}"
export MATHEMATICA_USERBASE="${MATHEMATICA_USERBASE:-${WORKER_HOME}/.Mathematica}"

ensure_layout() {
  mkdir -p \
    "${TWOHDM_ROOT}" \
    "${WORKER_HOME}/.vnc" \
    "${WORKER_HOME}/.config" \
    "${WORKER_HOME}/.cache" \
    "${WORKER_HOME}/.Mathematica/Applications" \
    "${WORKER_HOME}/.WolframEngine/Applications"
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
exec startxfce4
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
  write_vnc_config
  start_vnc

  log "desktop ready"
  log "open noVNC: http://localhost:${NOVNC_PORT}"
  log "inside XFCE terminal run: mathematica"

  start_novnc
}

main "$@"
