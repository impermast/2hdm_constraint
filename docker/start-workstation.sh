#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

if [[ -f .env ]]; then
  set -a
  # shellcheck disable=SC1091
  . ./.env
  set +a
fi

docker compose up -d --no-build workstation

url="http://127.0.0.1:${NOVNC_PORT:-6080}/vnc_lite.html"

printf '\nnoVNC: '
printf '\033]8;;%s\033\\%s\033]8;;\033\\\n' "$url" "$url"
printf 'VNC password: %s\n\n' "${VNC_PASSWORD:-kds}"

if [[ "${OPEN_BROWSER:-1}" = "1" ]] && command -v xdg-open >/dev/null 2>&1; then
  xdg-open "$url" >/dev/null 2>&1 || true
fi
