# Docker Mathematica workstation

## First start

Put the Wolfram installers into:

```bash
setup/wolfram/
```

Create `.env` if it is missing:

```bash
cp .env.example .env
```

Build the image:

```bash
docker compose build workstation
```

Start the workstation and open noVNC:

```bash
./scripts/start-workstation.sh
```

The script opens:

```text
http://127.0.0.1:6080/vnc_lite.html
```

Default VNC password:

```text
kds
```

Inside XFCE, use the desktop launchers:

- `Terminal`
- `Mathematica`
- `2HDM Project`

## Manual start

```bash
docker compose up -d workstation
```

Then open:

```text
http://127.0.0.1:6080/vnc_lite.html
```

## Notes

The compose file binds VNC and noVNC to `127.0.0.1` only. This keeps the password-protected desktop local to the host.

The image includes Qt/XCB runtime libraries needed by Mathematica GUI, XFCE icon themes, desktop launchers, and fixed DBus startup for XFCE under Docker.
