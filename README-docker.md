# Docker environment for `2hdm_constraint`

This document describes the Docker workstation used to run the `2hdm_constraint` project with a full graphical Wolfram Mathematica installation.

The Docker setup is intentionally separated from the physics README. The physics code and project structure are described in `README.md`.

## Goal

The container provides a persistent Linux desktop environment with:

- Ubuntu base image;
- XFCE desktop;
- VNC/noVNC browser access;
- full Wolfram Mathematica GUI;
- `wolframscript`, `WolframKernel`, and `mathematica` commands;
- FeynCalc;
- FeynArts;
- LoopTools;
- LHAPDF;
- Python dependencies from `requirements.txt`;
- a mounted project directory at `/home/worker/2hdm`.

This is not a batch runner. It is a persistent scientific workstation container.

## Local directory layout

Expected local layout:

```text
2hdm_constraint/
├── Dockerfile
├── docker-compose.yml
├── .env
├── .env.example
├── .dockerignore
├── README.md
├── README-docker.md
├── requirements.txt
│
├── docker/
│   ├── entrypoint.sh
│   └── setup-bashrc.sh
│
├── setup/
│   ├── wolfram/
│   │   ├── .gitkeep
│   │   ├── Mathematica_13.1.0_LINUX.sh
│   │   └── WLDocs_13.1.0_LINUX.sh
│   │
│   ├── licensing/
│   │   ├── .gitkeep
│   │   └── mathpass
│   │
│   └── models/
│       ├── FeynArts/
│       └── FeynRules/
│
└── 2hdm/
    ├── ZZZ_ZWW/
    ├── articles/
    ├── modules/
    ├── ppHgg/
    ├── setup/
    ├── tg/
    └── requirements.txt
```

## Files that should not be committed

Do not commit Wolfram installers or license files.

Recommended `.gitignore` entries:

```gitignore
# Wolfram installers and documentation
setup/wolfram/*
!setup/wolfram/.gitkeep
!setup/wolfram/README.md

# Wolfram license
setup/licensing/*
!setup/licensing/.gitkeep
!setup/licensing/README.md

# Persistent/local Wolfram user data, if used later
setup/wolfram-userbase/*
!setup/wolfram-userbase/.gitkeep
!setup/wolfram-userbase/README.md

# Local Python envs
2hdm/.venv/
```

The large local files are expected to remain on the machine:

```text
setup/wolfram/Mathematica_13.1.0_LINUX.sh
setup/wolfram/WLDocs_13.1.0_LINUX.sh
setup/licensing/mathpass
```

## Important `.dockerignore` note

The Docker build must see the Wolfram installers, but it should not send the project data or license into the build context.

Recommended `.dockerignore`:

```dockerignore
.git
.vscode

__pycache__
**/__pycache__
*.pyc
*.pyo

# Runtime/project data are mounted, not copied during build.
2hdm/
2hdm/.venv/

# Do not send local license/state to Docker build context.
setup/licensing/
setup/wolfram-userbase/
.state/

# Common runtime artifacts.
*.mx
*.csv
*.log
*.tmp

# Do not ignore setup/wolfram/, because Dockerfile needs the installers.
```

## `.env`

Create `.env` from `.env.example`:

```bash
cp .env.example .env
```

Typical content:

```env
TZ=Europe/Moscow

WORKER_USER=worker
WORKER_UID=1000
WORKER_GID=1000

VNC_PORT=5901
NOVNC_PORT=6080
VNC_PASSWORD=kds
VNC_GEOMETRY=1920x1080
VNC_DEPTH=24

WOLFRAM_VERSION=13.1
WOLFRAM_INSTALLER=Mathematica_13.1.0_LINUX.sh
WOLFRAM_DOC_INSTALLER=WLDocs_13.1.0_LINUX.sh

LHAPDF_VERSION=6.5.6
LHAPDF_SETS=NNPDF30_nnlo_as_0118
LHAPDF_SET=NNPDF30_nnlo_as_0118
```

## Build

From the repository root:

```bash
docker compose config
docker compose down --remove-orphans
docker compose build
```

The build can take a long time because it installs Mathematica, optional Wolfram documentation, LHAPDF, FeynCalc/FeynArts, and LoopTools.

Do not use `--no-cache` unless you intentionally want a full rebuild from scratch.

## Run

Start the workstation:

```bash
docker compose up -d --force-recreate
```

Check status:

```bash
docker compose ps
docker compose logs -f workstation
```

## Access the GUI

Use noVNC in the browser:

```text
http://localhost:6080
```

If noVNC asks for a password, use `VNC_PASSWORD` from `.env`.

Inside the XFCE desktop:

1. Open a terminal.
2. Go to the project.
3. Start Mathematica:
   ```bash
   mathematica
   ```

## VNC versus noVNC

Two ports are used:

```text
5901 -> raw VNC protocol
6080 -> noVNC browser interface
```

Use `6080` in a browser.

Do not open `http://localhost:5901` in a browser. Port `5901` is not HTTP; it is for VNC clients such as Remmina or TigerVNC Viewer.

## Container paths

Inside the container:

```text
/home/worker/2hdm                         project volume
/opt/Wolfram/Mathematica/13.1             Mathematica installation
/opt/wolfram/userbase/Applications        FeynCalc/FeynArts/LoopTools layer
/opt/lhapdf                               LHAPDF installation
/usr/share/Mathematica/Licensing          mounted license directory
```

The local project directory is mounted as:

```text
./2hdm -> /home/worker/2hdm
```

The local license directory is mounted as:

```text
./setup/licensing -> /usr/share/Mathematica/Licensing
```

## License check

The file

```text
setup/licensing/mathpass
```

must be visible inside the container as

```text
/usr/share/Mathematica/Licensing/mathpass
```

Check Wolfram from inside the container:

```bash
docker exec -it 2hdm-mathematica-desktop bash
wolframscript -code 'Print[1+1]'
```

Expected output:

```text
2
Null
```

If the command hangs or asks for activation, the `mathpass` is missing, invalid, or not visible at the expected path.

## FeynCalc/FeynArts/LoopTools check

Inside the container:

```bash
wolframscript -code 'Print[$UserBaseDirectory]; Needs["FeynCalc`"]; Print["FeynCalc OK"]'
```

Check LoopTools executable:

```bash
ls -lah /opt/wolfram/userbase/Applications/LoopTools/bin/LoopTools
```

A basic Wolfram-side check can be done from Mathematica after loading your `modules/setup.m`.

## Common problems

### Docker cannot find the Wolfram installer

Symptom:

```text
test -f /opt/dist/wolfram/... failed
```

Check:

```bash
ls setup/wolfram/Mathematica_13.1.0_LINUX.sh
```

Also check `.dockerignore`. It must not exclude `setup/wolfram/`.

### License is not visible

Check on host:

```bash
ls setup/licensing/mathpass
```

Check inside container:

```bash
ls -lah /usr/share/Mathematica/Licensing
wolframscript -code 'Print[$PasswordFile]'
```

### Browser shows garbage on port 5901

This is expected if you open raw VNC in a browser.

Use:

```text
http://localhost:6080
```

not:

```text
http://localhost:5901
```

### noVNC does not open

Check ports:

```bash
docker exec -it 2hdm-mathematica-desktop bash -lc "ss -lntp | grep -E '5901|6080' || true"
docker compose logs --tail=100 workstation
```

### Mathematica GUI does not start

Inside the container:

```bash
which mathematica
which WolframKernel
which wolframscript
```

If `mathematica` is missing, the Mathematica installer did not complete successfully or did not place launch scripts into `/usr/local/bin`.

### LHAPDF Python import fails

Check:

```bash
python3 -c "import lhapdf; print(lhapdf.version())"
```

If this fails, verify that `/opt/lhapdf/lib/pythonX.Y/site-packages` is visible through the `.pth` file created during build.

## Stop and restart

Stop:

```bash
docker compose down
```

Restart:

```bash
docker compose up -d
```

Rebuild after Dockerfile changes:

```bash
docker compose down
docker compose build
docker compose up -d --force-recreate
```

## What this Docker setup intentionally does not include

- JupyterLab;
- SSH server;
- Wolfram Engine base image;
- automatic online license activation;
- runtime package installation on every container start.

The intended workflow is full Mathematica GUI through XFCE/noVNC, with physics packages installed into the image during build.
