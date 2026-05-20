# Full Mathematica desktop workstation for 2HDM/ZZZ development.
# Build-time assets expected locally:
#   setup/wolfram/${WOLFRAM_INSTALLER}
#   setup/wolfram/${WOLFRAM_DOC_INSTALLER}  optional

# -----------------------------------------------------------------------------
# Stage 1: install Wolfram Mathematica Desktop without keeping installers
# -----------------------------------------------------------------------------
FROM ubuntu:22.04 AS mathematica_131

USER root

ARG WOLFRAM_VERSION=13.1
ARG WOLFRAM_INSTALLER
ARG WOLFRAM_DOC_INSTALLER=""

ENV DEBIAN_FRONTEND=noninteractive \
    WOLFRAM_INSTALL_DIR=/opt/Wolfram/Mathematica/${WOLFRAM_VERSION}

SHELL ["/bin/bash", "-lc"]

COPY setup/wolfram/ /tmp/wolfram/

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      bash ca-certificates coreutils findutils grep sed tar gzip xz-utils && \
    rm -rf /var/lib/apt/lists/*

RUN test -n "${WOLFRAM_INSTALLER}" && \
    test -f "/tmp/wolfram/${WOLFRAM_INSTALLER}" && \
    chmod +x "/tmp/wolfram/${WOLFRAM_INSTALLER}" && \
    "/tmp/wolfram/${WOLFRAM_INSTALLER}" \
      --nox11 \
      -- \
      -auto \
      -createdir=y \
      -overwrite=y \
      -targetdir="${WOLFRAM_INSTALL_DIR}" \
      -execdir=/usr/local/bin && \
    test -d "${WOLFRAM_INSTALL_DIR}" && \
    echo "Mathematica installed into ${WOLFRAM_INSTALL_DIR}"

RUN echo "=== Mathematica executables ===" && \
    find "${WOLFRAM_INSTALL_DIR}/Executables" -maxdepth 1 -type f -printf '%f\n' | sort && \
    if [[ -x "${WOLFRAM_INSTALL_DIR}/Executables/Mathematica" ]]; then \
      ln -sf "${WOLFRAM_INSTALL_DIR}/Executables/Mathematica" /usr/local/bin/Mathematica; \
      ln -sf "${WOLFRAM_INSTALL_DIR}/Executables/Mathematica" /usr/local/bin/mathematica; \
    fi && \
    if [[ -x "${WOLFRAM_INSTALL_DIR}/Executables/wolframscript" ]]; then \
      ln -sf "${WOLFRAM_INSTALL_DIR}/Executables/wolframscript" /usr/local/bin/wolframscript; \
    fi && \
    if [[ -x "${WOLFRAM_INSTALL_DIR}/Executables/WolframKernel" ]]; then \
      ln -sf "${WOLFRAM_INSTALL_DIR}/Executables/WolframKernel" /usr/local/bin/WolframKernel; \
    elif [[ -x "${WOLFRAM_INSTALL_DIR}/Executables/MathKernel" ]]; then \
      ln -sf "${WOLFRAM_INSTALL_DIR}/Executables/MathKernel" /usr/local/bin/WolframKernel; \
      ln -sf "${WOLFRAM_INSTALL_DIR}/Executables/MathKernel" /usr/local/bin/MathKernel; \
    fi && \
    if [[ -x "${WOLFRAM_INSTALL_DIR}/Executables/math" ]]; then \
      ln -sf "${WOLFRAM_INSTALL_DIR}/Executables/math" /usr/local/bin/math; \
    fi && \
    test -x /usr/local/bin/mathematica && \
    test -x /usr/local/bin/wolframscript && \
    test -x /usr/local/bin/WolframKernel

RUN if [[ -n "${WOLFRAM_DOC_INSTALLER}" ]]; then \
      test -f "/tmp/wolfram/${WOLFRAM_DOC_INSTALLER}" && \
      chmod +x "/tmp/wolfram/${WOLFRAM_DOC_INSTALLER}" && \
      "/tmp/wolfram/${WOLFRAM_DOC_INSTALLER}" \
        --nox11 \
        -- \
        -auto \
        -targetdir="${WOLFRAM_INSTALL_DIR}" \
        -execdir=/usr/local/bin; \
    else \
      echo "No WOLFRAM_DOC_INSTALLER specified; skipping Wolfram documentation"; \
    fi

RUN rm -rf /tmp/wolfram /tmp/wolfram-extract /var/tmp/*


# -----------------------------------------------------------------------------
# Stage 2: final workstation image
# -----------------------------------------------------------------------------
FROM mathematica_131 AS feyncalc_looptools

USER root

ARG USERNAME=worker
ARG USER_UID=1000
ARG USER_GID=1000
ARG TZ=Europe/Moscow
ARG WOLFRAM_VERSION=13.1
ARG LHAPDF_VERSION=6.5.6
ARG LHAPDF_SETS="NNPDF30_nnlo_as_0118"
ARG LHAPDF_SET="NNPDF30_nnlo_as_0118"

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    TZ=${TZ} \
    PYTHONUNBUFFERED=1 \
    PIP_NO_CACHE_DIR=1 \
    TWOHDM_ROOT=/home/${USERNAME}/2hdm \
    WOLFRAM_VERSION=${WOLFRAM_VERSION} \
    WOLFRAM_INSTALL_DIR=/opt/Wolfram/Mathematica/${WOLFRAM_VERSION} \
    WOLFRAM_USERBASE=/home/${USERNAME}/.Mathematica \
    MATHEMATICA_USERBASE=/home/${USERNAME}/.Mathematica \
    LHAPDF_PREFIX=/opt/lhapdf \
    LHAPDF_DATA_PATH=/opt/lhapdf/share/LHAPDF \
    LHAPDF_SET=${LHAPDF_SET} \
    DISPLAY=:1 \
    PATH=/usr/local/bin:/opt/lhapdf/bin:/home/${USERNAME}/.Mathematica/Applications/LoopTools/bin:${PATH} \
    LD_LIBRARY_PATH=/opt/lhapdf/lib

SHELL ["/bin/bash", "-lc"]

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      bash bash-completion ca-certificates curl wget git rsync tree \
      unzip zip xz-utils tar file less nano vim tmux htop procps lsof net-tools iputils-ping \
      locales tzdata sudo tini fzf ranger bat ripgrep fd-find \
      build-essential gcc g++ gfortran make cmake pkg-config autoconf automake libtool \
      python3 python3-pip python3-venv python3-dev \
      dbus-x11 xauth x11-apps xvfb xfce4 xfce4-terminal thunar \
      tigervnc-standalone-server tigervnc-tools novnc websockify \
      libx11-6 libxext6 libxrender1 libxt6 libsm6 libice6 libglu1-mesa libgl1 \
      libgtk-3-0 libasound2 libfontconfig1 libfreetype6 \
      libxrandr2 libxfixes3 libxcursor1 libxi6 libxinerama1 libxdamage1 \
      libxcomposite1 libxss1 libxtst6 libnss3 libgbm1 \
      libxkbcommon-x11-0 libxcb-xinerama0 libxcb-cursor0 libxcb-icccm4 \
      libxcb-image0 libxcb-keysyms1 libxcb-render-util0 libxcb-randr0 \
      libxcb-shape0 libxcb-sync1 libxcb-xfixes0 libxcb-xkb1 libxcb1 \
      libdbus-1-3 libglib2.0-bin \
      adwaita-icon-theme hicolor-icon-theme shared-mime-info desktop-file-utils \
      librsvg2-common libgtk-3-bin xdg-utils \
      fonts-dejavu fonts-dejavu-core fonts-freefont-ttf \
      xfonts-base xfonts-100dpi xfonts-75dpi && \
    ln -sf /usr/bin/python3 /usr/local/bin/python && \
    locale-gen C.UTF-8 && \
    (gtk-update-icon-cache -f -t /usr/share/icons/hicolor || true) && \
    (gtk-update-icon-cache -f -t /usr/share/icons/Adwaita || true) && \
    (update-mime-database /usr/share/mime || true) && \
    (update-desktop-database /usr/share/applications || true) && \
    rm -rf /var/lib/apt/lists/*

RUN if ! id -u "${USERNAME}" >/dev/null 2>&1; then \
      groupadd --gid "${USER_GID}" "${USERNAME}" && \
      useradd --uid "${USER_UID}" --gid "${USER_GID}" -m -s /bin/bash "${USERNAME}"; \
    fi && \
    usermod -aG sudo "${USERNAME}" && \
    echo "${USERNAME} ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/90-${USERNAME} && \
    chmod 0440 /etc/sudoers.d/90-${USERNAME}

RUN mkdir -p \
      /home/${USERNAME}/2hdm \
      /opt/dist /opt/src \
      /opt/lhapdf/share/LHAPDF \
      /usr/share/Mathematica/Licensing \
      /home/${USERNAME}/Desktop \
      /home/${USERNAME}/.vnc \
      /home/${USERNAME}/.config \
      /home/${USERNAME}/.cache \
      /home/${USERNAME}/.Mathematica/Applications \
      /home/${USERNAME}/.WolframEngine/Applications && \
    chown -R ${USERNAME}:${USERNAME} \
      /home/${USERNAME} \
      /opt/lhapdf \
      /usr/share/Mathematica/Licensing


COPY requirements.txt /tmp/requirements.txt

RUN python3 -m pip install --upgrade pip setuptools wheel && \
    grep -viE '^\s*lhapdf([<>=!~].*)?$' /tmp/requirements.txt | sed '/^\s*#/d;/^\s*$/d' > /tmp/requirements.docker.txt && \
    python3 -m pip install -r /tmp/requirements.docker.txt && \
    python3 -m pip install numpy pandas matplotlib python-telegram-bot


COPY setup/LHAPDF-${LHAPDF_VERSION}.tar.gz /opt/dist/LHAPDF-${LHAPDF_VERSION}.tar.gz

RUN tar -xzf /opt/dist/LHAPDF-${LHAPDF_VERSION}.tar.gz -C /opt/src && \
    cd /opt/src/LHAPDF-${LHAPDF_VERSION} && \
    ./configure --prefix=/opt/lhapdf --disable-static --with-python="$(command -v python3)" && \
    make -j"$(nproc)" && make install && ldconfig && \
    py_site="$(python3 -c 'import site; print(site.getsitepackages()[0])')" && \
    py_ver="$(python3 -c 'import sys; print(f"{sys.version_info.major}.{sys.version_info.minor}")')" && \
    printf '%s\n' "/opt/lhapdf/lib/python${py_ver}/site-packages" > "${py_site}/lhapdf-local.pth" && \
    for pdfset in ${LHAPDF_SETS}; do /opt/lhapdf/bin/lhapdf install "${pdfset}"; done && \
    rm -rf /opt/src/LHAPDF-${LHAPDF_VERSION}

# Download Mathematica package sources once via curl.
# FeynCalc installer below uses these local archives instead of downloading them from inside Mathematica.
RUN curl -L --fail https://feynarts.de/looptools/LoopTools-2.16.tar.gz \
      -o /opt/dist/LoopTools-2.16.tar.gz 

COPY setup/activate-mathematica.py /opt/dist/activate-mathematica.py

COPY docker/entrypoint.sh /usr/local/sbin/entrypoint.sh
COPY docker/setup-bashrc.sh /usr/local/bin/setup-bashrc.sh

RUN chmod +x \
      /opt/dist/activate-mathematica.py && \
     chown -R ${USERNAME}:${USERNAME} \
      /opt/src \
      /opt/dist \
      /home/${USERNAME} \
      /usr/share/Mathematica/Licensing
         
RUN chmod +x /usr/local/sbin/entrypoint.sh /usr/local/bin/setup-bashrc.sh && \
    /usr/local/bin/setup-bashrc.sh "${USERNAME}" && \
    ln -sfn /home/${USERNAME}/2hdm /2hdm && \
    chown -h ${USERNAME}:${USERNAME} /2hdm && \
    chown -R ${USERNAME}:${USERNAME} \
      /opt/lhapdf \
      /home/${USERNAME}

USER ${USERNAME}

WORKDIR /home/${USERNAME}/2hdm

EXPOSE 5901 6080

ENTRYPOINT ["/usr/bin/tini", "--", "/usr/local/sbin/entrypoint.sh"]
CMD ["start"]
