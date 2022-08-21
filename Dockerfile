FROM ubuntu
ENV DEBIAN_FRONTEND=noninteractive
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV BOOTSTRAP_HASKELL_GHC_VERSION=8.10.7
ENV BOOTSTRAP_HASKELL_INSTALL_STACK=1
RUN apt-get update && apt-get install -y curl\
    && curl -O https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb\
    && dpkg -i packages-microsoft-prod.deb\
    && rm packages-microsoft-prod.deb\
    && apt-get update\
    && apt-get install -y \
      git\
      build-essential\
      clang\
      lldb\
      lld\
      gfortran\
      python3\
      php\
      ruby\
      perl\
      nodejs\
      r-base\
      dotnet-sdk-6.0\
      default-jre\
      default-jdk\
      ocaml\
      scala\
      lua5.3\
      libgmp-dev\
    && curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/ashwanthkumar/install-golang/master/install.sh | sh -s \
    && curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y \
    && curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
COPY ./ /root/src
WORKDIR /root/src
RUN mv /root/src/.bashrc /root/.bashrc\
    && cp /root/.bashrc /root/.bashprofile\
    && chsh -s /bin/bash
RUN git clean -xdf\
    && . /root/.ghcup/env\
    && . /root/.cargo/env\
    && runghc ./bench.hs --no-md

CMD ["runghc", "./bench.hs", "--no-md"]
