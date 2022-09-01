FROM debian:stable-slim AS builder
ENV DEBIAN_FRONTEND=noninteractive
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV BOOTSTRAP_HASKELL_GHC_VERSION=8.10.7
ENV BOOTSTRAP_HASKELL_INSTALL_STACK=1
RUN apt-get update && apt-get install -y curl\
    && curl -O https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb\
    && dpkg -i packages-microsoft-prod.deb\
    && rm packages-microsoft-prod.deb\
    && apt-get update\
    && apt-get install -y  --no-install-recommends\
      git\
      build-essential\
      clang\
      lldb\
      lld\
      gfortran\
      dotnet-sdk-6.0\
      default-jre\
      default-jdk\
      ocaml\
      scala\
      libgmp-dev\
    && apt-get clean\
    && curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/ashwanthkumar/install-golang/master/install.sh | sh -s \
    && curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y \
    && curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
COPY ./ /root/src
WORKDIR /root/src
RUN cp /root/src/.bashrc /root/.bashrc\
    && cp /root/.bashrc /root/.bashprofile\
    && chsh -s /bin/bash
ENV LANG C.UTF-8
RUN git init\
    && git clean -Xdf\
    && . /root/.ghcup/env\
    && . /root/.cargo/env\
    && ghc bench.hs\
    && ./bench --no-md\
    && rm .gitignore\
    && mv .gitignore_bin .gitignore\
    && git clean -Xdf\
    && rm -rf .git

FROM debian:stable-slim AS bench
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update\
    && apt-get install -y  --no-install-recommends\
      git\
      python3\
      php\
      ruby\
      perl\
      nodejs\
      r-base\
      default-jre\
      lua5.3\
      scala\
    && apt-get clean
COPY --from=builder /root/src /root/src
WORKDIR /root/src
RUN chsh -s /bin/bash
ENV LANG C.UTF-8
CMD ["./bench", "--no-md", "--no-build"]
