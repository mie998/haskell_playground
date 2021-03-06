# reference for this Dockerfile configuration: https://stackoverflow.com/questions/67680726/installing-haskells-cabal-or-ghcup-inside-a-dockerfile-wont-work
# Only Arm architecture
FROM debian:stable-slim as build

# Install dependencies *You don't need all of them
RUN apt-get update -y \
    && apt-get upgrade -y \
    && apt-get install -y git sudo jq bc make automake \
    && apt-get install -y rsync htop curl build-essential lsb-release \
    && apt-get install -y pkg-config libffi-dev libgmp-dev software-properties-common \
    && apt-get install -y libssl-dev libtinfo-dev libsystemd-dev libnuma-dev \
    && apt-get install -y zlib1g-dev make g++ wget libncursesw5 libtool autoconf \
    && apt-get clean

RUN wget https://apt.llvm.org/llvm.sh
RUN chmod +x llvm.sh
RUN sudo ./llvm.sh 12

# Install ghcup
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN bash -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"

# Add ghcup to PATH
ENV PATH=${PATH}:/root/.local/bin
ENV PATH=${PATH}:/root/.ghcup/bin

# Install cabal
RUN bash -c "ghcup upgrade"
RUN bash -c "ghcup install cabal 3.4.0.0"
RUN bash -c "ghcup set cabal 3.4.0.0"

# Install GHC
RUN bash -c "ghcup install ghc 8.10.7"
RUN bash -c "ghcup set ghc 8.10.7"

# Install haskell-language-server
RUN bash -c "ghcup install hls"

# Update Path to include Cabal and GHC exports
RUN bash -c "echo PATH="$HOME/.local/bin:$PATH" >> $HOME/.bashrc"
RUN bash -c "echo export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH" >> $HOME/.bashrc"
RUN bash -c "source $HOME/.bashrc"

# Update cabal
RUN bash -c "cabal update"

# setup private-key for git-ssh
RUN bash -c "mkdir -p /root/.ssh && ln -s /run/secrets/id_rsa /root/.ssh/id_rsa"
RUN bash -c "chown -R root:root /root/.ssh"
RUN bash -c "chmod -R 0600 /root/.ssh"
RUN bash -c "git config --global core.sshCommand \"ssh -i /root/.ssh/id_rsa\" /dev/null"
