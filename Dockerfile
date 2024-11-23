FROM haskell:9.6

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

WORKDIR /opt/app/src

COPY ./src/ .

ENTRYPOINT ["/bin/bash"]
