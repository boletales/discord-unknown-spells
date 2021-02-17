FROM debian:stable-slim as prebuild1
RUN apt-get update && apt-get install -y curl
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN mkdir -p /root/work
COPY . /root/work
RUN cd /root/work && stack install --resolver lts-17.2 Cabal Only discord-haskell text mtl aeson transformers safe bytestring cryptonite unliftio memory time containers double-conversion either directory