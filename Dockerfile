FROM prebuild1 as builder
COPY . /root/work
RUN cd /root/work && stack install

FROM debian:stable-slim
RUN mkdir -p /root/.local/bin && apt-get update && apt-get install curl -y
ENV PATH /root/.local/bin:$PATH
ENV LANG C.UTF-8
COPY --from=builder /root/.local/bin /root/.local/bin
VOLUME /savedata
RUN discord-unknown-spells-exe