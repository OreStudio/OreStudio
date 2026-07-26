FROM debian:bookworm-slim AS extract

WORKDIR /src
COPY external/nats/packages/nats-server-v2.14.3-linux-amd64.tar.gz .
RUN tar xzf nats-server-v2.14.3-linux-amd64.tar.gz \
    && mv nats-server-v2.14.3-linux-amd64/nats-server .

# Statically linked Go binary — scratch is sufficient, no libc needed.
FROM scratch

COPY --from=extract /src/nats-server /nats-server

ENTRYPOINT ["/nats-server"]
