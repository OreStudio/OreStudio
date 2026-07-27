FROM debian:bookworm-slim AS strip

RUN apt-get update && apt-get install -y --no-install-recommends binutils \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /src
# Populated by docker/stage-runtime.sh — only the ores.*.service binaries
# and their actual shared-library dependency closure, not the full publish/
# tree (which also carries Qt libs and test binaries).
COPY build/docker-stage/bin/ /src/bin/
COPY build/docker-stage/lib/ /src/lib/
RUN strip --strip-debug /src/bin/ores.*.service /src/lib/libores.*.so*

FROM cgr.dev/chainguard/glibc-dynamic:latest

WORKDIR /app

COPY --from=strip /src/bin/ /app/bin/
COPY --from=strip /src/lib/ /app/lib/

ENV LD_LIBRARY_PATH=/app/lib
WORKDIR /app/bin

ENTRYPOINT ["./ores.controller.service"]
