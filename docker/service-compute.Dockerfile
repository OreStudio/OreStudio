# Self-contained compute-wrapper image (no base/overlay — a single
# image, not part of the N-services duplication problem).
#
# Uses pre-stripped binaries from build/docker-stage (stage-runtime.sh
# now strips on the host), so there is no in-container strip stage.
#
# Built with:
#   podman build --format docker \
#     --build-arg SERVICE_NAME=ores.compute.wrapper \
#     -t localhost/ores-compute-wrapper:local \
#     -t localhost/ores-compute-wrapper:<tag> \
#     -f docker/service-compute.Dockerfile .

# Stage 1 — static healthcheck probe (no shell in final image)
FROM debian:bookworm-slim AS healthcheck-build
RUN apt-get update && apt-get install -y --no-install-recommends gcc libc6-dev \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /src
COPY docker/healthcheck.c .
RUN gcc -O2 -static -o /healthcheck healthcheck.c

# Stage 2 — final runtime image
FROM cgr.dev/chainguard/glibc-dynamic:latest

WORKDIR /app

# Pre-stripped binaries and libraries (stripped by stage-runtime.sh on host).
COPY build/docker-stage/bin/ /app/bin/
COPY build/docker-stage/lib/ /app/lib/

# Runtime directories (pre-created by stage-runtime.sh).
COPY build/docker-stage/log/ /app/log/
COPY build/docker-stage/run/ /app/run/
COPY build/docker-stage/storage/ /app/storage/

# Static healthcheck binary
COPY --from=healthcheck-build /healthcheck /app/bin/healthcheck

# Build context already has an entrypoint -> ./<SERVICE_NAME> symlink
# pre-created by stage-runtime.sh in build/docker-stage/bin/.

ENV LD_LIBRARY_PATH=/app/lib
WORKDIR /app/bin

HEALTHCHECK --interval=2s --timeout=2s --start-period=60s --retries=3 \
    CMD ["./healthcheck"]

ENTRYPOINT ["./entrypoint"]
