# Base layer of the image hierarchy. Built once, transferred once.
#
# Carries the runtime skeleton every service shares:
#   - The healthcheck probe
#   - Empty /app/log, /app/run, /app/storage (placeholders — the runtime
#     bind-mounts host dirs over log and storage; see remote-run.sh)
#   - The library path the shared-library image and every service image
#     inherit
#
# The shared libraries live in service-libs.Dockerfile, and each service
# image (service-runtime.Dockerfile) adds one binary on top of it.
#
# The build context expects build/docker-stage/ to have been
# populated by stage-runtime.sh, and docker-stage must already have its
# binaries stripped by that script.

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

# Base staging directory — empty log/run/storage.
# NOTE: buildah normalizes directory modes on
# COPY (a 0777 source lands 0755 root:root, and --chmod/--chown are
# ignored for directory modes), so these in-image dirs are NOT writable by
# the runtime uid — they are placeholders only.  The service runner
# bind-mounts host-owned dirs over /app/log and /app/storage (see
# docker/remote-run.sh); /app/run is unused today.
COPY build/docker-stage/log/ /app/log/
COPY build/docker-stage/run/ /app/run/
COPY build/docker-stage/storage/ /app/storage/

# Static healthcheck binary
COPY --from=healthcheck-build /healthcheck /app/bin/healthcheck

ENV LD_LIBRARY_PATH=/app/lib
WORKDIR /app/bin

HEALTHCHECK --interval=2s --timeout=2s --start-period=60s --retries=3 \
    CMD ["./healthcheck"]

# No ENTRYPOINT here — the per-service overlay sets it.
