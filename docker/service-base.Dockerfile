# Base layer shared by every per-service image.
#
# Built *once*, transferred *once*.  Contains:
#   - Common shared libraries (the ldd intersection across all services)
#   - The healthcheck probe
#   - Empty /app/log, /app/run, /app/storage (placeholders — the runtime
#     bind-mounts host dirs over log and storage; see remote-run.sh)
#
# Per-service overlays (service-runtime.Dockerfile) add a single binary
# (and any service-specific extra libs) on top of this image.
#
# The build context expects build/docker-stage/base/ to have been
# populated by library_partition.stage_for_build(), and docker-stage
# must already have its binaries stripped by stage-runtime.sh.

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

# Base staging directory — pre-stripped common libs and empty log/run/storage.
COPY build/docker-stage/base/lib/ /app/lib/
# Empty log/run/storage dirs.  NOTE: buildah normalizes directory modes on
# COPY (a 0777 source lands 0755 root:root, and --chmod/--chown are
# ignored for directory modes), so these in-image dirs are NOT writable by
# the runtime uid — they are placeholders only.  The service runner
# bind-mounts host-owned dirs over /app/log and /app/storage (see
# docker/remote-run.sh); /app/run is unused today.
COPY build/docker-stage/base/log/ /app/log/
COPY build/docker-stage/base/run/ /app/run/
COPY build/docker-stage/base/storage/ /app/storage/

# Static healthcheck binary
COPY --from=healthcheck-build /healthcheck /app/bin/healthcheck

ENV LD_LIBRARY_PATH=/app/lib
WORKDIR /app/bin

HEALTHCHECK --interval=2s --timeout=2s --start-period=60s --retries=3 \
    CMD ["./healthcheck"]

# No ENTRYPOINT here — the per-service overlay sets it.
