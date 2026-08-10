# Per-service overlay — built ON TOP of the ores-service-base image.
#
# The base carries all the common shared libraries, the healthcheck
# probe, and the empty /app/{log,run,storage} directories.  This
# Dockerfile adds just this service's binary (and any service-specific
# extra shared libraries) — a few COPY instructions, no rebuild of
# the heavy content.
#
# Build context expects::
#
#   build/docker-stage/services/<SERVICE_NAME>/bin/<SERVICE_NAME>
#   build/docker-stage/services/<SERVICE_NAME>/lib/…  (optional)
#
# Built with::
#
#   podman build --format docker                          \
#       --build-arg SERVICE_NAME=ores.iam.service         \
#       -t localhost/ores.iam.service:local               \
#       -t localhost/ores.iam.service:<tag>               \
#       -f docker/service-runtime.Dockerfile .

ARG BASE_IMAGE=localhost/ores-service-base
ARG BASE_TAG=local
FROM ${BASE_IMAGE}:${BASE_TAG}

ARG SERVICE_NAME=ores.iam.service

# Service binary + entrypoint symlink.
# The staging directory contains:
#   <SERVICE_NAME>           real binary
#   entrypoint -> ./<SERVICE_NAME>   relative symlink
# COPY into a directory (trailing /) preserves the symlink as-is.
COPY build/docker-stage/services/${SERVICE_NAME}/bin/ /app/bin/

# Service-specific extra libs (may not exist for every service).
COPY build/docker-stage/services/${SERVICE_NAME}/lib/ /app/lib/

ENTRYPOINT ["./entrypoint"]
