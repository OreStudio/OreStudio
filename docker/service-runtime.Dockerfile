# Per-service image — built ON TOP of the shared-library image.
#
# The base carries the healthcheck probe and the empty
# /app/{log,run,storage} directories; the library image carries every
# shared library any service loads. This Dockerfile adds one thing: this
# service's binary, its build record and the entrypoint symlink. Nothing
# here changes when another service or another library changes, so the
# layer is a few MB.
#
# Build context expects::
#
#   build/docker-stage/services/<SERVICE_NAME>/bin/<SERVICE_NAME>
#
# Built with::
#
#   podman build --format docker                          \
#       --build-arg SERVICE_NAME=ores.iam.service         \
#       -t localhost/ores.iam.service:local               \
#       -t localhost/ores.iam.service:<tag>               \
#       -f docker/service-runtime.Dockerfile .

ARG BASE_IMAGE=localhost/ores-service-libs
ARG BASE_TAG=local
FROM ${BASE_IMAGE}:${BASE_TAG}

ARG SERVICE_NAME=ores.iam.service

# Service binary, its version manifest and the entrypoint symlink.
# The staging directory contains:
#   <SERVICE_NAME>           real binary
#   version.manifest         the build record the binary reads
#   entrypoint -> ./<SERVICE_NAME>   relative symlink
# COPY into a directory (trailing /) preserves the symlink as-is.
COPY build/docker-stage/services/${SERVICE_NAME}/bin/ /app/bin/

ENTRYPOINT ["./entrypoint"]
