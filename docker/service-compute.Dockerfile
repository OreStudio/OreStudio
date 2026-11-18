# Compute-wrapper image.
#
# The wrapper is a single binary, so its image is the shared library image
# plus that binary: the libraries it loads are the same ones the services
# load, and a separate copy of them would only exist to be updated twice.
#
# Built with:
#   podman build --format docker \
#     --build-arg SERVICE_NAME=ores.compute.wrapper \
#     -t localhost/ores-compute-wrapper:local \
#     -t localhost/ores-compute-wrapper:<tag> \
#     -f docker/service-compute.Dockerfile .

ARG BASE_IMAGE=localhost/ores-service-libs
ARG BASE_TAG=local
FROM ${BASE_IMAGE}:${BASE_TAG}

ARG SERVICE_NAME=ores.compute.wrapper

# The wrapper binary, its build record and the entrypoint symlink, staged
# by library_partition.stage_for_build().
COPY build/docker-stage/services/${SERVICE_NAME}/bin/ /app/bin/

ENTRYPOINT ["./entrypoint"]
