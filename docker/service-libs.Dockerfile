# The shared-library layer of the image hierarchy.
#
# Carries every library any service loads. A library that eleven services
# need is stored and transferred once here, instead of inside eleven
# per-service images. The per-service images (service-runtime.Dockerfile)
# add only their own binary on top of this image.
#
# The build context expects build/docker-stage/libs/ to have been
# populated by library_partition.stage_for_build() from libraries already
# stripped by stage-runtime.sh.

ARG BASE_IMAGE=localhost/ores-service-base
ARG BASE_TAG=local
FROM ${BASE_IMAGE}:${BASE_TAG}

# Shared libraries — the SONAME symlinks and their real files.
COPY build/docker-stage/libs/lib/ /app/lib/
