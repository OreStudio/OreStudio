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

# process_supervisor always launches child services with --log-enabled and
# a relative --log-directory ../log (see default_args_template), and writes
# each child's PID file under ../run -- both resolve to /app/log and
# /app/run given WORKDIR /app/bin below. ores.http.server's own
# args_template additionally passes --storage-dir ../storage (/app/storage).
# The final base image has no shell to mkdir with, so create them here and
# COPY across; world-writable since the runtime uid is only known at
# `podman run --user ...` time, not build time.
RUN mkdir -p /src/log /src/run /src/storage && chmod 777 /src/log /src/run /src/storage

# Chainguard glibc-dynamic has no shell to `ln -s` with, so the entrypoint
# symlink is created here instead, where the real binary already exists --
# relative, so it resolves identically once COPY'd into /app/bin/ below.
ARG SERVICE_NAME=ores.controller.service
# Fail the build loudly if SERVICE_NAME and stage-runtime.sh's --service
# disagree (or --service wasn't used), rather than shipping a dangling
# symlink that only fails once the container actually starts.
RUN test -f "/src/bin/${SERVICE_NAME}" || \
    (echo "SERVICE_NAME=${SERVICE_NAME} was not staged -- check stage-runtime.sh --service" >&2; exit 1)
RUN ln -s "./${SERVICE_NAME}" /src/bin/entrypoint

FROM cgr.dev/chainguard/glibc-dynamic:latest

WORKDIR /app

COPY --from=strip /src/bin/ /app/bin/
COPY --from=strip /src/lib/ /app/lib/
COPY --from=strip /src/log/ /app/log/
COPY --from=strip /src/run/ /app/run/
COPY --from=strip /src/storage/ /app/storage/

ENV LD_LIBRARY_PATH=/app/lib
WORKDIR /app/bin

ENTRYPOINT ["./entrypoint"]
