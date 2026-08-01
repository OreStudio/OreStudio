# HEALTHCHECK probe: the final chainguard/glibc-dynamic image has no shell
# (so no grep/test to script a check with) -- a tiny statically-linked C
# binary instead, checking any *.log file under /app/log for the
# "Service ready." marker every supervised service logs once it has
# registered its NATS handlers (see ores.service's *_runner_impl.hpp),
# automating today's wait_for_log_ready polling loop as a container-native
# probe.
FROM debian:bookworm-slim AS healthcheck-build

RUN apt-get update && apt-get install -y --no-install-recommends gcc libc6-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /src
COPY docker/healthcheck.c .
RUN gcc -O2 -static -o /healthcheck healthcheck.c

FROM debian:bookworm-slim AS strip

RUN apt-get update && apt-get install -y --no-install-recommends binutils \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /src
# Populated by docker/stage-runtime.sh — only the staged service binary/
# binaries and their actual shared-library dependency closure, not the
# full publish/ tree (which also carries Qt libs and test binaries).
# Strip every staged binary, not just an ores.*.service glob: two
# supervised binaries (ores.compute.wrapper, ores.http.server) don't
# follow that naming convention -- stage-runtime.sh's own comment
# already calls them out -- and the glob silently matched zero files
# for them, failing the whole build ("strip: '/src/bin/ores.*.service':
# No such file").
COPY build/docker-stage/bin/ /src/bin/
COPY build/docker-stage/lib/ /src/lib/
RUN strip --strip-debug /src/bin/* /src/lib/libores.*.so*

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
COPY --from=healthcheck-build /healthcheck /app/bin/healthcheck

ENV LD_LIBRARY_PATH=/app/lib
WORKDIR /app/bin

# Must build this image with `podman build --format docker` -- the default
# OCI format has no health-check concept and silently drops this directive
# entirely (only a build-time warning, easy to miss); `podman ps`/`inspect`
# will never show a health status at all if built without it.
HEALTHCHECK --interval=2s --timeout=2s --start-period=60s --retries=3 \
    CMD ["./healthcheck"]

ENTRYPOINT ["./entrypoint"]
