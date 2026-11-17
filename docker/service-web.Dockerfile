# The web service image.
#
# The service is Node, so this image differs from the others in three ways:
# its runtime is Node rather than the compiled-service base, it carries the
# browser bundle it serves, and its healthcheck is a TCP probe written in Node
# (the static probe the other images use is a C binary that knows nothing
# about an HTTP port).
#
# The build context holds the staged tree from docker/stage-web.sh, laid out
# as the checkout is: config/ beside packages/bff/dist and packages/web/dist.
#
# Built with:
#   podman build --format docker \
#     -t localhost/ores-web:local \
#     -t localhost/ores-web:<tag> \
#     -f docker/service-web.Dockerfile .

# The plain Alpine base with the distribution's nodejs package, not the
# official node image: that image carries npm, npx, corepack and the headers
# for native addons, none of which a runtime image uses, and they are most of
# its 171 MB. The bundle is plain JavaScript, so musl is not a problem, and
# this base is about 60 MB.
#
# apk must run before the copy below: the build context holds node_modules
# with relative symlinks, and a package install afterwards would have to walk
# them.
FROM alpine:3.22

RUN apk add --no-cache nodejs

WORKDIR /app

# Compiled JavaScript, the browser bundle, the site configuration and the
# installed workspace dependencies.
COPY build/docker-stage/web/ /app/

ENV NODE_ENV=production
# The port the BFF binds; the deployment passes its own through ORES_WEB_PORT.
ENV ORES_WEB_PORT=8080

EXPOSE 8080

# A TCP connect to the bound port. The BFF logs readiness once it has bound,
# and a container that cannot accept a connection is not serving.
HEALTHCHECK --interval=5s --timeout=3s --start-period=20s --retries=3 \
    CMD ["node", "-e", "const p=Number(process.env.ORES_WEB_PORT||8080);const s=require('net').connect(p,'127.0.0.1',()=>{s.end();process.exit(0)});s.on('error',()=>process.exit(1));"]

ENTRYPOINT ["node", "packages/bff/dist/main.js"]
