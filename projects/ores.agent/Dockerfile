FROM node:24-bookworm-slim@sha256:01d566e8d94d8c619591d78652babfccd3c6992a3dffa9ed5ab39e11cf6c70e9 AS build
WORKDIR /src
COPY package.json package-lock.json ./
RUN npm ci --ignore-scripts
COPY tsconfig.json ./
COPY scripts/build.mjs scripts/
COPY src/ src/
RUN npm run build

FROM node:24-bookworm-slim@sha256:01d566e8d94d8c619591d78652babfccd3c6992a3dffa9ed5ab39e11cf6c70e9
WORKDIR /app
COPY --from=build /src/build/agent.mjs ./
RUN install -d -o node -g node -m 0700 /var/lib/ores-agent
ENV ORES_AGENT_CONFIG=/run/ores-agent/agent.json \
    ORES_AGENT_STATE_DIR=/var/lib/ores-agent
USER node
EXPOSE 20100
HEALTHCHECK --interval=10s --timeout=3s --start-period=10s \
    CMD ["node", "-e", "fetch('http://127.0.0.1:20100/healthz').then(r=>{if(!r.ok)process.exitCode=1}).catch(()=>{process.exitCode=1})"]
ENTRYPOINT ["node", "/app/agent.mjs"]
