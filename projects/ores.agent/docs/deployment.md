# Separate application deployment

Build and package in the component checkout, using Node 22.11+:

```text
npm ci --ignore-scripts
npm test
npm run package
```

These commands never invoke CMake, install models or start services. The package
contains a bundled `agent.mjs`, dependency-free `service.mjs` and a manifest
with source revision and file checksums. Transfer that artifact to the target
Windows or Linux host. No npm installation is required there; Node is required.
The source-free build option `ORES_AGENT_SOURCE_REVISION` must be a full Git SHA
supplied by the build pipeline; otherwise packaging reads the checkout's HEAD.

Installation and configuration are separate operations:

```text
node scripts/deploy.mjs install <artifact-directory> <app-root-outside-checkout>
node scripts/deploy.mjs configure <app-root> <installed-release-directory>
```

Install verifies hashes and refuses to overwrite a damaged existing release.
Releases include manifest identity in their directory names. Configure preserves
an existing private `config/agent.json` and writes an explicit `service.json`.
Set NATS URL/prefix, a dedicated client certificate/CA/key and the separate
inference key-file path in agent.json. No main-server `.env` is loaded or edited.
Protect configuration/key directories with the operating system's user ACLs;
POSIX-created private files use mode 0600. See backend-contract.md for broker
ACL limits and mcp.md for the local bearer authentication profile.

Use the **installed** lifecycle script; the checkout is not needed to operate:

```text
node <release>/service.mjs start <app-root>/service.json
node <release>/service.mjs status <app-root>/service.json
node <release>/service.mjs stop <app-root>/service.json
```

The detached supervisor owns exactly one child and authenticates control
requests over a random loopback port with a private per-run secret. It does not
signal processes based on a saved PID. Invalid or stale control records fail
closed; inspect `run/stdout.log`, `run/stderr.log` and the recorded supervisor
before removing a stale record manually. There is no automatic restart policy
or OS boot registration in this local version. Startup has a 30-second health
deadline; a failed health check leaves diagnostic state for status/stop.
`/readyz` checks NATS readiness; MCP tools can remain available while inference
is stopped. Chat failures identify inference unavailability separately.

Upgrade: install a new artifact, stop the current app, configure the new release
and start it. Rollback: stop, configure an existing older release, start. This
never rebuilds or touches OreStudio or inference. Operator configuration and
keys survive release changes; sessions do not survive process restarts.
Inference uses its own root, spec and lifecycle. After configuring inference,
its lifecycle is also copied to `<inference-root>/service.mjs` so it can be
operated without the checkout. Neither lifecycle controls the other component.

The manual GitHub workflow verifies Windows/Linux on Node 22/24; it has no PR
trigger. Local Linux validation also runs in a disposable Node container with
only component source mounted, without backend credentials. A Linux NVIDIA
host is still required to validate the native CUDA runtime on that OS.
