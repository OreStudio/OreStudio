# ORE Studio Web

A TypeScript web interface for ORE Studio.

The prototype that this component adopted was called Volga. It now lives in ORE
Studio as the component `projects/ores.web`.

The Qt client was slow to build and reached the limits of what a C++ UI toolkit
can express. This component reimplements the interface for the browser. It talks
to the same backend over the same NATS subjects. The structure allows desktop
packaging later without reworking the application.

## Two concerns, kept apart

Earlier versions of this project merged two different things into one screen:
where the application points, and who you are. They belong to different people.

**Where the application points is deployment configuration.** One JSON file
declares every environment and which one this site serves. An operator chooses
it at start. It never appears in the interface.

**Who you are is authentication.** A username and a password, on an ordinary web
login form. Nothing else. A browser that can name a host can ask the server to
connect to it. This browser cannot.

## Configuration

`config/environments.json` is the one place the environments are declared. It
also declares the shared certificates, whether the developer surface is offered,
and the ACME test accounts. See `config/README.md`.

Point a deployment at an environment when you start it:

```sh
npm run dev:bff -- --env brave_hopper
```

Or use the environment variable, which is what a container would use:

```sh
ORES_WEB_ENV=brave_hopper npm run dev:bff
```

The process logs which environment it serves as its first line. The worst
failure mode is not knowing whether you are looking at staging or production.
The interface is to repeat the environment permanently, beside the copyright.

The component has no `.env` of its own. It reads the checkout's `.env` through
`ORES_WEB_*` for its own settings and `ORES_NATS_*` for the broker.
`compass services start` launches it. The session cookie holds an opaque
random identifier, so the BFF needs no signing secret.

## The interface

The interface is not built yet. The browser renders the application shell and a
session line, and it declares no screens and no routes
(`packages/web/src/main.tsx`). The paragraphs below state the design that the
journey screens are to follow.

The landing page uses the same layout as the project site at
orestudio.github.io. It uses the same artwork, the same heading, and the same
links to ORE and QuantLib. A link from there arrives here, and the two should
not feel like different products.

The header carries the mark, a link to the project site named Site, and Sign in.
Once signed in, it also offers Accounts, a Deployment page when the deployment
has the developer surface on, and Sign out.

Signing in is a username, a password, and a Show toggle. Nothing else. There is
no heading repeating the button, no environment notice, and no field for
anything the deployment already knows.

The environment is a small permanent marker in the footer beside the copyright.
It is not a field and not a header item. It is small because it is not an
action. It is permanent because the worst failure mode is not knowing which
environment you are looking at.

The Deployment page is reachable only after signing in. It holds everything a
person signing in should not have to think about: which environment this process
serves, where it points, which file chose it, and what else that file declares.
The server serves it only when the deployment has the developer surface switched
on, because it names the host, the port, and the namespace.

## Architecture

```
browser  --HTTP-->  BFF  --NATS over mTLS-->  ores.*.service
                     |
                     +-- holds the session token, never the browser
```

The browser cannot reach NATS directly. The broker requires mutual TLS, and no
browser can present a client certificate on a WebSocket. A server component is
therefore mandatory. That component buys a stronger boundary than a direct
connection would. The browser is not told the host, the port, the namespace, or
the certificates. It never holds the bearer token.

## Packages

| Package               | Directory                | Responsibility                                                                                                                                                      |
| --------------------- | ------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `@ores/wire-protocol` | `packages/wire-protocol` | The ORE NATS protocol: msgpack codec, subjects, schemas, session lifecycle, mTLS transport.                                                                         |
| `@ores/contracts`     | `packages/contracts`     | The HTTP shapes the BFF parses. The browser does not import the package today; it parses with `@ores/wire-protocol/browser`. The shared-schema split is unfinished. |
| `@ores/org`           | `packages/org`           | Parses org text into a line-indexed outline and renders it as safe HTML. The BFF reads and writes scenario docs with it, and the browser renders them.              |
| `@ores/bff`           | `packages/bff`           | The Fastify server. It owns the NATS connection and the session token.                                                                                              |
| `@ores/web`           | `packages/web`           | The React client. It talks to the BFF only.                                                                                                                         |

## Running the stack

`compass services start` is the way to run this component, as it is for every
other one. The fleet includes `ores.web.service`. The BFF serves the built
browser bundle and the `/api` routes from one process on `ORES_WEB_PORT`. In
this checkout that port is 21402.

Build the component before you start the fleet. From `projects/ores.web`, run
`npm ci` and `npm run build`. The service runs the built BFF from
`packages/bff/dist`.

For hot reload while working on the client, start the fleet and then the Vite
development server, which proxies `/api` to the BFF. It reads the checkout's
`.env`, so it needs no arguments and it cannot disagree with the BFF about
which port it is proxying to:

```sh
compass services start
npm run dev:web
```

`compass services stop` stops the fleet.

## Verification

Two verifiers assert against the running system rather than a mock.

`scripts/verify-login.ts` exercises the protocol layer. Its first assertion is a
deliberately rejected login. A server that decoded the msgpack body answers with
a message. A server that did not decode it never replies at all. A well-formed
rejection therefore proves the subject, the encoding, and the response schema in
one step.

`scripts/verify-first-run.ts` walks the First run journey the way the browser
walks it: the same client (`packages/web/src/api/client.ts`), the same
request-building and password rules, in the journey's order, from an empty
installation to _Ready_. It refuses to run unless the deployment still needs its
administrator, because the first run is the one path that can only happen on an
empty database, so its run starts by wiping this environment's data.

Unit tests cover the pieces that must not drift. They include a golden-bytes
test that an empty request encodes as the msgpack empty map, and that every
declared field is written even when the caller omits it.

```sh
npm test
npm run verify:login

compass services stop
compass db recreate -y -k
compass services start
npm run verify:first-run
```

## The test account

Row level security restricts every read to the tenant the service runs in. A
seeded account must therefore live in that tenant. The password hash has to come
from the project's own hasher, so `scripts/make-test-hash.cpp` links the real
`libores.security` rather than reimplementing scrypt.

```sh
npm run seed:account -- ores_web_probe 'Secure-Password-123'
```

## Local desktop packaging

This work is deferred, and the structure anticipates it. The web client talks to
an HTTP API and holds no token, so a Tauri or Electron shell can host the same
bundle unchanged. The BFF would either ship alongside it or be replaced by a
thin host that uses `@ores/wire-protocol` directly. That is why that package is
separate from the server that currently drives it.

## Generated types

`ores.codegen` generates the TypeScript from the same org models that drive the
C++ headers. Two facets write into this component:

- `ores.ts.protocol` writes
  `packages/wire-protocol/src/generated/{component}/protocol/{entity}_protocol.ts`.
  The file holds the wire payload interfaces and the NATS subject constants.
- `ores.ts.domain` writes
  `packages/wire-protocol/src/generated/{component}/domain/{entity}.ts`.
  The file holds the TypeScript shape of one entity or junction.

Regenerate the wire types of one entity:

```sh
./compass.sh codegen regenerate --component refdata --entity currency --address ores.ts.protocol
```

Or regenerate from one model file:

```sh
./compass.sh codegen generate --model projects/ores.refdata/modeling/ores.refdata.currency.org --address ores.ts.protocol
```

Regenerate all the TypeScript:

```sh
./compass.sh codegen regenerate --all --address ores.ts
```

Do not edit the generated files. Change the org model, then regenerate.

A codegen drift check covers the generated output. It regenerates the models and
fails when the checked-in files differ from the generated ones.

```sh
python3 projects/ores.codegen/scripts/check_component_drift.py --all
```

See `projects/ores.web/modeling/0001-protocol-types-from-codegen.org` for the
decision behind the generated wire types.
