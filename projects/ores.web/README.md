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
npm run dev:bff -- --env bright_hopper
```

Or use the environment variable, which is what a container would use:

```sh
ORES_WEB_ENV=bright_hopper npm run dev:bff
```

The process logs which environment it serves as its first line. The worst
failure mode is not knowing whether you are looking at staging or production.
The interface repeats the environment in the header, permanently.

The component has no `.env` of its own. It reads the checkout's `.env` through
`ORES_WEB_*` for its own settings and `ORES_NATS_*` for the broker.
`compass services start` launches it. The session cookie holds an opaque
random identifier, so the BFF needs no signing secret.

## The interface

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

| Package | Directory | Responsibility |
|---|---|---|
| `@ores/wire-protocol` | `packages/wire-protocol` | The ORE NATS protocol: msgpack codec, subjects, schemas, session lifecycle, mTLS transport. |
| `@ores/contracts` | `packages/contracts` | The HTTP shapes that the BFF and the browser both parse. No Node dependency. |
| `@ores/bff` | `packages/bff` | The Fastify server. It owns the NATS connection and the session token. |
| `@ores/web` | `packages/web` | The React client. It talks to the BFF only. |

## Running the stack

`compass services start` starts the fleet. The fleet includes
`ores.web.service`. The BFF serves the built browser bundle and the `/api`
routes from one process on `ORES_WEB_PORT`. In this checkout that port is 21402.

Build the component before you start the fleet. From `projects/ores.web`, run
`npm ci` and `npm run build`. The service runs the built BFF from
`packages/bff/dist`.

`scripts/dev-stack.sh` remains the standalone development path:

```sh
scripts/dev-stack.sh start
```

That path starts the broker, the IAM and refdata services, the BFF, and the Vite
development server. It waits for each port and prints the URL to open.

## Verification

Two verifiers assert against the running system rather than a mock.

`scripts/verify-login.ts` exercises the protocol layer. Its first assertion is a
deliberately rejected login. A server that decoded the msgpack body answers with
a message. A server that did not decode it never replies at all. A well-formed
rejection therefore proves the subject, the encoding, and the response schema in
one step.

`scripts/verify-browser.ts` drives a real browser through the landing page, the
Deployment page, the sign-in screen, a rejected credential, sign-in, and
sign-out. It asserts the absence as well as the presence: no server field, no
namespace, no connection chooser, and no master password anywhere. It captures
screenshots under `.runtime/screenshots/`.

Unit tests cover the pieces that must not drift. They include a golden-bytes
test that an empty request encodes as the msgpack empty map, and that every
declared field is written even when the caller omits it.

```sh
npm test
npm run verify:login
npm run verify:browser
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

`ores.codegen` generates the TypeScript from the same org entity models that
drive the C++ headers. Two facets write into this component:

- `ores.ts.ui` writes `packages/web/src/generated/{component}/ui/{entity}_ui.ts`.
  The file holds the table columns, the form fields, and the entity meta.
- `ores.ts.protocol` writes
  `packages/wire-protocol/src/generated/{component}/protocol/{entity}_protocol.ts`.
  The file holds the wire payload interfaces and the NATS subject constants.

Regenerate the UI metadata of one entity:

```sh
./compass.sh codegen entity generate <entity> --address ores.ts.ui
```

Regenerate all the TypeScript:

```sh
./compass.sh codegen regenerate --all --address ores.ts
```

The hand-written `packages/web/src/ui-contract.ts` holds the shapes that the
generated files conform to. Do not edit the generated files. Change the org
model, then regenerate.

A codegen drift check covers the generated output. It regenerates the models and
fails when the checked-in files differ from the generated ones.

```sh
python3 projects/ores.codegen/scripts/check_component_drift.py --all
```

See `projects/ores.web/modeling/0001-protocol-types-from-codegen.org` for the
decision behind the generated wire types.
