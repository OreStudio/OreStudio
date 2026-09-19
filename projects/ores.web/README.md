# ORE Studio Web

A TypeScript web interface for ORE Studio.

The Qt client is slow to build and reaches the limits of what a C++ UI toolkit
can express. This reimplements the interface for the browser, talking to the
same backend over the same NATS subjects, and is structured so a desktop
packaging can be added later without reworking the application.

## Two concerns, kept apart

Earlier versions of this project merged two different things into one screen:
where the application points, and who you are. They belong to different people.

**Where the application points is deployment configuration.** One JSON file
declares every environment and which one this site serves. It is chosen at
start and never appears in the interface.

**Who you are is authentication.** A username and a password, on an ordinary
web login form. Nothing else, because a browser that can name a host can ask
the server to connect to it, and this one cannot.

## Configuration

`config/environments.json` is the one place the environments are declared, along
with the shared certificates, whether the developer surface is offered, and the
ACME test accounts. See `config/README.md`.

Point a deployment at an environment when you start it:

```sh
npm run dev:bff -- --env bright_hopper
```

Or with the environment variable, which is what a container would use:

```sh
ORES_WEB_ENV=bright_hopper npm run dev:bff
```

The process logs which environment it serves as its first line, because the
worst failure mode is not knowing whether you are looking at staging or
production. The interface repeats it in the header, permanently.

## The interface

The landing page uses the same layout as the project site at
orestudio.github.io, with the same artwork, the same heading and the same links
to ORE and QuantLib, because a link from there arrives here and the two should
not feel like different products.

The header carries the mark, a link to the project site named Site, and Sign in.
Once signed in it also offers Accounts, a Deployment page when the deployment
has the developer surface on, and Sign out.

Signing in is a username, a password and a Show toggle. Nothing else: no
heading repeating the button, no environment notice, and no field for anything
the deployment already knows.

The environment is a small permanent marker in the footer beside the copyright,
not a field and not a header item. Small because it is not an action, permanent
because the worst failure mode is not knowing which environment you are looking
at.

The Deployment page is reachable only after signing in. It holds everything a
person signing in should not have to think about: which environment this process
serves, where it points, which file chose it, and what else that file declares.
It is served only when the deployment has the developer surface switched on,
because it names the host, the port and the namespace.

## Architecture

```
browser  --HTTP-->  BFF  --NATS over mTLS-->  ores.*.service
                     |
                     +-- holds the session token, never the browser
```

The browser cannot reach NATS directly. The broker requires mutual TLS and no
browser can present a client certificate on a WebSocket, so a server component
is mandatory. Having one buys a stronger boundary than a direct connection
would: the browser is not told the host, the port, the namespace or the
certificates, and it never holds the bearer token.

## Packages

| Package | Responsibility |
|---|---|
| `packages/wire-protocol` | The ORE NATS protocol: msgpack codec, subjects, schemas, session lifecycle, mTLS transport. |
| `packages/contracts` | The HTTP shapes the BFF and the browser both parse. No Node dependency. |
| `packages/bff` | Fastify server. Owns the NATS connection and the session token. |
| `packages/web` | React client. Talks to the BFF only. |

## Running the stack

The C++ services have to be running first. `compass services start` installs
systemd units outside the checkout, so this repository starts what it needs
directly:

```sh
scripts/dev-stack.sh start
```

That starts the broker, the IAM and refdata services, the BFF and the web
server, then waits for each port. Open <http://127.0.0.1:21802/>.

Copy `.env.example` to `.env` first and fill in a session secret. The session
secret is required and has no default, because a default would be a backdoor.

## Verification

Two verifiers, both asserting against the running system rather than a mock.

`scripts/verify-login.ts` exercises the protocol layer. Its first assertion is
a deliberately rejected login, because a server that decoded the msgpack body
answers with a message while a server that did not decode it never replies at
all. A well-formed rejection therefore proves the subject, the encoding, and
the response schema in one step.

`scripts/verify-browser.ts` drives a real browser through the landing page, the
Deployment page, the sign-in screen, a rejected credential, sign-in and
sign-out. It asserts the absence as well as the presence: no server field, no
namespace, no connection chooser and no master password anywhere. It captures
screenshots under `.runtime/screenshots/`.

Unit tests cover the pieces that must not drift, including a golden-bytes test
that an empty request encodes as the msgpack empty map, and that every declared
field is written even when the caller omits it.

```sh
npm test
npm run verify:login
npm run verify:browser
```

## The test account

Row level security restricts every read to the tenant the service runs in, so a
seeded account must live in that tenant. The password hash has to come from the
project's own hasher, so `scripts/make-test-hash.cpp` links the real
`libores.security` rather than reimplementing scrypt.

```sh
npm run seed:account -- ores_web_probe 'Secure-Password-123'
```

## Local desktop packaging

Deferred, and the structure anticipates it. The web client talks to an HTTP API
and holds no token, so a Tauri or Electron shell can host the same bundle
unchanged. The BFF would either ship alongside it or be replaced by a thin host
that uses `@ores/wire-protocol` directly, which is why that package is separate from
the server that currently drives it.

## Generated types

Hand-maintaining wire field names in TypeScript is the largest remaining drift
risk, so the intent is to generate them from the C++ codegen model.
`scripts/emit_protocol_ir.py` reads the same org entity model that codegen
renders into the RFL structs and emits a language-neutral protocol IR. It
covers the 148 codegen entities across 13 components, which is 1184 messages.
See `doc/decisions/0001-protocol-types-from-codegen.org`.
