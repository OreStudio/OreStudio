# NATS adapter contract

Pinned backend reference: `fc4f559dfb187a368bf2a8cd4053d8e833348835`.
The adapter maintains its own checked subset of the IAM, reference, market,
trading and reporting protocols. It does not import the moving web/BFF package.
The operation allowlist is in `src/backend.ts`.

After building the agent, check a test deployment before a backend upgrade:

```text
node scripts/smoke-backend.mjs <PRIVATE_AGENT_CONFIG> <PRIVATE_CREDENTIALS_JSON> <PARTY_UUID>
```

The party argument is optional when the account has a valid default. The smoke
prints scoped business data; keep its output private. An incompatible reply produces
an error. There is no HTTP or direct SQL fallback.

## Requests and identity

Requests use MessagePack, mTLS, the user's Bearer token, `Nats-Session-Id` and
a fresh correlation ID. Reads have an eight-second deadline and no retry.
Cancellation discards the result; the NATS request can remain active until its
deadline. Compressed replies, decoded replies and tool output have size limits.

Agent tokens are random, short-lived and hashed in memory. Backend credentials
stay server-side. Logout revokes the agent token; restart removes all sessions.
There is no automatic token refresh or in-session party switch.

Tools validate returned tenant/party identity and required fields. The backend
still enforces access. Current scope queries use existing historical booking
and report-metric reads. Adding an agent allowlist entry does not add a server
handler or change a broker permission.

## Local security limit

The local client uses a dedicated `CN=ores.agent` certificate. The development
broker requires mTLS but does not assign certificate-specific subject permissions.
The agent allowlist is therefore not a broker access boundary.

A restricted deployment must verify broker publish/subscribe permissions and
backend user access. It must not use an administrator account as a substitute
for the user. Current deployment commands do not edit broker configuration.
See [future proposals](architecture-future-prs.md) for conditional security work.
