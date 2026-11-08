# External MCP clients

The agent process serves `/mcp` through the official TypeScript SDK.
It supports protocol revisions 2026-07-28 and 2025-11-25 using Streamable HTTP.
There is no separate gateway. Chat and MCP share one tool executor.
MCP calls do not require the language model.

## Sign in

1. Send `POST /auth/login` with `principal`, `password` and optional `partyId`.
2. Read the returned `accessToken`.
3. Send `Authorization: Bearer <TOKEN>` on every MCP request.
4. Send `POST /auth/logout` with that header to revoke the token.

Use a client that supports private custom headers. An MCP session ID is not a
credential. Do not give clients the backend token or NATS keys.

This is a local bearer profile. It does not provide OAuth discovery or automatic
client enrollment. The listener accepts loopback binding only. Remote hosting
needs a separately reviewed authentication profile, TLS proxy and Origin rules;
a proxy alone does not complete that work.

## Tools and checks

The catalogue has 14 read-only tools. It covers currencies, market data,
discovery, stored values, run status and scope NPV/Greeks. See the business
guides in the [README](../README.md). SDK cancellation reaches the shared executor.
An in-flight NATS read can still finish at its deadline.

```text
node scripts/smoke-mcp.mjs <AGENT_URL> <PRIVATE_CREDENTIALS_JSON> <PARTY_UUID>
```

The smoke uses an independent SDK client, reads currencies and signs out.
It does not print credentials. Tool tests also cover revoked tokens, invalid
arguments and selected-party checks. See [validation](validation.md) for dated
live results and remaining security checks.
