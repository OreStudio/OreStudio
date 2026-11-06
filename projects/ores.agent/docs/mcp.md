# External MCP clients

The combined application serves `/mcp` using the official TypeScript SDK's
production HTTP handler and Node adapter. It supports revisions 2026-07-28
and 2025-11-25 (stateless Streamable HTTP compatibility). There is no separate
gateway process and no MCP loopback in the built-in agent. SDK request
cancellation reaches the same shared executor as local tool requests.

This first deployment uses an explicit **local bearer authentication profile**,
not OAuth discovery. Sign in to `POST /auth/login` with `principal`, `password`
and, where needed, `partyId`. Supply the returned short-lived `accessToken` as
`Authorization: Bearer ...` on every MCP exchange. Configure it privately in
clients that support custom headers. Never copy the backend session token or
NATS keys to an MCP client. `POST /auth/logout` revokes the agent token.
An MCP session identifier is not an authentication credential. No OAuth
authorization-server metadata is advertised; clients requiring automatic
OAuth enrollment are not supported in this local version.

Only loopback binding is accepted. For remote hosting use a TLS reverse proxy,
preserve Authorization and the expected loopback Host header, and configure
the exact browser Origin if browser access is required. Do not expose the
loopback login/MCP listener directly to a network. A future hosted auth profile
should use the deployment's authorization server rather than inventing OAuth
inside OreStudio.

Run `node scripts/smoke-mcp.mjs <agent-url> <private-credentials-json> <party-uuid>`
to verify discovery and live currency reads without running a model. It uses
the official independent client SDK, signs out, and never prints credentials.
The initial Windows smoke on 2026-11-06 discovered three read tools and read the
first three of 170 real currencies under Acme's selected party. Automated
tests additionally reject missing/revoked bearer credentials and injected
tenant arguments.

Version 0.1.1 adds `read_fx_spot` and `read_ir_rate` to the same catalogue.
Both use the shared selected-party lookup and return structured ambiguity,
freshness and missing-data statuses; see market-lookups.md. Automated MCP client
tests execute a dated FX lookup, not just catalogue discovery.
