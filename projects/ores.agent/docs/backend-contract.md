# NATS adapter contract

Pinned reference: OreStudio `fc4f559dfb187a368bf2a8cd4053d8e833348835`.
The adapter contains an independently maintained, validated read-only subset of
the generated IAM, currency, market-series and observation protocols. It does
not import the backend checkout or its private `0.0.1` wire package. Before
upgrading the backend, run `scripts/smoke-backend.mjs` against a test deployment.
An incompatible reply fails closed; no HTTP or direct SQL fallback exists.

The allowlist is in `src/backend.ts`. Requests use MessagePack, mTLS, a user
Bearer token, Nats-Session-Id and a fresh correlation ID. Reads time out after
eight seconds without retries. Compressed and decoded replies are bounded.
Only selected DTO fields leave the adapter. Backend result envelopes and
authorization remain authoritative.

Agent access tokens are random, short-lived opaque credentials, hashed in
memory. Backend credentials/tokens remain server-side. Sessions have separate
tenant/party identities and tool budgets. Logout revokes the local token
immediately; restart loses all sessions. Sign in again on expiry; automatic
refresh and party switching are intentionally absent from this first version.

Development deployment uses a dedicated `CN=ores.agent` client certificate.
The existing development broker requires mTLS but does **not** map certificate
identities to per-subject ACLs. Application allowlisting is therefore not a
broker security boundary. For a restricted hosted deployment, provision a
broker identity that publishes only the subjects in the adapter allowlist and
subscribes only to its request/reply inbox. This application never rewrites
the running broker configuration. Backend user authorization applies in both
environments; a shared administrator service account is not used.

Live Windows smoke (2026-11-06): dedicated certificate, tenant_admin in Acme
Corporation Plc; 170 currencies and 392 market series visible. First sampled
series had zero observations. Empty observations are reported as empty data,
not as a price. Fixture tests are separate from these live results.
