// SPDX-License-Identifier: GPL-3.0-or-later
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { loadConfig } from "../dist/config.js";
import { NatsBackend } from "../dist/backend.js";
const options = {}, allowed = /* @__PURE__ */ new Set(["profile", "config", "credentials", "party"]);
for (let i = 2; i < process.argv.length; i += 2) {
  const key = process.argv[i].replace(/^--/, "");
  if (!process.argv[i].startsWith("--") || !allowed.has(key) || !process.argv[i + 1] || process.argv[i + 1].startsWith("--")) throw Error("Usage: preflight.mjs --profile replay|model|live [--config FILE --credentials FILE --party UUID]");
  options[key] = process.argv[i + 1];
}
async function main() {
  const profile = options.profile ?? "replay";
  if (!["replay", "model", "live"].includes(profile)) throw Error("Invalid profile.");
  const checks = [{ name: "profile", status: "pass" }];
  let config;
  if (profile !== "replay") {
    try {
      if (!options.config) throw Error();
      config = await loadConfig(resolve(options.config));
      checks.push({ name: "configuration", status: "pass" });
    } catch {
      checks.push({ name: "configuration", status: "unavailable" });
    }
    if (config) {
      try {
        const key = config.inference.apiKeyFile ? (await readFile(config.inference.apiKeyFile, "utf8")).trim() : null;
        const r = await fetch(config.inference.baseUrl.replace(/\/$/, "") + "/models", { headers: key ? { authorization: "Bearer " + key } : {}, signal: AbortSignal.timeout(5e3) });
        if (!r.ok) {
          await r.body?.cancel();
          throw Error();
        }
        const body = await r.json();
        if (!body.data?.some((m) => m.id === config.inference.model)) throw Error();
        checks.push({ name: "configured_model_endpoint", status: "pass" });
      } catch {
        checks.push({ name: "configured_model_endpoint", status: "unavailable" });
      }
    }
  }
  if (profile === "live") {
    try {
      const c = JSON.parse(await readFile(resolve(options.credentials ?? ""), "utf8"));
      if (typeof c.tenantUsername !== "string" || !c.tenantUsername || typeof c.tenantPassword !== "string" || !c.tenantPassword || !/^[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}$/i.test(options.party ?? "")) throw Error();
      checks.push({ name: "existing_identity_inputs", status: "pass" });
    } catch {
      checks.push({ name: "existing_identity_inputs", status: "unavailable" });
    }
    let backend;
    try {
      if (!config?.nats) throw Error();
      backend = await NatsBackend.open(config.nats);
      if (!backend.ready()) throw Error();
      checks.push({ name: "nats_tls_connection", status: "pass" });
    } catch {
      checks.push({ name: "nats_tls_connection", status: "unavailable" });
    } finally {
      await backend?.close();
    }
  }
  const ready = checks.every((c) => c.status === "pass");
  console.log(JSON.stringify({ profile, ready, checks, limitations: "Connectivity and input presence only; no login, business reads/writes, inference generation, GPU attestation or capability/data/permission proof. Live evaluation separately requires --live-read; alternate identity and artifact provenance are recorded by the runner." }));
  return ready ? 0 : 1;
}
main().then((code) => {
  process.exitCode = code;
}).catch(() => {
  console.error("Preflight failed. Check profile/options and private paths; no secret values are printed.");
  process.exitCode = 1;
});
