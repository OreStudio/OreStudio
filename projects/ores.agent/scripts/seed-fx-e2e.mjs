// SPDX-License-Identifier: GPL-3.0-or-later
import { readFile, writeFile } from 'node:fs/promises';
import { loadConfig } from '../dist/config.js';
import { NatsBackend } from '../dist/backend.js';
import { Sessions } from '../dist/auth.js';
import { datasetNats } from './fx-e2e-nats.mjs';
import { loadFxDataset } from './fx-e2e-loader.mjs';
const [configPath, credentialsPath, partyId, output, ...flags] = process.argv.slice(2);
if (!configPath || !credentialsPath || !partyId || !output || flags.some((f) => f !== '--apply'))
    throw Error('Usage: seed-fx-e2e.mjs CONFIG PRIVATE_CREDENTIALS PARTY NEW_REPORT [--apply]');
const config = await loadConfig(configPath),
    c = JSON.parse(await readFile(credentialsPath, 'utf8'));
await writeFile(
    output,
    JSON.stringify({ status: 'started', mode: flags.includes('--apply') ? 'apply' : 'plan' }) +
        '\n',
    { flag: 'wx', mode: 0o600 },
);
const backend = await NatsBackend.open(config.nats),
    sessions = new Sessions(backend);
let writer;
try {
    const tenant = await sessions.login({
        principal: c.tenantUsername,
        password: c.tenantPassword,
        partyId,
    });
    const system = await sessions.login({
        principal: c.systemUsername,
        password: c.systemPassword,
    });
    const identity = sessions.require('Bearer ' + tenant.accessToken),
        admin = sessions.require('Bearer ' + system.accessToken);
    writer = await datasetNats(config.nats);
    const report = await loadFxDataset({
        tenantId: identity.tenantId,
        partyId: identity.partyId,
        systemRequest: writer.requestFor(admin),
        tenantRequest: writer.requestFor(identity),
        apply: flags.includes('--apply'),
    });
    await writeFile(output, JSON.stringify(report, null, 2) + '\n', { mode: 0o600 });
    console.log(JSON.stringify(report));
} finally {
    await writer?.close();
    await sessions.close();
    await backend.close();
}
