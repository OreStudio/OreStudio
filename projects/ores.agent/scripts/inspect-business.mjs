import { readFile, writeFile } from 'node:fs/promises';
import { loadConfig } from '../dist/config.js';
import { NatsBackend } from '../dist/backend.js';
import { Sessions } from '../dist/auth.js';
import { inspectBusiness } from '../dist/business-baseline.js';
const [configPath, credentialsPath, partyId, outputPath] = process.argv.slice(2);
if (!configPath || !credentialsPath || !partyId || !outputPath) throw Error('Pass agent config, private credentials JSON, selected party UUID and private output JSON path.');
const config = await loadConfig(configPath), credentials = JSON.parse(await readFile(credentialsPath, 'utf8'));
const backend = await NatsBackend.open(config.nats), sessions = new Sessions(backend);
try {
    const login = await sessions.login({ principal: credentials.tenantUsername, password: credentials.tenantPassword, partyId });
    const report = await inspectBusiness(backend, sessions.require('Bearer ' + login.accessToken));
    await writeFile(outputPath, JSON.stringify(report, null, 2) + '\n', { mode: 0o600 });
    console.log(JSON.stringify({ outputPath, reportingReady: report.reportingReady, resources: Object.fromEntries(Object.entries(report.findings).map(([k, v]) => [k, { status: v.status, selectedPartyCount: v.selectedPartyCount }])), gaps: report.gaps }));
} finally { await sessions.close(); await backend.close(); }
