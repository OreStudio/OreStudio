// Proves refresh reads permissions again: grant a role, refresh, revoke it, refresh.
import { readFileSync } from 'node:fs';
import { loadSiteConfiguration } from '/home/marco/Development/OreStudio/ores_dev_eager_maxwell/projects/ores.web/packages/bff/dist/site-config.js';
import { resolveBroker } from '/home/marco/Development/OreStudio/ores_dev_eager_maxwell/projects/ores.web/packages/bff/dist/broker.js';
import { NatsTransport, OresClient } from '/home/marco/Development/OreStudio/ores_dev_eager_maxwell/projects/ores.web/packages/wire-protocol/dist/index.js';

const pem = (v) => (v === '' || v.includes('-----BEGIN') ? v : readFileSync(v, 'utf8'));
const site = loadSiteConfiguration({});
const broker = resolveBroker(site.configuration, site.environment);
const transport = new NatsTransport({
    server: broker.server,
    subjectPrefix: broker.subjectPrefix,
    tls: { ca: pem(broker.tls.ca), cert: pem(broker.tls.cert), key: pem(broker.tls.key) },
    name: 'refresh-rereads-probe',
});
await transport.connect();
const client = new OresClient({ transport });
const [principal, password, username] = process.argv.slice(2);
const outcome = await client.login({ principal, password });
if (outcome.kind === 'party-selection-required') {
    await client.selectParty({ partyId: outcome.availableParties[0].id, expected: outcome });
}
const roles = () => JSON.parse(Buffer.from(client.token.split('.')[1], 'base64url').toString()).roles ?? [];
const any = { parse: (v) => v, safeParse: (v) => ({ success: true, data: v }) };
console.log('before', JSON.stringify(roles()));
console.log('assign', JSON.stringify(await client.callAuthenticated('iam.v1.roles.assign-by-name', { principal: username, role_name: 'Viewer' }, any)));
await client.refresh();
console.log('after grant + refresh', JSON.stringify(roles()));
console.log('revoke', JSON.stringify(await client.callAuthenticated('iam.v1.roles.revoke-by-name', { principal: username, role_name: 'Viewer' }, any)));
await client.refresh();
console.log('after revoke + refresh', JSON.stringify(roles()));
await transport.close();
