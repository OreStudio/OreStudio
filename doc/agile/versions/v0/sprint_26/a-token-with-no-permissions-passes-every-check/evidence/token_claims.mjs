// Signs in over NATS as the BFF does and prints the issued token's claims.
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
    name: 'token-claims-probe',
});
await transport.connect();
const client = new OresClient({ transport });
const [principal, password] = process.argv.slice(2);
const outcome = await client.login({ principal, password });
const claims = (t) => JSON.parse(Buffer.from(t.split('.')[1], 'base64url').toString());
console.log(JSON.stringify({ outcome: outcome.kind, roles: claims(client.token).roles ?? [] }));
if (outcome.kind === 'party-selection-required') {
    await client.selectParty({ partyId: outcome.availableParties[0].id, expected: outcome });
    console.log(JSON.stringify({ afterSelect: claims(client.token).roles ?? [] }));
    await client.switchParty({
        partyId: outcome.availableParties[0].id,
        availableParties: outcome.availableParties,
    });
    console.log(JSON.stringify({ afterSwitch: claims(client.token).roles ?? [] }));
}
await client.refresh();
console.log(JSON.stringify({ afterRefresh: claims(client.token).roles ?? [] }));
await transport.close();
