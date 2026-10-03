// Signs in to a tenant whose account must choose a party, then tries to
// refresh the party-choice token before choosing; refresh must refuse it.
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
    name: 'party-choice-refresh-probe',
});
await transport.connect();
const client = new OresClient({ transport });
const [principal, password] = process.argv.slice(2);
const outcome = await client.login({ principal, password });
console.log('login', outcome.kind);
try {
    await client.refresh();
    console.log('refresh ACCEPTED: the party-choice token was renewed');
} catch (error) {
    console.log('refresh refused:', error.constructor.name);
}
await transport.close();
