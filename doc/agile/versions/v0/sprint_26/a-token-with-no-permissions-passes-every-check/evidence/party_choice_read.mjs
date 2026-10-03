// Signs in to a tenant whose account must choose a party, then reads parties
// with the party-choice token, which every service must refuse, and again
// after choosing a party, which must be answered.
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
    name: 'party-choice-read-probe',
});
await transport.connect();
const client = new OresClient({ transport });
const [principal, password] = process.argv.slice(2);
const outcome = await client.login({ principal, password });
console.log('login', outcome.kind);
const read = async () => {
    try {
        const answer = await client.callAuthenticated(
            "refdata.v1.parties.list",
            { offset: 0, limit: 5, order: { field: "", descending: false } },
            { parse: (v) => v, safeParse: (v) => ({ success: true, data: v }) },
        );
        return "ANSWERED with " + (answer.parties ?? []).length + " parties";
    } catch (error) {
        return "refused: " + error.constructor.name;
    }
};
console.log("read with the party-choice token:", await read());
if (outcome.kind === "party-selection-required") {
    await client.selectParty({ partyId: outcome.availableParties[0].id, expected: outcome });
    console.log("read after choosing a party:", await read());
}
await transport.close();
