// Creates one Acme account per role the walk covers, acting as Acme's
// administrator: save the account, associate it with Acme's top operational
// party, and assign the role. Idempotent enough to rerun: a taken username
// is reported and the association and role are attempted again.
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
    name: 'walk-setup',
});
await transport.connect();
const client = new OresClient({ transport });
const any = { parse: (v) => v, safeParse: (v) => ({ success: true, data: v }) };
const call = (subject, body) => client.callAuthenticated(subject, body, any);

const [adminPassword, memberPassword] = process.argv.slice(2);
const host = 'acme_corporation';
const outcome = await client.login({ principal: 'acme_admin@' + host, password: adminPassword });
if (outcome.kind === 'party-selection-required') {
    await client.selectParty({ partyId: outcome.availableParties[0].id, expected: outcome });
}
const parties = await call('refdata.v1.parties.list', { offset: 0, limit: 50, order: { field: '', descending: false } });
const top = parties.parties.find((p) => p.party_category === 'Operational' && p.parent_party_id == null);
console.log('party', top.short_code, top.id);

for (const role of ['Viewer', 'Trading', 'Sales', 'Support', 'Operations']) {
    const username = 'walk_' + role.toLowerCase();
    const principal = username + '@' + host;
    const saved = await call('iam.v1.accounts.save', {
        principal, password: memberPassword, totp_secret: '',
        email: username + '@example.com', account_type: 'user',
    });
    let accountId = saved.account_id;
    if (!saved.success) {
        const accounts = await call('iam.v1.accounts.list', { offset: 0, limit: 100, order: { field: '', descending: false } });
        accountId = (accounts.accounts ?? []).find((a) => a.username === username)?.id ?? '';
    }
    const linked = await call('iam.v1.account_parties.put', {
        change: { write: { account_id: accountId, party_id: top.id }, precondition: { kind: 'any', version: null } },
        intent: { reason_code: '', commentary: '' },
    });
    const assigned = await call('iam.v1.roles.assign-by-name', { principal, role_name: role });
    console.log(JSON.stringify({ role, username, saved: saved.success || saved.message, accountId,
        linked: linked.result?.outcome ?? linked, assigned: assigned.success || assigned.error_message }));
}
await transport.close();
