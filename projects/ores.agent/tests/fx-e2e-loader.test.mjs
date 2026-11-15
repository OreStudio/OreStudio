// SPDX-License-Identifier: GPL-3.0-or-later
import test from 'node:test';
import assert from 'node:assert/strict';
import { loadFxDataset } from '../scripts/fx-e2e-loader.mjs';
import { fxDataset } from '../evals/fx-e2e-dataset.mjs';
const tenantId = '11111111-1111-4111-8111-000000000001',
    partyId = '11111111-1111-4111-8111-000000000002';
function fixture() {
    const store = new Map(),
        writes = [];
    const key = (s, k) => s.split('.')[2] + '/' + (k.code ?? k.id);
    const request = async (subject, args) => {
        const entity = subject.split('.')[2],
            property = {
                providers: 'provider',
                market_series: 'market_series',
                market_observations: 'market_observation',
            }[entity];
        if (subject.endsWith('.get')) {
            const value = store.get(key(subject, args.key));
            return value
                ? { result: { outcome: 'ok' }, [property]: structuredClone(value) }
                : { result: { outcome: 'missing' } };
        }
        const changes = args.changes ?? [args.change];
        for (const c of changes) {
            assert.equal(c.precondition.kind, 'must_not_exist');
            assert.ok(!store.has(key(subject, c.write)));
        }
        for (const c of changes)
            store.set(key(subject, c.write), { ...structuredClone(c.write), tenant_id: tenantId });
        writes.push(subject);
        return { result: { outcome: 'ok' } };
    };
    return { store, writes, systemRequest: request, tenantRequest: request, tenantId, partyId };
}
test('isolated dataset setup previews without writes, persists all records and is idempotent', async () => {
    const f = fixture();
    const plan = await loadFxDataset(f);
    assert.equal(plan.missing.length, 13);
    assert.equal(f.writes.length, 0);
    const result = await loadFxDataset({ ...f, apply: true });
    assert.equal(result.created, 13);
    assert.equal(f.store.size, 13);
    assert.equal(f.writes.length, 5);
    assert.equal(f.writes.at(-1), 'marketdata.v1.market_observations.put_many');
    const again = await loadFxDataset({ ...f, apply: true });
    assert.equal(again.created, 0);
    assert.equal(again.reused, 13);
    assert.equal(f.writes.length, 5);
});
test('a conflicting existing observation prevents all dataset writes', async () => {
    const f = fixture(),
        dataset = fxDataset(tenantId, partyId),
        row = dataset.observations[0];
    f.store.set('market_observations/' + row.id, { ...row, tenant_id: tenantId, value: '999' });
    await assert.rejects(() => loadFxDataset({ ...f, apply: true }), /conflicts/);
    assert.equal(f.writes.length, 0);
});
test('a lost write response is reconciled from exact stored records without duplicate submission', async () => {
    const f = fixture(),
        request = f.tenantRequest;
    let lost = false;
    f.tenantRequest = async (subject, args) => {
        const r = await request(subject, args);
        if (subject.endsWith('.put_many') && !lost) {
            lost = true;
            throw Error('LABELLED lost response');
        }
        return r;
    };
    assert.equal((await loadFxDataset({ ...f, apply: true })).created, 13);
    assert.equal(f.writes.filter((s) => s.endsWith('.put_many')).length, 1);
});
