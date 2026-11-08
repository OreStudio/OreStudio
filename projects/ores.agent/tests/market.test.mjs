import test from 'node:test';
import assert from 'node:assert/strict';
import { MarketLookup, fxInput, irInput, formatMarketResult } from '../dist/market.js';
const tenant = '11111111-1111-4111-8111-111111111111', party = '22222222-2222-4222-8222-222222222222';
const other = '33333333-3333-4333-8333-333333333333';
const now = Date.parse('2026-11-06T12:00:00Z');
const session = () => ({ token: 'private', sessionId: 'session', tenantId: tenant, partyId: party, active: true, expires: now + 100000 });
const fxUri = 'oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD';
const irUri = 'oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=3M';
const id = n => `00000000-0000-f000-0000-${String(n).padStart(12, '0')}`;
const series = (n = 1, uri = fxUri, provider = 'ore') => ({ id: id(n), tenant_id: tenant, party_id: party, oresmd_uri: uri, provider_code: provider, series_subclass: 'fixture' });
const point = (s, value = '1.0552', time = '2026-11-06 00:00:00.000000Z', uri = s.oresmd_uri.replace('type=series', 'type=quote')) => ({ series_id: s.id, tenant_id: tenant, party_id: party, oresmd_uri: uri, value, observation_datetime: time, source: 'labelled.fixture' });
function fixture(catalog, observations = []) {
    const calls = [];
    const backend = { request: async (op, args, identity, signal) => {
        calls.push({ op, args, identity }); assert.equal(identity.token, 'private'); signal?.throwIfAborted();
        let rows = op === 'series' ? catalog : observations;
        if (op === 'series') {
            if (args.filter?.provider_code) rows = rows.filter(r => r.provider_code === args.filter.provider_code);
            if (args.filter?.id_one_of) rows = rows.filter(r => args.filter.id_one_of.includes(r.id));
        }
        const total = rows.length;
        return { result: { outcome: 'ok' }, total, [op === 'series' ? 'market_series' : 'market_observations']: rows.slice(args.offset, args.offset + args.limit) };
    } };
    return { lookup: new MarketLookup(backend, () => now), backend, calls };
}
const fx = args => fxInput.parse({ baseCurrency: 'eur', quoteCurrency: 'usd', ...args });
const ir = args => irInput.parse({ currency: 'USD', instrument: 'ir_swap', term: '5Y', ...args });
test('FX finds a complete paginated exact match with selected-party provenance and precise decimal text', async () => {
    const s = series(60);
    const catalog = Array.from({ length: 50 }, (_, n) => series(n + 1, fxUri.replace('EUR', 'GBP')));
    catalog.push({ ...series(58), party_id: other }, s);
    const { lookup, calls } = fixture(catalog, [point(s, '1.0552000000000000001')]);
    const r = await lookup.lookup('fx', fx(), session());
    assert.equal(r.status, 'ok'); assert.equal(r.observation.value, '1.0552000000000000001');
    assert.equal(r.units, 'USD per 1 EUR'); assert.equal(r.observation.source, 'labelled.fixture');
    assert.equal(calls.filter(c => c.op === 'series').length, 2);
    assert.equal(calls[0].args.as_of, calls[1].args.as_of); assert.deepEqual(calls.at(-1).args.order, { field: '', descending: false });
});
test('provider ambiguity never auto-selects an empty or populated provider; explicit selection narrows server filter', async () => {
    const a = series(), b = series(2, fxUri, 'vendor'); const f = fixture([a, b], [point(a)]);
    const ambiguous = await f.lookup.lookup('fx', fx(), session());
    assert.equal(ambiguous.status, 'ambiguous'); assert.equal(ambiguous.candidateCount, 2);
    assert.ok(!f.calls.some(c => c.op === 'observations'));
    const result = await f.lookup.lookup('fx', fx({ provider: 'ore' }), session());
    assert.equal(result.status, 'ok'); assert.equal(f.calls.findLast(c => c.op === 'series').args.filter.provider_code, 'ore');
});
test('historical data is stale now and usable only as an explicitly dated lookup', async () => {
    const s = series(), { lookup, calls } = fixture([s], [point(s, '1.0552', '2016-12-30 00:00:00.000000Z')]);
    const stale = await lookup.lookup('fx', fx(), session()); assert.equal(stale.status, 'stale');
    assert.match(formatMarketResult(stale), /not a current market quote/); assert.match(formatMarketResult(stale), /2016-12-30/);
    const historical = await lookup.lookup('fx', fx({ asOf: '2016-12-30' }), session());
    assert.equal(historical.status, 'ok'); assert.equal(historical.historicalRequest, true);
    assert.equal(calls.at(-1).args.filter.observation_datetime_to, '2016-12-30T23:59:59.999Z');
});
test('no inversion, cross-party or cross-tenant fallback; invalid pairs and future dates are rejected', async () => {
    const s = series(); const f = fixture([{ ...s, party_id: other }, { ...series(2), tenant_id: other }, series(3, fxUri.replace('/EUR?', '/USD?').replace('ccy=USD', 'ccy=EUR'))]);
    assert.equal((await f.lookup.lookup('fx', fx(), session())).status, 'not_found');
    await assert.rejects(() => f.lookup.lookup('fx', fx({ quoteCurrency: 'EUR' }), session()), /different currencies/);
    await assert.rejects(() => f.lookup.lookup('fx', fx({ asOf: '2026-11-07' }), session()), /Future/);
});
test('IR maturity term is distinct from floating tenor; negative rates are valid and period equivalents match', async () => {
    const s = series(1, irUri);
    const prefix = irUri.replace('type=series', 'type=quote');
    const f = fixture([s], [point(s, '0.03', undefined, prefix + '&term=1D'), point(s, '-0.005', undefined, prefix + '&term=60M')]);
    const r = await f.lookup.lookup('ir', ir({ floatingTenor: '3M' }), session());
    assert.equal(r.status, 'ok'); assert.equal(r.observation.value, '-0.005'); assert.equal(r.units, 'decimal rate');
    assert.equal((await f.lookup.lookup('ir', ir({ floatingTenor: '6M' }), session())).status, 'not_found');
});
test('discount factors retain their units; missing IR maturity is not replaced by another curve point', async () => {
    const uri = 'oresmd://ir/USD?type=series&instrument=discount&quote=rate&curve_id=USD-SOFR';
    const s = series(1, uri), f = fixture([s], [point(s, '0.9', undefined, uri.replace('type=series', 'type=quote') + '&term=5Y')]);
    const r = await f.lookup.lookup('ir', ir({ instrument: 'discount', curveId: 'USD-SOFR' }), session());
    assert.equal(r.status, 'ok'); assert.equal(r.units, 'discount factor');
    assert.equal((await f.lookup.lookup('ir', ir({ instrument: 'discount', term: '7Y' }), session())).status, 'no_observation');
});
test('zero observations and duplicate latest observations cannot become a quote', async () => {
    const s = series(); assert.equal((await fixture([s]).lookup.lookup('fx', fx(), session())).status, 'no_observation');
    const duplicate = await fixture([s], [point(s), point(s, '1.2')]).lookup.lookup('fx', fx(), session());
    assert.equal(duplicate.status, 'incomplete'); assert.equal(duplicate.observation, undefined);
});
test('malformed values, timestamps, future data and wrong-party observations fail closed', async () => {
    const s = series();
    for (const bad of [point(s, 'NaN'), point(s, '0'), point(s, '1.1', 'not a date'), point(s, '1.1', '2026-11-07T00:00:00Z'), { ...point(s), party_id: other }]) {
        await assert.rejects(() => fixture([s], [bad]).lookup.lookup('fx', fx(), session()));
    }
});
test('scan ceilings produce incomplete results instead of false not-found or uniqueness claims', async () => {
    const rows = Array.from({ length: 1001 }, (_, n) => series(n + 1, fxUri.replace('EUR', 'GBP')));
    rows[0] = series(1); const f = fixture(rows);
    const result = await f.lookup.lookup('fx', fx(), session()); assert.equal(result.status, 'incomplete');
    assert.equal(f.calls.length, 20); assert.equal(result.observation, undefined);
});
test('cancellation, logout and backend denial do not produce market data', async () => {
    const s = series(), f = fixture([s], [point(s)]), controller = new AbortController(); controller.abort();
    await assert.rejects(() => f.lookup.lookup('fx', fx(), session(), controller.signal)); assert.equal(f.calls.length, 0);
    await assert.rejects(() => f.lookup.lookup('fx', fx(), { ...session(), active: false }));
    f.backend.request = async () => ({ result: { outcome: 'denied' }, total: 0, market_series: [] });
    await assert.rejects(() => f.lookup.lookup('fx', fx(), session()), /refused/);
});
test('short/inconsistent pages and observation scan ceilings cannot establish a quote', async () => {
    const s = series(), f = fixture([s]);
    f.backend.request = async () => ({ result: { outcome: 'ok' }, total: 51, market_series: [s] });
    assert.equal((await f.lookup.lookup('fx', fx(), session())).status, 'incomplete');
    const swap = series(1, irUri);
    const points = Array.from({ length: 1001 }, (_, n) => point(swap, '0.03', `2026-11-05T00:${String(Math.floor(n / 60)).padStart(2, '0')}:${String(n % 60).padStart(2, '0')}Z`, irUri.replace('type=series', 'type=quote') + '&term=1D')).reverse();
    assert.equal((await fixture([swap], points).lookup.lookup('ir', ir(), session())).status, 'incomplete');
});
