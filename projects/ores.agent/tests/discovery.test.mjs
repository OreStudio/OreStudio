import test from 'node:test';
import assert from 'node:assert/strict';
import { Discovery } from '../dist/discovery.js';
const id = n => `11111111-1111-4111-8111-${String(n).padStart(12, '0')}`;
const tenant = id(1), party = id(2), other = id(3);
const entity = n => ({ id: id(n), tenant_id: tenant, party_id: party });
function fixture() {
    const calls = [], session = { tenantId: tenant, partyId: party, token: 'private', active: true, expires: Date.now() + 120000 };
    const trades = [{ ...entity(10), version: 1, trade_type: 'FxForward', counterparty_id: id(40), origin_reference: 'fixture forward', booking_nature: 'test' },
        { ...entity(11), version: 1, trade_type: 'UnknownProduct', counterparty_id: null, origin_reference: 'unsupported fixture', booking_nature: 'test' },
        { ...entity(12), party_id: other, version: 1, trade_type: 'FxForward', counterparty_id: null, origin_reference: 'secret other-party trade', booking_nature: 'test' }];
    const books = [{ ...entity(20), name: 'FX Hedges', parent_portfolio_id: id(30), functional_currency: 'GBP' }];
    const portfolios = [{ ...entity(30), name: 'Treasury', parent_portfolio_id: null, aggregation_ccy: 'GBP' }];
    const counterparties = [{ id: id(40), tenant_id: tenant, short_code: 'ALPHA', full_name: 'Alpha fixture bank', status: 'Active' }];
    const booking = { tenant_id: tenant, trade_id: id(10), book_id: id(20), trade_date: '2026-11-06', execution_timestamp: null };
    const instrument = { identity: { tenant_id: tenant, party_id: party, trade_id: id(10), trade_type_code: 'FxForward' }, bought_currency: 'EUR', bought_amount: '1000000.00000001', sold_currency: 'GBP', sold_amount: '850000', value_date: '2027-05-06', settlement: 'Cash' };
    const backend = { request: async (op, args, context) => {
        assert.equal(context, session); calls.push({ op, args });
        const ok = value => ({ result: { outcome: 'ok' }, ...value });
        if (op === 'trade') return ok({ trade: trades.find(t => t.id === args.key.id) ?? null });
        if (op === 'tradeBooking') return ok({ trade_booking: booking });
        if (op === 'tradePortfolios') return ok({ total: 0, trade_portfolios: [] });
        if (op === 'fxForward') return ok({ fx_forward_instrument: instrument });
        if (op === 'exportPortfolio') return { success: true, items: [{ anchor: trades[0] }] };
        if (op === 'counterparty') return ok({ counterparty: counterparties.find(c => c.short_code === args.key.short_code) ?? null });
        const keys = { trades: 'trades', books: 'books', portfolios: 'portfolios', counterparties: 'counterparties' };
        let rows = { trades, books, portfolios, counterparties }[op];
        if (op === 'counterparties' && args.filter?.id_one_of) rows = rows.filter(r => args.filter.id_one_of.includes(r.id));
        assert.ok(rows, 'Unknown or mutation operation: ' + op);
        return ok({ total: rows.length, [keys[op]]: rows.slice(args.offset, args.offset + args.limit) });
    } };
    return { discovery: new Discovery(backend), backend, calls, session, trades, books, portfolios, instrument, booking, counterparties };
}
test('discovery restricts selected party and keeps complete-match pagination explicit', async () => {
    const f = fixture(), r = await f.discovery.execute('find_trades', { limit: 1 }, f.session);
    assert.equal(r.totalMatches, 2); assert.equal(r.rows.length, 1); assert.ok(!JSON.stringify(r).includes('secret'));
    assert.equal((await f.discovery.execute('find_trades', { query: 'missing' }, f.session)).status, 'not_found');
    await assert.rejects(() => f.discovery.execute('find_trades', { tenantId: other }, f.session));
    await assert.rejects(() => f.discovery.execute('find_trades', {}, { ...f.session, active: false }));
});
test('ambiguous names never choose membership; exact IDs use the server membership operation', async () => {
    const f = fixture(); f.books.push({ ...f.books[0], id: id(21) });
    const r = await f.discovery.execute('find_trades', { book: 'FX Hedges' }, f.session);
    assert.equal(r.status, 'ambiguous'); assert.equal(r.candidates.length, 2); assert.ok(!f.calls.some(c => c.op === 'exportPortfolio'));
    const selected = await f.discovery.execute('find_trades', { book: id(20) }, f.session);
    assert.equal(selected.totalMatches, 1); assert.equal(f.calls.find(c => c.op === 'exportPortfolio').args.node_id, id(20));
});
test('trade detail includes validated decimal economics, booking and separate hierarchy', async () => {
    const f = fixture(), r = await f.discovery.execute('read_trade', { tradeId: id(10) }, f.session);
    assert.equal(r.economics.bought_amount, '1000000.00000001'); assert.equal(r.economicsStatus, 'available');
    assert.equal(r.book.name, 'FX Hedges'); assert.equal(r.directPortfolios.length, 0); assert.equal(r.bookingPortfolioHierarchy[0].name, 'Treasury');
    assert.equal(r.counterparty.short_code, 'ALPHA'); assert.ok(!JSON.stringify(r).includes('private'));
});
test('wrong-party trades stop before fetching economics; wrong instrument scope and changed versions fail closed', async () => {
    const f = fixture(); assert.equal((await f.discovery.execute('read_trade', { tradeId: id(12) }, f.session)).status, 'not_found');
    assert.ok(!f.calls.some(c => c.op === 'fxForward'));
    f.instrument.identity.party_id = other;
    await assert.rejects(() => f.discovery.execute('read_trade', { tradeId: id(10) }, f.session), /identity mismatch/);
    f.instrument.identity.party_id = party; const original = f.backend.request; let read = 0;
    f.backend.request = async (op, ...args) => { const r = await original(op, ...args); if (op === 'trade' && ++read === 2) return { ...r, trade: { ...r.trade, version: 2 } }; return r; };
    await assert.rejects(() => f.discovery.execute('read_trade', { tradeId: id(10) }, f.session), /changed/);
});
test('counterparty exact IDs/codes are complete while name pages remain explicitly partial', async () => {
    const f = fixture(); assert.equal((await f.discovery.execute('find_counterparties', { query: 'ALPHA' }, f.session)).matchedBy, 'exact_short_code');
    assert.equal((await f.discovery.execute('find_counterparties', { query: id(40) }, f.session)).searchComplete, true);
    f.counterparties.push({ id: id(41), tenant_id: tenant, short_code: 'BETA', full_name: 'Beta fixture bank', status: 'Active' });
    const r = await f.discovery.execute('find_counterparties', { query: 'fixture bank', limit: 1 }, f.session);
    assert.equal(r.status, 'partial'); assert.equal(r.searchComplete, false); assert.equal(r.nextOffset, 1);
});
test('malformed pages and backend permission denial never yield partial successful facts', async () => {
    const f = fixture(); f.backend.request = async () => ({ result: { outcome: 'denied' } });
    await assert.rejects(() => f.discovery.execute('find_books', {}, f.session), /authorize/);
    f.backend.request = async () => ({ result: { outcome: 'ok' }, total: 20, books: [] });
    await assert.rejects(() => f.discovery.execute('find_books', {}, f.session), /incomplete/);
});
test('unsupported products and bond references never masquerade as complete instrument economics', async () => {
    const f = fixture(), original = f.backend.request;
    f.backend.request = async (op, args, context) => op === 'tradeBooking' && args.key.trade_id === id(11) ? { result: { outcome: 'missing' } } : original(op, args, context);
    const unsupported = await f.discovery.execute('read_trade', { tradeId: id(11) }, f.session);
    assert.equal(unsupported.economicsStatus, 'unsupported_product'); assert.equal(unsupported.economics, null);
    f.trades[0].trade_type = 'Bond';
    f.backend.request = async (op, args, context) => op === 'bond' ? { result: { outcome: 'ok' }, bond_instrument: { identity: { ...f.instrument.identity, trade_type_code: 'Bond' }, issue_id: id(50), notional: '1000000' } } : original(op, args, context);
    assert.equal((await f.discovery.execute('read_trade', { tradeId: id(10) }, f.session)).economicsStatus, 'issue_reference_only');
});
