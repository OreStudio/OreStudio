import test from 'node:test';
import assert from 'node:assert/strict';
import { encode } from '@msgpack/msgpack';
import { gzipSync } from 'node:zlib';
import { Sessions } from '../dist/auth.js';
import { ToolExecutor } from '../dist/tools.js';
import { decodeReply } from '../dist/backend.js';

const party = '11111111-1111-4111-8111-111111111111';
function fixture() {
    const calls = [];
    const backend = { ready: () => true, close: async () => {}, request: async (op, args, s) => {
        calls.push({ op, args, token: s?.token });
        if (op === 'login') return { success: true, token: args.principal + '-backend-secret', session_id: args.principal + '-session', username: args.principal,
            tenant_id: args.principal, selected_party_id: party, available_parties: [], access_lifetime_s: 1800 };
        if (op === 'logout') return {};
        return { result: { outcome: 'ok' }, currencies: [{ iso_code: s.username === 'alice' ? 'GBP' : 'EUR', name: 'fixture currency', secret: 'omit' }], total: 1 };
    } };
    return { backend, calls };
}
test('tokens remain server-side; separate users carry their own backend identity; logout revokes', async () => {
    const { backend, calls } = fixture(); const sessions = new Sessions(backend); const executor = new ToolExecutor(backend, () => {});
    const a = await sessions.login({ principal: 'alice', password: 'secret' });
    const b = await sessions.login({ principal: 'bob', password: 'secret' });
    assert.ok(!JSON.stringify(a).includes('backend-secret'));
    const sa = sessions.require('Bearer ' + a.accessToken), sb = sessions.require('Bearer ' + b.accessToken);
    const ra = await executor.execute('list_currencies', {}, sa), rb = await executor.execute('list_currencies', {}, sb);
    assert.equal(ra.rows[0].iso_code, 'GBP'); assert.equal(rb.rows[0].iso_code, 'EUR'); assert.ok(!JSON.stringify(ra).includes('omit'));
    assert.deepEqual(calls.filter(c => c.op === 'currencies').map(c => c.token), ['alice-backend-secret', 'bob-backend-secret']);
    await assert.rejects(() => executor.execute('book_trade', {}, sa));
    await assert.rejects(() => executor.execute('list_currencies', { tenantId: 'bob' }, sa));
    await sessions.logout('Bearer ' + a.accessToken);
    assert.throws(() => sessions.require('Bearer ' + a.accessToken));
    await assert.rejects(() => executor.execute('list_currencies', {}, sa));
    assert.equal(sessions.require('Bearer ' + b.accessToken), sb);
    await sessions.close();
});
test('denied and malformed backend replies never become successful data', async () => {
    const { backend } = fixture(); const sessions = new Sessions(backend);
    const a = await sessions.login({ principal: 'alice', password: 'secret' }); const s = sessions.require('Bearer ' + a.accessToken);
    backend.request = async () => ({ result: { outcome: 'denied' }, total: 1, currencies: [{ iso_code: 'USD', name: 'hidden' }] });
    await assert.rejects(() => new ToolExecutor(backend, () => {}).execute('list_currencies', {}, s), /authorize/);
    backend.request = async () => ({ result: { outcome: 'ok' }, total: 1, currencies: [{ broken: true }] });
    await assert.rejects(() => new ToolExecutor(backend, () => {}).execute('list_currencies', {}, s));
    s.expires = 0; await assert.rejects(() => new ToolExecutor(backend, () => {}).execute('list_currencies', {}, s), /Sign in/);
});
test('wire decoder handles msgpack/gzip and rejects malformed or oversized data', () => {
    const data = { result: { outcome: 'ok' }, currencies: [] };
    assert.deepEqual(decodeReply(gzipSync(encode(data)), 'gzip'), data);
    assert.throws(() => decodeReply(new Uint8Array(2_000_001)));
    assert.throws(() => decodeReply(gzipSync(Buffer.alloc(2_000_001)), 'gzip'));
    assert.throws(() => decodeReply(Uint8Array.of(0xc1)));
});
