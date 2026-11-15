// SPDX-License-Identifier: GPL-3.0-or-later
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { createServer } from 'node:http';
import test from 'node:test';
import { Script } from 'node:vm';
import { createApp } from '../dist/server.js';
import { BookingDrafts } from '../dist/booking-drafts.js';
import { fixtureSession, id } from '../evals/fixtures.mjs';
import { bookingDraftFixtureBackend } from '../evals/booking-draft-fixtures.mjs';
import { bookingBackend } from './forward-booking-fixture.mjs';

async function listen(t, server) {
    t.after(() => new Promise(resolve => server.close(resolve)));
    server.listen(0, '127.0.0.1');
    await once(server, 'listening');
    return 'http://127.0.0.1:' + server.address().port;
}

test('chat retains a combined amount and pair and accepts a full settlement label', async () => {
    const proposals = [
        [
            { field: 'side', text: 'buy' },
            { field: 'notional', text: '20M EUR/USD' },
            { field: 'book', text: 'Fixture FX Book' },
            { field: 'counterparty', text: 'FIX-A' },
        ],
        [
            { field: 'tradeDate', text: '2026-11-11' },
            { field: 'valueDate', text: '2027-05-13' },
            { field: 'settlement', text: 'physical settlement' },
            { field: 'rate', text: '1.0835' },
        ],
    ];
    const draft = new BookingDrafts({
        complete: async () => ({
            tool_calls: [{ function: { name: 'interpret_fx_forward_draft',
                arguments: JSON.stringify({ action: 'update', fields: proposals.shift() }) } }],
        }),
    }, {
        execute: async (name, { query }) => ({
            searchComplete: true, totalMatches: 1,
            rows: [{ id: name === 'find_books' ? id(50) : id(60), name: query }],
        }),
    });
    const session = fixtureSession();
    const signal = new AbortController().signal;
    const opening = await draft.handle('Draft a buy of 20M EUR/USD forward with FIX-A in book "Fixture FX Book".', session, signal);
    assert.equal(opening.draft.fields.baseAmount, '20000000');
    assert.equal(opening.draft.fields.pair, 'EUR/USD');
    const review = await draft.handle('Trade date 2026-11-11, agreed value date 2027-05-13, physical settlement, agreed rate 1.0835.', session, signal);
    assert.equal(review.draft.id, opening.draft.id);
    assert.equal(review.draft.fields.settlement, 'physical');
    assert.equal(review.draft.ready, true);
});

test('authenticated draft review validates edits and rejects stale or other-session revisions', async t => {
    const model = await listen(t, createServer(async (req, res) => {
        const chunks = [];
        for await (const chunk of req) chunks.push(chunk);
        const request = JSON.parse(Buffer.concat(chunks));
        const name = request.tool_choice.function.name;
        assert.equal(name, 'interpret_fx_forward_draft');
        const fields = [
            { field: 'side', text: 'Buy' },
            { field: 'baseAmount', text: '20M' },
            { field: 'pair', text: 'EUR/USD' },
            { field: 'rate', text: '1' },
        ];
        res.end(JSON.stringify({ choices: [{ message: {
            role: 'assistant',
            tool_calls: [{ id: 'draft', type: 'function', function: {
                name, arguments: JSON.stringify({ action: 'update', fields }),
            } }],
        } }] }));
    }));
    const fixture = bookingDraftFixtureBackend();
    const reads = [];
    const booking = bookingBackend();
    let loginCount = 0;
    const backend = { ...fixture, async request(op, args, ...rest) {
        if (op === 'login') {
            loginCount++;
            return { success: true, token: 'backend-token-' + loginCount, session_id: 'session-' + loginCount,
                username: 'fixture', tenant_id: id(1), selected_party_id: id(2) };
        }
        if (['bookTrade', 'trade', 'tradeBooking', 'fxForward', 'tradeState', 'tradeStatusNames'].includes(op))
            return booking.backend.request(op, args, ...rest);
        assert.ok(['books', 'counterparty'].includes(op), op);
        reads.push(op);
        return fixture.request(op, args, ...rest);
    } };
    const base = await listen(t, createApp({ allowedOrigins: [], inference: {
        baseUrl: model, model: 'fixture', timeoutMs: 5000,
    } }, backend));
    const post = async (path, data, token) => {
        const response = await fetch(base + path, { method: 'POST', body: JSON.stringify(data), headers: {
            'content-type': 'application/json', ...(token ? { authorization: 'Bearer ' + token } : {}),
        } });
        return { status: response.status, ...(await response.json()) };
    };
    const signIn = async () => {
        const result = await post('/auth/login', { principal: 'fixture', password: 'fixture' });
        assert.equal(result.status, 200);
        return result.accessToken;
    };
    const token = await signIn();
    const other = await signIn();
    const page = await (await fetch(base)).text();
    assert.match(page, /Edit FX-forward draft/);
    const script = page.match(/<script\b[^>]*>([\s\S]+)<\/script>/);
    assert.ok(script);
    assert.doesNotThrow(() => new Script(script[1]));

    const opening = await post('/chat', { message: 'Buy 20M EUR/USD forward at the agreed rate of 1.08.' }, token);
    assert.equal(opening.status, 200);
    assert.equal(opening.bookingDraft.fields.baseAmount, '20000000');
    assert.equal(opening.bookingDraft.fields.rate, undefined, 'A substring of a decimal is not an agreed rate');
    assert.equal(opening.bookingDraft.ready, false);
    const action = { action: 'edit', id: opening.bookingDraft.id, revision: opening.bookingDraft.revision, fields: {
        ...opening.bookingDraft.fields, tradeDate: '2026-11-11', valueDate: '2027-05-13', settlement: 'cash',
        rate: '1.0835', book: 'Fixture FX Book', counterparty: 'FIX-A',
    } };
    const edit = (request, session = token) => post('/chat', { bookingDraft: request }, session);
    assert.equal((await edit(action, other)).status, 409);
    assert.deepEqual(reads, []);
    const cash = await edit(action);
    assert.equal(cash.bookingDraft.ready, false);
    assert.deepEqual(cash.evidence[0].missing, ['settlementCurrency', 'fixingIndex', 'settlementDate']);
    assert.equal((await edit(action)).status, 409);
    const physical = await edit({ ...action, revision: cash.bookingDraft.revision,
        fields: { ...action.fields, settlement: 'physical' } });
    assert.equal(physical.bookingDraft.ready, true);
    assert.match(physical.answerHtml, /<td>21670000<\/td>/);
    assert.deepEqual(reads, ['books', 'counterparty', 'books', 'counterparty']);
    const say = (message, session = token) => post('/chat', { message }, session);
    assert.match((await say('Book this reviewed draft.')).answer, /Confirm booking/);
    assert.match((await say(physical.bookingDraft.confirmation, other)).answer, /expired or changed/);
    assert.equal(booking.writes.length, 0);

    const changedPair = await edit({ ...action, revision: physical.bookingDraft.revision,
        fields: { ...physical.bookingDraft.fields, pair: 'USD/EUR' } });
    assert.equal(changedPair.bookingDraft.ready, false);
    assert.equal(changedPair.bookingDraft.fields.rate, undefined);
    assert.equal(changedPair.bookingDraft.fields.baseAmount, undefined);
    assert.match((await say(physical.bookingDraft.confirmation)).answer, /expired or changed/);
    assert.equal(booking.writes.length, 0);

    const discard = { action: 'discard', id: action.id, revision: changedPair.bookingDraft.revision };
    assert.equal((await edit(discard)).bookingDraft, null);
    assert.equal((await edit(discard)).status, 409);
    const restarted = await post('/chat', { message: 'Draft a forward.' }, token);
    assert.equal((await post('/chat/reset', {}, token)).status, 200);
    assert.equal((await edit({ ...discard, id: restarted.bookingDraft.id, revision: restarted.bookingDraft.revision })).status, 409);

    const next = await say('Draft a forward.');
    const reviewAction = { ...action, id: next.bookingDraft.id, revision: next.bookingDraft.revision,
        fields: physical.bookingDraft.fields };
    const reviewed = await edit(reviewAction);
    const booked = await say(reviewed.bookingDraft.confirmation);
    assert.equal(booked.evidence[0].status, 'booked');
    assert.match(booked.answer, /Awaiting authorisation/);
    assert.equal(booked.bookingDraft, null);
    const repeated = await say(reviewed.bookingDraft.confirmation);
    assert.equal(repeated.evidence[0].tradeId, booked.evidence[0].tradeId);
    assert.equal(booking.writes.length, 1);
    t.diagnostic(JSON.stringify([
        { request: { bookingDraft: reviewAction }, answer: reviewed.answer },
        { request: reviewed.bookingDraft.confirmation, answer: booked.answer, evidence: booked.evidence },
        { request: reviewed.bookingDraft.confirmation, answer: repeated.answer, fixtureWriteCount: booking.writes.length },
    ]));
});
