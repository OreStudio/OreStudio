// SPDX-License-Identifier: GPL-3.0-or-later
import assert from 'node:assert/strict';
import test from 'node:test';
import { Agent } from '../dist/agent.js';
import { BookingDrafts } from '../dist/booking-drafts.js';
import { TradeBookings } from '../dist/trade-bookings.js';
import { ToolExecutor } from '../dist/tools.js';
import { fixtureSession, id } from '../evals/fixtures.mjs';
import { bookingDraftFixtureBackend } from '../evals/booking-draft-fixtures.mjs';
import { swapBackend, swapFields } from './product-booking-fixture.mjs';

function fixture(options) {
    const booking = swapBackend(options);
    const discovery = bookingDraftFixtureBackend();
    const backend = { ...discovery, request(op, ...args) {
        return (['books', 'counterparty', 'counterparties'].includes(op) ? discovery : booking.backend).request(op, ...args);
    } };
    let extraction = {};
    const inference = { async complete(_messages, _tools, _signal, options) {
        const fields = Object.entries(extraction).map(([field, text]) => ({ field, text }));
        return { tool_calls: [{ function: { name: options.tool, arguments: JSON.stringify({ action: 'update', fields }) } }] };
    } };
    const executor = new ToolExecutor(backend, () => {});
    const drafts = new BookingDrafts(inference, executor, new TradeBookings(backend), backend);
    const agent = new Agent(inference, executor, undefined, undefined, drafts);
    const session = fixtureSession();
    const signal = new AbortController().signal;
    const chat = (message, fields = {}, identity = session) => {
        extraction = fields;
        return agent.chat({ message }, identity, signal);
    };
    const edit = (draft, fields) => agent.chat({ bookingDraft: {
        action: 'edit', id: draft.id, revision: draft.revision, fields,
    } }, session, signal);
    return { chat, edit, writes: booking.writes };
}

test('SOFR swap capture, revision-bound confirmation and read-back of both legs and schedules', async t => {
    const app = fixture();
    const request = 'Pay fixed, receive floating on a 100M USD five-year SOFR swap with FIX-A.';
    const opening = await app.chat(request, { fixedDirection: 'Pay', floatDirection: 'receive',
        notional: '100M', currency: 'USD', floatingIndex: 'SOFR', counterparty: 'FIX-A' });
    assert.equal(opening.bookingDraft.fields.notional, '100000000');
    assert.match(opening.answer, /effective date/);
    assert.match(opening.answer, /agreed fixed rate/);
    assert.equal(opening.bookingDraft.confirmation, undefined);
    const inconsistent = await app.edit(opening.bookingDraft, { ...swapFields, floatDirection: 'pay' });
    assert.match(inconsistent.answer, /directions must be opposite/);
    assert.equal(inconsistent.bookingDraft.confirmation, undefined);
    const invalidRate = await app.edit(inconsistent.bookingDraft, { ...swapFields, fixedRate: '4.25' });
    assert.equal(invalidRate.bookingDraft.confirmation, undefined);
    const payFixed = await app.edit(invalidRate.bookingDraft, swapFields);
    assert.equal(payFixed.bookingDraft.fields.fixedRate, '0.0425 decimal');
    assert.equal(payFixed.bookingDraft.fields.spread, '-0.0005 decimal');
    const receiveFixed = await app.edit(payFixed.bookingDraft, {
        ...payFixed.bookingDraft.fields, fixedDirection: 'receive', floatDirection: 'pay',
    });
    assert.match((await app.chat(payFixed.bookingDraft.confirmation)).answer, /expired or changed/);
    assert.match((await app.chat(receiveFixed.bookingDraft.confirmation, {}, fixtureSession())).answer, /expired or changed/);
    assert.equal(app.writes.length, 0);
    const booked = await app.chat(receiveFixed.bookingDraft.confirmation);
    assert.equal(booked.evidence[0].status, 'booked');
    assert.match(booked.answer, /Awaiting authorisation/);
    const stored = app.writes[0].instruments;
    assert.deepEqual(stored.swap_legs.map(leg => leg.payer), [false, true]);
    assert.deepEqual(stored.swap_leg_rates.map(rate => rate.value), ['0.0425', '-0.0005']);
    assert.deepEqual(stored.instrument_schedules.map(schedule => schedule.tenor), ['1Y', '6M']);
    assert.equal((await app.chat(receiveFixed.bookingDraft.confirmation)).evidence[0].tradeId, booked.evidence[0].tradeId);
    assert.equal(app.writes.length, 1);
    t.diagnostic('SWAP_TRANSCRIPT ' + JSON.stringify([
        { request, answer: opening.answer },
        { request: { bookingDraft: { action: 'edit', fields: receiveFixed.bookingDraft.fields } }, answer: receiveFixed.answer },
        { request: receiveFixed.bookingDraft.confirmation, answer: booked.answer, evidence: booked.evidence },
    ]));
});

test('swap schedule mismatch remains unverified and reconciliation never resubmits', async () => {
    const app = fixture({ mismatch: true });
    const opening = await app.chat('Draft a USD SOFR swap.');
    const review = await app.edit(opening.bookingDraft, swapFields);
    const confirmation = review.bookingDraft.confirmation;
    assert.equal((await app.chat(confirmation)).evidence[0].status, 'readback_mismatch');
    assert.equal((await app.chat(confirmation)).evidence[0].status, 'readback_mismatch');
    assert.equal(app.writes.length, 1);
});

test('unsupported swap economics cannot be dropped by editing the draft', async () => {
    const app = fixture();
    const opening = await app.chat('Book a USD SOFR swap with a lookback.');
    const review = await app.edit(opening.bookingDraft, swapFields);
    assert.match(review.answer, /outside the supported plain USD SOFR swap/);
    assert.equal(review.bookingDraft.confirmation, undefined);
    assert.equal(app.writes.length, 0);
});

test('bond coupon is not a transaction price, issue lookup is scoped and review cannot book', async () => {
    const app = fixture();
    const opening = await app.chat('Buy 10M USD Apple 3.85% 2032 bonds with FIX-A.',
        { side: 'Buy', notional: '10M', currency: 'USD', bondName: 'Apple 3.85% 2032',
            counterparty: 'FIX-A', bondPrice: '3.85%' });
    assert.match(opening.answer, /agreed price/);
    assert.equal(opening.bookingDraft.fields.bondPrice, undefined);
    const review = await app.edit(opening.bookingDraft, {
        ...opening.bookingDraft.fields, issueId: id(80), bondName: 'Fixture Issuer', tradeDate: '2026-11-11',
        settlementDate: '2026-11-12', bondPrice: '99.5% of par', bondPriceType: 'clean', book: 'Fixture FX Book',
    });
    assert.match(review.answer, /LABELLED-BOND-2032/);
    assert.match(review.answer, /no trade-level fields for direction or agreed transaction price/);
    assert.equal(review.evidence[0].status, 'booking_unavailable');
    assert.equal(review.bookingDraft.confirmation, undefined);
    assert.equal(app.writes.length, 0);
});

test('unsupported multi-leg, credit and listed products have specific responses', async () => {
    const app = fixture();
    for (const [message, explanation] of [
        ['Book an EUR/USD risk reversal, buy call strike 1.08 and sell put strike 1.05, expiry 2027-05-13.', /both strikes/],
        ['Buy protection on CDX IG series 45 at 100 bps.', /index series and version/],
        ['Sell protection on Boeing CDS at 125 bps.', /reference entity and obligation/],
        ['Book an SPX collar with 4500 put and 5500 call, expiry 2027-05-13.', /separate strikes and quantities/],
        ['Buy 50 NVDA calls at strike 160 expiring 2027-05-13 with multiplier 100.', /verified contract multiplier/],
        ['Send a VCON for this trade.', /VCON delivery is unavailable/],
        ['Quote a USD SOFR swap.', /Executable swap and bond prices are unavailable/],
    ]) {
        const response = await app.chat(message);
        assert.match(response.answer, explanation);
        assert.equal(response.bookingDraft, undefined);
    }
    assert.equal(app.writes.length, 0);
});
