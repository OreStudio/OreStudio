// SPDX-License-Identifier: GPL-3.0-or-later
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import test from 'node:test';
import { TradeBookings } from '../dist/trade-bookings.js';
import { forwardTerms } from '../dist/booking-wire.js';
import { fixtureSession, id } from '../evals/fixtures.mjs';
import { bookingBackend } from './forward-booking-fixture.mjs';

const terms = forwardTerms.parse({
    bookId: id(3), counterpartyId: id(60), tradeDate: '2026-11-11',
    instrument: {
        bought_currency: 'USD', bought_amount: '10835000',
        sold_currency: 'EUR', sold_amount: '10000000',
        value_date: '2027-05-13', settlement: 'Cash', settlement_currency: 'USD',
        settlement_fx_index: 'FX-ECB-EUR-USD', settlement_date: '2027-05-13',
    },
});
const review = () => ({ token: randomUUID(), draftId: randomUUID(), revision: 1, terms });
const status = result => result.evidence[0].status;

test('lost booking response is reconciled against stored terms with one submission', async () => {
    const fixture = bookingBackend('timeout');
    const service = new TradeBookings(fixture.backend);
    const session = fixtureSession();
    const signal = new AbortController().signal;
    const confirmation = review();
    const first = await service.submit(confirmation, session, signal);
    assert.equal(status(first), 'verification_pending');
    assert.equal(await service.previous(confirmation.token, fixtureSession(), signal), undefined);
    const secondReview = review();
    assert.equal(status(await service.submit(secondReview, session, signal)), 'verification_pending');
    fixture.makeVisible();
    const checked = await service.previous(confirmation.token, session, signal);
    assert.equal(status(checked), 'booked');
    assert.equal(checked.evidence[0].tradeId, first.evidence[0].tradeId);
    assert.match(checked.answer, /Cash settlement in USD on 2027-05-13/);
    assert.equal((await service.previous(secondReview.token, session, signal)).evidence[0].tradeId, first.evidence[0].tradeId);
    assert.equal(fixture.writes.length, 1);
});

for (const [mode, expected] of [['denied', 'rejected'], ['duplicate', 'verification_pending'], ['mismatch', 'readback_mismatch']]) {
    test(`booking fixture ${mode} remains ${expected} on repeated confirmation`, async () => {
        const fixture = bookingBackend(mode);
        const service = new TradeBookings(fixture.backend);
        const session = fixtureSession();
        const signal = new AbortController().signal;
        const confirmation = review();
        const response = await service.submit(confirmation, session, signal);
        assert.equal(status(response), expected);
        if (mode === 'denied') assert.match(response.answer, /Book is closed/);
        assert.equal(status(await service.previous(confirmation.token, session, signal)), expected);
        assert.equal(fixture.writes.length, 1);
    });
}

test('expired sessions and out-of-range amounts never reach booking', async () => {
    const fixture = bookingBackend();
    const service = new TradeBookings(fixture.backend);
    const signal = new AbortController().signal;
    const session = fixtureSession();
    const expired = { ...session, expires: 0 };
    await assert.rejects(service.submit(review(), expired, signal), { code: 'session_expired' });
    const invalid = { ...review(), terms: { ...terms, instrument: { ...terms.instrument, bought_amount: '0.0000000000001' } } };
    await assert.rejects(service.submit(invalid, session, signal));
    assert.equal(fixture.writes.length, 0);
});
