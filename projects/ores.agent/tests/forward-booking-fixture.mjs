// SPDX-License-Identifier: GPL-3.0-or-later
import assert from 'node:assert/strict';
import { AppError } from '../dist/backend.js';
import { id } from '../evals/fixtures.mjs';

export function bookingBackend(mode = 'ok') {
    const writes = [];
    let stored;
    let visible = mode !== 'timeout';
    const activityId = id(70);
    const statusId = id(71);
    const ok = { outcome: 'ok' };
    const backend = { async request(op, payload, session, signal) {
        signal?.throwIfAborted();
        if (op === 'bookTrade') {
            assert.equal(payload.anchor.party_id, session.partyId);
            assert.equal(payload.activity_type_code, 'new_booking');
            assert.equal(payload.anchor.booking_nature, 'actual');
            assert.equal(payload.instruments.fx_forward_instruments.length, 1);
            writes.push(payload);
            if (mode === 'denied') return { result: { outcome: 'failed', code: 'book_not_open', message: 'Book is closed.' } };
            if (mode !== 'duplicate') stored = payload;
            if (mode === 'timeout') throw new AppError('backend_unavailable', 'Fixture lost the reply.');
            if (mode === 'duplicate') return { result: { outcome: 'conflict', code: 'already_exists' } };
            return { result: ok, activity_id: activityId };
        }
        const tradeId = stored?.anchor.id;
        if (!stored || !visible) return { result: { outcome: 'missing' } };
        if (op === 'tradeStatusNames') {
            assert.deepEqual(payload.filter, { id_one_of: [statusId] });
            return { result: ok, fsm_states: [{ id: statusId, name: 'Awaiting authorisation' }] };
        }
        assert.equal(payload.key.id ?? payload.key.trade_id, tradeId);
        if (op === 'trade') return { result: ok, trade: { ...stored.anchor, tenant_id: session.tenantId } };
        if (op === 'tradeBooking') return { result: ok, trade_booking: {
            ...stored.booking, tenant_id: session.tenantId, trade_activity_id: activityId,
        } };
        if (op === 'tradeState') return { result: ok, trade_state: {
            tenant_id: session.tenantId, trade_id: tradeId, status_id: statusId,
        } };
        assert.equal(op, 'fxForward');
        const forward = stored.instruments.fx_forward_instruments[0];
        return { result: ok, fx_forward_instrument: {
            ...forward, bought_amount: mode === 'mismatch' ? '1' : forward.bought_amount + '.0000',
            identity: { ...forward.identity, tenant_id: session.tenantId, trade_activity_id: activityId },
        } };
    } };
    return { backend, writes, makeVisible() { visible = true; } };
}
