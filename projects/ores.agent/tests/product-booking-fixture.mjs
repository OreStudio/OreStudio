// SPDX-License-Identifier: GPL-3.0-or-later
import assert from 'node:assert/strict';
import { swapChildReads } from '../dist/swap-booking.js';
import { id } from '../evals/fixtures.mjs';

export const swapFields = {
    fixedDirection: 'pay', floatDirection: 'receive',
    notional: '100M', currency: 'USD', floatingIndex: 'SOFR',
    fixedRate: '4.25%', spread: '-5 bps',
    startDate: '2026-11-13', maturityDate: '2031-11-13', tradeDate: '2026-11-11',
    fixedFrequency: '1Y', floatFrequency: '6M', fixedDayCount: 'A360', floatDayCount: 'A360',
    calendar: 'US', adjustment: 'ModifiedFollowing', maturityAdjustment: 'ModifiedFollowing',
    scheduleRule: 'Forward', endOfMonth: 'no', fixedPaymentLag: '2', floatPaymentLag: '2',
    fixingDays: '0', fixingCalendar: 'US', book: 'Fixture FX Book', counterparty: 'FIX-A',
};
export const swapBookingTerms = {
    ...swapFields, kind: 'swap', fixedPays: true, notional: '100000000',
    fixedRate: '0.0425', spread: '-0.0005', fixedFrequency: 'Annual', floatFrequency: 'Semiannual',
    endOfMonth: 'false', bookId: id(3), counterpartyId: id(60),
};

export function swapBackend({ mismatch = false } = {}) {
    const writes = [];
    let request;
    const activity = id(70);
    const result = { outcome: 'ok' };
    const backend = { async request(op, payload, session, signal) {
        signal?.throwIfAborted();
        if (op === 'bondIssue') {
            assert.equal(payload.key.issue_id, id(80));
            return { result, bond_issue: { issue_id: id(80), tenant_id: session.tenantId,
                security_id: 'LABELLED-BOND-2032', issuer: 'Fixture Issuer' } };
        }
        if (op === 'bookTrade') {
            assert.equal(payload.anchor.trade_type, 'Swap');
            assert.equal(payload.anchor.party_id, session.partyId);
            assert.equal(payload.activity_type_code, 'new_booking');
            writes.push(payload);
            request = payload;
            return { result, activity_id: activity };
        }
        const tradeId = request?.anchor.id;
        if (!request) return { result: { outcome: 'missing' } };
        if (op === 'tradeStatusNames') return { result, fsm_states: [{ id: id(71), name: 'Awaiting authorisation' }] };
        assert.equal(payload.key?.id ?? payload.key?.trade_id ?? payload.trade_id, tradeId);
        const scope = { tenant_id: session.tenantId, trade_activity_id: activity };
        if (op === 'trade') return { result, trade: { ...request.anchor, tenant_id: session.tenantId } };
        if (op === 'tradeBooking') return { result, trade_booking: { ...request.booking, ...scope } };
        if (op === 'tradeState') return { result, trade_state: { trade_id: tradeId, ...scope, status_id: id(71) } };
        const scoped = row => row.identity ? { ...row, identity: { ...row.identity, ...scope } } : { ...row, ...scope };
        if (op === 'swapHeader') return { result, rate_instrument: scoped(request.instruments.rate_instruments[0]) };
        const key = swapChildReads[op];
        assert.ok(key, op);
        const rows = request.instruments[key].map(scoped).reverse();
        if (mismatch && op === 'swapSchedules') rows[0].end_date = '2031-11-14';
        return { result, total: rows.length, [key]: rows };
    } };
    return { backend, writes };
}
