// SPDX-License-Identifier: GPL-3.0-or-later
import { randomUUID } from 'node:crypto';
import { z } from 'zod';
import { AppError, type Backend } from './backend.js';
import type { Session } from './auth.js';
import {
    bookForwardRequest,
    forwardTerms,
    storedForward,
    type ForwardTerms,
} from './booking-wire.js';
import { decimalText } from './decimal.js';
import { markdownTable, markdownText } from './markdown.js';
import { marketId } from './market.js';
import { bookSwapRequest, readBookedSwap, swapTerms, type SwapTerms } from './swap-booking.js';

type BookingTerms = ForwardTerms | SwapTerms;
export type BookingConfirmation = {
    token: string;
    draftId: string;
    revision: number;
    terms: BookingTerms;
};
type Attempt = {
    tradeId: string;
    terms: BookingTerms;
    activityId?: string;
    rejection?: string;
    verified?: boolean;
};
const reply = z.object({
    result: z.object({
        outcome: z.string(),
        code: z.string().optional(),
        message: z.string().max(2000).optional(),
    }),
    activity_id: marketId.nullish(),
});
const ok = z.object({ result: z.object({ outcome: z.literal('ok') }) });
const anchorReply = ok.extend({
    trade: z.object({
        id: marketId,
        tenant_id: marketId,
        party_id: marketId,
        trade_type: z.enum(['FxForward', 'Swap']),
        origin: z.literal('api'),
        origin_reference: z.string(),
        booking_nature: z.literal('actual'),
        counterparty_scope: z.literal('external'),
        counterparty_id: marketId,
    }),
});
const bookingReply = ok.extend({
    trade_booking: z.object({
        trade_id: marketId,
        tenant_id: marketId,
        book_id: marketId,
        trade_activity_id: marketId,
        trade_date: z.iso.date(),
    }),
});
const forwardReply = ok.extend({ fx_forward_instrument: storedForward });
const stateReply = ok.extend({
    trade_state: z.object({ trade_id: marketId, tenant_id: marketId, status_id: marketId }),
});
const stateNameReply = ok.extend({
    fsm_states: z.array(z.object({ id: marketId, name: z.string().min(1).max(200) })).max(1),
});
const preWriteErrors = new Set([
    'session_expired',
    'unauthorized',
    'backend_denied',
    'backend_bad_request',
]);
const businessRefusals = new Set([
    'book_not_open',
    'product_type_not_allowed',
    'instrument_not_routed',
    'instrument_child_without_header',
    'instrument_not_supported',
]);

function active(session: Session, signal: AbortSignal) {
    signal.throwIfAborted();
    if (!session.active || session.expires <= Date.now())
        throw new AppError('session_expired', 'Sign in again.', 401);
}

export class TradeBookings {
    private attempts = new WeakMap<Session, Map<string, Attempt>>();
    constructor(private backend: Pick<Backend, 'request'>) {}

    async previous(token: string, session: Session, signal: AbortSignal) {
        const attempt = this.attempts.get(session)?.get(token);
        if (attempt) return this.reconcile(attempt, session, signal);
    }

    async submit(confirmation: BookingConfirmation, session: Session, signal: AbortSignal) {
        active(session, signal);
        const input = confirmation.terms;
        const terms = 'kind' in input ? swapTerms.parse(input) : forwardTerms.parse(input);
        let attempts = this.attempts.get(session);
        if (!attempts) this.attempts.set(session, (attempts = new Map()));
        const existing =
            attempts.get(confirmation.token) ??
            [...attempts.values()].find(
                (attempt) =>
                    !attempt.rejection &&
                    !attempt.verified &&
                    JSON.stringify(attempt.terms) === JSON.stringify(terms),
            );
        if (existing) {
            attempts.set(confirmation.token, existing);
            return this.reconcile(existing, session, signal);
        }

        const attempt: Attempt = { tradeId: randomUUID(), terms };
        attempts.set(confirmation.token, attempt);
        const build =
            'kind' in terms
                ? bookSwapRequest(terms, attempt.tradeId, session.partyId, new Date().toISOString())
                : bookForwardRequest(
                      terms,
                      attempt.tradeId,
                      session.partyId,
                      new Date().toISOString(),
                  );
        try {
            const result = reply.parse(
                await this.backend.request('bookTrade', build, session, signal),
            );
            const { outcome, code, message } = result.result;
            if (outcome === 'ok' && result.activity_id) attempt.activityId = result.activity_id;
            if (
                outcome === 'denied' ||
                outcome === 'invalid' ||
                (outcome === 'failed' && code && businessRefusals.has(code))
            )
                attempt.rejection = message || 'OreStudio refused the booking.';
        } catch (error) {
            if (error instanceof AppError && preWriteErrors.has(error.code))
                attempt.rejection = error.message;
        }
        return this.reconcile(attempt, session, signal);
    }

    private async reconcile(attempt: Attempt, session: Session, signal: AbortSignal) {
        active(session, signal);
        const { tradeId, terms } = attempt;
        const isSwap = 'kind' in terms;
        const evidence = (status: string, fields = {}) => [
            {
                tool: isSwap ? 'book_interest_rate_swap' : 'book_fx_forward',
                status,
                tradeId,
                ...fields,
            },
        ];
        if (attempt.rejection)
            return {
                answer: markdownText('Booking declined. ' + attempt.rejection),
                evidence: evidence('rejected'),
            };
        try {
            const { trade } = anchorReply.parse(
                await this.backend.request('trade', { key: { id: tradeId } }, session, signal),
            );
            if (
                trade.id !== tradeId ||
                trade.tenant_id !== session.tenantId ||
                trade.party_id !== session.partyId ||
                trade.trade_type !== (isSwap ? 'Swap' : 'FxForward') ||
                trade.origin_reference !== tradeId ||
                trade.counterparty_id !== terms.counterpartyId
            )
                throw new AppError(
                    'booking_mismatch',
                    'The stored trade does not match the confirmed draft.',
                );
            const key = { key: { trade_id: tradeId } };
            const [bookingRaw, stateRaw] = await Promise.all([
                this.backend.request('tradeBooking', key, session, signal),
                this.backend.request('tradeState', key, session, signal),
            ]);
            const { trade_booking: booking } = bookingReply.parse(bookingRaw);
            const { trade_state: state } = stateReply.parse(stateRaw);
            if (
                booking.tenant_id !== session.tenantId ||
                booking.trade_id !== tradeId ||
                booking.book_id !== terms.bookId ||
                booking.trade_date !== terms.tradeDate ||
                (attempt.activityId && attempt.activityId !== booking.trade_activity_id) ||
                state.tenant_id !== session.tenantId ||
                state.trade_id !== tradeId
            )
                throw new AppError(
                    'booking_mismatch',
                    'The stored trade does not match the confirmed draft.',
                );
            const summary = isSwap
                ? await readBookedSwap(
                      this.backend,
                      terms,
                      tradeId,
                      booking.trade_activity_id,
                      session,
                      signal,
                  )
                : await this.readForward(
                      terms,
                      tradeId,
                      booking.trade_activity_id,
                      session,
                      signal,
                  );

            let statusName: string | undefined;
            try {
                const raw = await this.backend.request(
                    'tradeStatusNames',
                    {
                        offset: 0,
                        limit: 1,
                        order: { field: '', descending: false },
                        filter: { id_one_of: [state.status_id] },
                        as_of: null,
                    },
                    session,
                    signal,
                );
                statusName = stateNameReply
                    .parse(raw)
                    .fsm_states.find((row) => row.id === state.status_id)?.name;
            } catch {
                // A scoped trade-state read is sufficient when the status name is unavailable.
            }
            active(session, signal);
            attempt.verified = true;
            return {
                answer: [
                    `${isSwap ? 'Interest rate swap' : 'FX forward'} booked as \`${tradeId}\`.`,
                    markdownText(
                        statusName
                            ? `OreStudio status is ${statusName}.`
                            : `OreStudio status ID is ${state.status_id}.`,
                    ),
                    ...summary,
                    `Trade date **${booking.trade_date}**.`,
                    `Book \`${booking.book_id}\`. Counterparty \`${trade.counterparty_id}\`.`,
                ].join('\n\n'),
                evidence: evidence('booked', {
                    activityId: booking.trade_activity_id,
                    statusId: state.status_id,
                    statusName,
                    terms,
                }),
            };
        } catch (error) {
            active(session, signal);
            const mismatched = error instanceof AppError && error.code === 'booking_mismatch';
            const status = mismatched ? 'readback_mismatch' : 'verification_pending';
            return {
                answer: [
                    mismatched
                        ? 'OreStudio returned terms that differ from the confirmed draft.'
                        : attempt.activityId
                          ? 'OreStudio acknowledged the booking. Read-back is still pending.'
                          : 'The booking outcome is still unverified.',
                    `Trade ID \`${tradeId}\`. Send the same confirmation again to check the result.`,
                ].join('\n\n'),
                evidence: evidence(status),
            };
        }
    }

    private async readForward(
        terms: ForwardTerms,
        tradeId: string,
        activityId: string,
        session: Session,
        signal: AbortSignal,
    ) {
        const { fx_forward_instrument: forward } = forwardReply.parse(
            await this.backend.request(
                'fxForward',
                { key: { trade_id: tradeId } },
                session,
                signal,
            ),
        );
        if (
            forward.identity.tenant_id !== session.tenantId ||
            forward.identity.party_id !== session.partyId ||
            forward.identity.trade_id !== tradeId ||
            forward.identity.trade_activity_id !== activityId ||
            Object.entries(terms.instrument).some(([field, expected]) => {
                const value = forward[field as keyof ForwardTerms['instrument']];
                return field === 'bought_amount' || field === 'sold_amount'
                    ? typeof value !== 'string' || decimalText(value) !== expected
                    : value !== expected;
            })
        )
            throw new AppError(
                'booking_mismatch',
                'The stored FX forward does not match the confirmed draft.',
            );
        return [
            markdownTable(
                ['Leg', 'Currency', 'Amount'],
                [
                    ['Buy', forward.bought_currency, decimalText(forward.bought_amount)],
                    ['Sell', forward.sold_currency, decimalText(forward.sold_amount)],
                ],
            ),
            `Value date **${forward.value_date}**. Settlement is **${forward.settlement}**.`,
            ...(forward.settlement === 'Cash'
                ? [
                      markdownText(
                          `Cash settlement in ${forward.settlement_currency} on ${forward.settlement_date} using ${forward.settlement_fx_index}.`,
                      ),
                  ]
                : []),
        ];
    }
}
