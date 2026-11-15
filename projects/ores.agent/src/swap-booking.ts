// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { AppError, type Backend, type Operation } from './backend.js';
import type { Session } from './auth.js';
import { bookingEnvelope } from './booking-wire.js';
import { decimalText } from './decimal.js';
import { markdownTable, markdownText } from './markdown.js';
import { marketId } from './market.js';

export const swapFrequencies = {
    Annual: '1Y',
    Semiannual: '6M',
    Quarterly: '3M',
    Bimonthly: '2M',
    Monthly: '1M',
} as const;
export const swapDayCounts = [
    'A360',
    'A365',
    'A365F',
    '30/360',
    'ActAct(ISDA)',
    'ActAct(ISMA)',
    'ActAct(AFB)',
] as const;
export const swapAdjustments = [
    'Following',
    'ModifiedFollowing',
    'Preceding',
    'ModifiedPreceding',
    'Unadjusted',
] as const;
export const swapCalendars = ['US', 'US-FED', 'US-GOV', 'US-NYSE', 'NullCalendar'] as const;
const signedDecimal = z.string().regex(/^-?\d{1,26}(?:\.\d{1,12})?$/);
const lag = z.string().regex(/^(?:0|[1-9]\d?)$/);
export const swapTerms = z
    .object({
        kind: z.literal('swap'),
        bookId: marketId,
        counterpartyId: marketId,
        tradeDate: z.iso.date(),
        startDate: z.iso.date(),
        maturityDate: z.iso.date(),
        fixedPays: z.boolean(),
        notional: signedDecimal.refine((value) => !value.startsWith('-') && /[1-9]/.test(value)),
        fixedRate: signedDecimal,
        spread: signedDecimal,
        fixedFrequency: z.enum(Object.keys(swapFrequencies) as (keyof typeof swapFrequencies)[]),
        floatFrequency: z.enum(Object.keys(swapFrequencies) as (keyof typeof swapFrequencies)[]),
        fixedDayCount: z.enum(swapDayCounts),
        floatDayCount: z.enum(swapDayCounts),
        calendar: z.enum(swapCalendars),
        adjustment: z.enum(swapAdjustments),
        maturityAdjustment: z.enum(swapAdjustments),
        scheduleRule: z.enum(['Forward', 'Backward']),
        endOfMonth: z.enum(['true', 'false']),
        fixedPaymentLag: lag,
        floatPaymentLag: lag,
        fixingDays: lag,
        fixingCalendar: z.enum(swapCalendars),
    })
    .refine((value) => value.tradeDate <= value.startDate && value.startDate < value.maturityDate, {
        message: 'Trade date must be on or before the effective date, which must precede maturity.',
    });
export type SwapTerms = z.infer<typeof swapTerms>;

export function bookSwapRequest(terms: SwapTerms, tradeId: string, partyId: string, now: string) {
    const { request, identity, audit } = bookingEnvelope(terms, 'Swap', tradeId, partyId, now);
    const { trade_type_code: _tradeType, ...legIdentity } = identity;
    const { party_id: _party, ...childIdentity } = legIdentity;
    const child = { ...childIdentity, ...audit, sequence_number: 1 };
    const legs = [
        {
            number: 1,
            payer: terms.fixedPays,
            type: 'Fixed',
            frequency: terms.fixedFrequency,
            dayCount: terms.fixedDayCount,
            lag: terms.fixedPaymentLag,
        },
        {
            number: 2,
            payer: !terms.fixedPays,
            type: 'Floating',
            frequency: terms.floatFrequency,
            dayCount: terms.floatDayCount,
            lag: terms.floatPaymentLag,
        },
    ];
    return {
        ...request,
        instruments: {
            ...request.instruments,
            rate_instruments: [
                {
                    identity,
                    start_date: terms.startDate,
                    maturity_date: terms.maturityDate,
                    description: '',
                    audit,
                },
            ],
            swap_legs: legs.map((leg) => ({
                identity: { ...legIdentity, leg_number: leg.number },
                payer: leg.payer,
                leg_type_code: leg.type,
                currency: 'USD',
                day_count_fraction_code: leg.dayCount,
                business_day_convention_code: terms.adjustment,
                payment_frequency_code: leg.frequency,
                floating_index_code: leg.number === 1 ? '' : 'USD-SOFR',
                payment_calendar: terms.calendar,
                payment_lag: leg.lag,
                notional_initial_exchange: false,
                notional_final_exchange: false,
                ...(leg.number === 2
                    ? {
                          fixing_days: Number(terms.fixingDays),
                          fixing_calendar: terms.fixingCalendar,
                          is_in_arrears: false,
                          is_averaged: false,
                          include_spread: false,
                      }
                    : {}),
                audit,
            })),
            swap_leg_amounts: legs.map((leg) => ({
                ...child,
                leg_number: leg.number,
                amount: terms.notional,
            })),
            swap_leg_rates: [
                { ...child, leg_number: 1, rate_role: 'fixed', value: terms.fixedRate },
                { ...child, leg_number: 2, rate_role: 'spread', value: terms.spread },
            ],
            instrument_schedules: legs.map((leg) => ({
                ...child,
                owner_role: 'swap',
                owner_number: leg.number,
                schedule_role: 'schedule',
                schedule_kind: 'rules',
                start_date: terms.startDate,
                end_date: terms.maturityDate,
                tenor: swapFrequencies[leg.frequency],
                calendar: terms.calendar,
                convention: terms.adjustment,
                term_convention: terms.maturityAdjustment,
                rule: terms.scheduleRule,
                end_of_month: terms.endOfMonth,
            })),
        },
    };
}

const ok = z.object({ result: z.object({ outcome: z.literal('ok') }) });
export const swapChildReads = {
    swapLegs: 'swap_legs',
    swapAmounts: 'swap_leg_amounts',
    swapRates: 'swap_leg_rates',
    swapSchedules: 'instrument_schedules',
} as const satisfies Partial<Record<Operation, string>>;
export const swapPage = (tradeId: string) => ({
    trade_id: tradeId,
    scope: 'direct',
    offset: 0,
    limit: 3,
    order: { field: '', descending: false },
    filter: null,
});
const row = z.record(z.string(), z.unknown());
const omittedAudit = new Set([
    'audit',
    'version',
    'modified_by',
    'performed_by',
    'recorded_at',
    'change_reason_code',
    'change_commentary',
    'description',
]);

// Check every submitted economic value, nested identity, and leg ordinal. SQL may pad decimal values.
function sameTerms(expected: Record<string, unknown>, stored: Record<string, unknown>): boolean {
    return Object.entries(expected).every(([field, value]) => {
        if (omittedAudit.has(field)) return true;
        const actual = stored[field];
        if (field === 'identity') return sameTerms(row.parse(value), row.parse(actual));
        if ((field === 'amount' || field === 'value') && typeof actual === 'string')
            return decimalText(actual) === value;
        return actual === value;
    });
}

export async function readBookedSwap(
    backend: Pick<Backend, 'request'>,
    terms: SwapTerms,
    tradeId: string,
    activityId: string,
    session: Session,
    signal: AbortSignal,
) {
    const expected = bookSwapRequest(terms, tradeId, session.partyId, '').instruments;
    const scoped = { tenant_id: session.tenantId, trade_activity_id: activityId };
    const mismatch = () =>
        new AppError(
            'booking_mismatch',
            'The stored swap does not match the confirmed legs and schedules.',
        );
    const { rate_instrument: header } = ok
        .extend({ rate_instrument: row })
        .parse(
            await backend.request('swapHeader', { key: { trade_id: tradeId } }, session, signal),
        );
    const wantedHeader = expected.rate_instruments[0]!;
    if (!sameTerms({ ...wantedHeader, identity: { ...wantedHeader.identity, ...scoped } }, header))
        throw mismatch();
    const childOps = Object.keys(swapChildReads) as (keyof typeof swapChildReads)[];
    await Promise.all(
        childOps.map(async (op) => {
            const key = swapChildReads[op];
            const payload = ok
                .extend({ total: z.literal(2), [key]: z.array(row).length(2) })
                .parse(await backend.request(op, swapPage(tradeId), session, signal));
            const stored = z.array(row).parse(payload[key]);
            for (const leg of expected[key]) {
                const wanted =
                    'identity' in leg
                        ? { ...leg, identity: { ...leg.identity, ...scoped } }
                        : { ...leg, ...scoped };
                if (stored.filter((actual) => sameTerms(wanted, actual)).length !== 1)
                    throw mismatch();
            }
        }),
    );
    return swapSummary(terms);
}

export function swapSummary(terms: SwapTerms) {
    return [
        markdownTable(
            [
                'Leg',
                'Direction',
                'Notional',
                'Rate (decimal)',
                'Frequency',
                'Day count',
                'Payment lag (days)',
            ],
            [
                [
                    'Fixed',
                    terms.fixedPays ? 'Pay' : 'Receive',
                    terms.notional + ' USD',
                    terms.fixedRate,
                    terms.fixedFrequency,
                    terms.fixedDayCount,
                    terms.fixedPaymentLag,
                ],
                [
                    'Floating',
                    terms.fixedPays ? 'Receive' : 'Pay',
                    terms.notional + ' USD',
                    'USD-SOFR + ' + terms.spread,
                    terms.floatFrequency,
                    terms.floatDayCount,
                    terms.floatPaymentLag,
                ],
            ],
        ),
        markdownText(
            `Effective ${terms.startDate}, maturity ${terms.maturityDate}. ${terms.scheduleRule} schedule on ${terms.calendar}, ${terms.adjustment} accrual adjustment and ${terms.maturityAdjustment} termination adjustment, end-of-month ${terms.endOfMonth}.`,
        ),
        markdownText(
            `SOFR compounds with spread added outside compounding, a ${terms.fixingDays}-day fixing lag on ${terms.fixingCalendar}, and no principal exchanges.`,
        ),
    ];
}
