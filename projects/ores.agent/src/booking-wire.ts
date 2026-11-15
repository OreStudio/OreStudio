// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { marketId } from './market.js';

const currency = z.string().regex(/^[A-Z]{3}$/);
const amount = z
    .string()
    .regex(/^\d{1,26}(?:\.\d{1,12})?$/)
    .refine((value) => /[1-9]/.test(value));
const economics = z.object({
    bought_currency: currency,
    bought_amount: amount,
    sold_currency: currency,
    sold_amount: amount,
    value_date: z.iso.date(),
    settlement: z.enum(['Physical', 'Cash']),
    settlement_currency: currency.nullable().default(null),
    settlement_fx_index: z.string().min(1).max(200).nullable().default(null),
    settlement_date: z.iso.date().nullable().default(null),
    settlement_payment_lag: z.null().default(null),
    settlement_payment_calendar: z.null().default(null),
    settlement_payment_convention: z.null().default(null),
    far_date: z.null().default(null),
    far_bought_amount: z.null().default(null),
    far_sold_amount: z.null().default(null),
});
export const forwardTerms = z
    .object({
        bookId: marketId,
        counterpartyId: marketId,
        tradeDate: z.iso.date(),
        instrument: economics,
    })
    .superRefine(({ tradeDate, instrument }, ctx) => {
        if (
            tradeDate >= instrument.value_date ||
            instrument.bought_currency === instrument.sold_currency ||
            (instrument.settlement === 'Cash'
                ? !instrument.settlement_currency ||
                  !instrument.settlement_fx_index ||
                  !instrument.settlement_date ||
                  instrument.settlement_date < tradeDate
                : instrument.settlement_currency !== null ||
                  instrument.settlement_fx_index !== null ||
                  instrument.settlement_date !== null)
        )
            ctx.addIssue({ code: 'custom', message: 'Inconsistent FX-forward booking terms.' });
    });
export type ForwardTerms = z.infer<typeof forwardTerms>;
export const storedForward = economics.extend({
    identity: z.object({
        tenant_id: marketId,
        party_id: marketId,
        trade_id: marketId,
        trade_type_code: z.literal('FxForward'),
        trade_activity_id: marketId,
    }),
});

// The server's strict MessagePack decoder requires every instrument_batch vector.
const instrumentTables = [
    'bond_instruments',
    'commodity_instruments',
    'composite_instruments',
    // Older servers require this empty vector. Newer codecs ignore it.
    'composite_legs',
    'credit_instruments',
    'equity_accumulator_instruments',
    'equity_asian_option_instruments',
    'equity_barrier_option_instruments',
    'equity_digital_option_instruments',
    'equity_forward_instruments',
    'equity_option_instruments',
    'equity_position_instruments',
    'equity_swap_instruments',
    'equity_variance_swap_instruments',
    'fx_accumulator_instruments',
    'fx_asian_forward_instruments',
    'fx_barrier_option_instruments',
    'fx_digital_option_instruments',
    'fx_forward_instruments',
    'fx_vanilla_option_instruments',
    'fx_variance_swap_instruments',
    'rate_instruments',
    'scripted_instruments',
    'bond_issues',
    'bond_issue_call_dates',
    'bond_issue_conversion_targets',
    'bond_issue_legs',
    'bond_issue_leg_amortizations',
    'bond_issue_leg_amounts',
    'bond_issue_leg_rates',
    'bond_issue_leg_schedules',
    'bond_issue_leg_schedule_dates',
    'ascots',
    'bond_forwards',
    'bond_futures',
    'bond_legs',
    'bond_leg_amortizations',
    'bond_leg_amounts',
    'bond_leg_rates',
    'bond_options',
    'bond_repos',
    'bond_trs',
    'balance_guaranteed_swap_instruments',
    'balance_guaranteed_swap_tranches',
    'balance_guaranteed_swap_tranche_notionals',
    'basket_swap_periods',
    'callable_swap_call_dates',
    'callable_swap_instruments',
    'cap_floor_instruments',
    'commodity_basket_constituents',
    'credit_basket_constituents',
    'credit_default_swap_instruments',
    'credit_linked_swap_instruments',
    'equity_basket_constituents',
    // Older servers require this empty vector. Newer codecs ignore it.
    'equity_swap_periods',
    'equity_position_option_underlyings',
    'flexi_swap_instruments',
    'flexi_swap_lower_notionals',
    'fra_instruments',
    'index_credit_default_swap_instruments',
    'index_credit_default_swap_option_instruments',
    'inflation_swap_instruments',
    'instrument_barriers',
    'instrument_barrier_levels',
    'instrument_options',
    'instrument_option_exercise_fees',
    'instrument_option_exercise_prices',
    'instrument_option_payment_dates',
    'instrument_option_premiums',
    'instrument_schedules',
    'instrument_schedule_dates',
    'instrument_strikes',
    'instrument_underlyings',
    'knock_out_swap_instruments',
    'range_bounds',
    'risk_participation_agreement_instruments',
    'swap_legs',
    'swap_leg_amounts',
    'swap_leg_indexings',
    'swap_leg_amortizations',
    'swap_leg_rates',
    'scripted_parameters',
    'scripted_parameter_values',
    'structure_members',
    'swaption_instruments',
    'synthetic_cdo_instruments',
    'vanilla_swap_instruments',
] as const;

export function bookingEnvelope(
    terms: Pick<ForwardTerms, 'bookId' | 'counterpartyId' | 'tradeDate'>,
    tradeType: string,
    tradeId: string,
    partyId: string,
    now: string,
) {
    const serverAssigned = 'ffffffff-ffff-ffff-ffff-ffffffffffff';
    const audit = {
        modified_by: '',
        performed_by: '',
        change_reason_code: 'system.new_record',
        change_commentary: '',
        recorded_at: now,
    };
    const identity = {
        version: 0,
        tenant_id: serverAssigned,
        trade_type_code: tradeType,
        party_id: partyId,
        trade_id: tradeId,
        trade_activity_id: serverAssigned,
    };
    const request = {
        anchor: {
            version: 0,
            tenant_id: serverAssigned,
            id: tradeId,
            party_id: partyId,
            counterparty_id: terms.counterpartyId,
            trade_type: tradeType,
            counterparty_scope: 'external',
            booking_nature: 'actual',
            entry_channel: 'manual',
            origin: 'api',
            origin_reference: tradeId,
            external_version: 0,
            economic_digest: '',
            draft_reason_code: '',
            ...audit,
        },
        booking: {
            version: 0,
            tenant_id: serverAssigned,
            trade_id: tradeId,
            trade_activity_id: serverAssigned,
            book_id: terms.bookId,
            netting_set_id: null,
            counterparty_identifier_id: null,
            netting_set_identifier_id: null,
            trade_date: terms.tradeDate,
            execution_timestamp: now,
            ...audit,
        },
        activity_type_code: 'new_booking',
        draft_reason_code: '',
        identifiers: [],
        instruments: Object.fromEntries(instrumentTables.map((table) => [table, []])),
    };
    return { request, identity, audit };
}

export function bookForwardRequest(
    terms: ForwardTerms,
    tradeId: string,
    partyId: string,
    now: string,
) {
    const { request, identity, audit } = bookingEnvelope(terms, 'FxForward', tradeId, partyId, now);
    return {
        ...request,
        instruments: {
            ...request.instruments,
            fx_forward_instruments: [{ identity, ...terms.instrument, description: '', audit }],
        },
    };
}
