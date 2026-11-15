// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { AppError, type Backend } from './backend.js';
import type { Session } from './auth.js';
import { decimalText } from './decimal.js';
import { markdownText } from './markdown.js';
import { marketId } from './market.js';
import {
    swapAdjustments,
    swapCalendars,
    swapDayCounts,
    swapFrequencies,
    swapSummary,
    swapTerms,
} from './swap-booking.js';

export const swapLabels = {
    fixedDirection: 'Fixed leg direction (pay or receive)',
    floatDirection: 'Floating leg direction (pay or receive)',
    notional: 'Notional per leg',
    currency: 'Notional currency (USD)',
    floatingIndex: 'Floating index (USD-SOFR)',
    fixedRate: 'Agreed fixed rate (include % or bps)',
    spread: 'Agreed floating spread (include % or bps, even for zero)',
    startDate: 'Effective date (YYYY-MM-DD)',
    maturityDate: 'Maturity date (YYYY-MM-DD)',
    fixedFrequency: 'Fixed schedule frequency',
    floatFrequency: 'Floating schedule frequency',
    fixedDayCount: 'Fixed day count (e.g. A360)',
    floatDayCount: 'Floating day count (e.g. A360)',
    calendar: 'Shared accrual and payment calendar',
    adjustment: 'Accrual adjustment (e.g. ModifiedFollowing)',
    maturityAdjustment: 'Termination adjustment (e.g. ModifiedFollowing)',
    scheduleRule: 'Schedule generation (Forward or Backward)',
    endOfMonth: 'End of month (yes or no)',
    fixedPaymentLag: 'Fixed payment lag (business days)',
    floatPaymentLag: 'Floating payment lag (business days)',
    fixingDays: 'SOFR fixing lag (business days)',
    fixingCalendar: 'SOFR fixing calendar',
};
export const bondLabels = {
    bondName: 'Bond description (issuer, coupon and maturity)',
    issueId: 'OreStudio bond issue UUID',
    notional: 'Face amount',
    currency: 'Face amount currency',
    side: 'Direction (buy or sell)',
    bondPrice: 'Agreed price (% of par)',
    bondPriceType: 'Agreed price convention (clean or dirty)',
    settlementDate: 'Settlement date (YYYY-MM-DD)',
};
export const swapPrompt = `Interpret a plain USD fixed-versus-compounded-SOFR swap. Each field must be the exact literal value in the message. Pay/receive apply to the named leg. Do not derive effective or maturity dates from tenor. Fixed rate and spread need a literal number with % or bps. Accept a shared convention only when the user states it applies to both legs, and return the value for each matching field. Recognize common wording for payment frequencies and day counts. Fixed and floating legs use one USD notional. A bare product name is not agreement to default terms. This draft supports shared payment/accrual calendar and shared accrual/termination adjustments, with explicit lags, date rules, and frequencies. Mark action unsupported when the user requests economic terms outside this shape, including stubs, distinct payment schedules, observation shifts, payment offsets other than business days, averaging, nonstandard compounding, principal exchanges, amortisation, extra legs, fees or a non-SOFR index. Use the existing state only for context and do not repeat unstated terms.`;
export const bondPrompt = `Interpret a cash bond purchase or sale. Copy the description, issuer, coupon and maturity together when they identify the issue, and the exact OreStudio bond issue UUID when stated. Extract buy/sell, face amount and currency, trade/settlement dates, book and counterparty, and the agreed transaction price with its percent-of-par unit and clean or dirty convention. A coupon is never the transaction price. Never guess an identifier from issuer name or infer dates or price.`;

function choose(value: string, options: readonly string[]) {
    const key = (text: string) => text.toLowerCase().replace(/[ -]/g, '');
    return options.find((option) => key(option) === key(value));
}

export function normalizeProductField(field: string, value: string): string | undefined {
    switch (field) {
        case 'fixedDirection':
        case 'floatDirection':
            return choose(value, ['pay', 'receive']);
        case 'currency':
            return /^[a-z]{3}$/i.test(value) ? value.toUpperCase() : undefined;
        case 'floatingIndex':
            return /^(?:USD-)?SOFR$/i.test(value) ? 'USD-SOFR' : undefined;
        case 'fixedRate':
        case 'spread': {
            const rate = /^([+-]?\d{1,7}(?:\.\d{1,12})?)\s*(%|bps?|basis points?|decimal)$/i.exec(
                value,
            );
            if (!rate) return;
            const decimal = decimalText(
                rate[1]! +
                    (rate[2] === '%' ? 'e-2' : rate[2]!.toLowerCase() === 'decimal' ? '' : 'e-4'),
            );
            return /^-?\d+(?:\.\d{1,12})?$/.test(decimal) ? decimal + ' decimal' : undefined;
        }
        case 'startDate':
        case 'maturityDate':
            return z.iso.date().safeParse(value).success ? value : undefined;
        case 'fixedFrequency':
        case 'floatFrequency':
            return Object.entries(swapFrequencies).find(([name, tenor]) =>
                choose(value, [name, tenor]),
            )?.[0];
        case 'fixedDayCount':
        case 'floatDayCount': {
            const actual = choose(value, ['Actual/360', 'ACT/360', 'Actual/365', 'ACT/365']);
            return actual ? 'A' + actual.slice(-3) : choose(value, swapDayCounts);
        }
        case 'calendar':
        case 'fixingCalendar':
            return choose(value, swapCalendars);
        case 'adjustment':
        case 'maturityAdjustment':
            return choose(value, swapAdjustments);
        case 'scheduleRule':
            return choose(value, ['Forward', 'Backward']);
        case 'endOfMonth':
            if (/^(?:yes|true)$/i.test(value)) return 'true';
            if (/^(?:no|false)$/i.test(value)) return 'false';
            return;
        case 'fixedPaymentLag':
        case 'floatPaymentLag':
        case 'fixingDays':
            return /^(?:0|[1-9]\d?)$/.test(value) ? value : undefined;
        case 'issueId':
            return marketId.safeParse(value).success ? value.toLowerCase() : undefined;
        case 'bondPriceType':
            return choose(value, ['clean', 'dirty']);
        case 'bondPrice': {
            const price = /^(\d{1,5}(?:\.\d{1,12})?)\s*%(?:\s+of\s+par)?$/i.exec(value);
            if (price && Number(price[1]) > 0) return decimalText(price[1]!) + '%';
            return;
        }
        default:
            return value;
    }
}

export function swapReview(
    fields: Partial<Record<string, string>>,
    bookId?: string,
    counterpartyId?: string,
) {
    return swapTerms.safeParse({
        ...fields,
        kind: 'swap',
        fixedPays: fields.fixedDirection === 'pay',
        bookId,
        counterpartyId,
        fixedRate: fields.fixedRate?.replace(/ decimal$/, ''),
        spread: fields.spread?.replace(/ decimal$/, ''),
    });
}
export { swapSummary };

export async function bondIssue(
    backend: Pick<Backend, 'request'>,
    issueId: string,
    session: Session,
    signal: AbortSignal,
) {
    const response = z
        .object({
            result: z.object({ outcome: z.string() }),
            bond_issue: z
                .object({
                    issue_id: marketId,
                    tenant_id: marketId,
                    security_id: z.string(),
                    issuer: z.string(),
                })
                .nullish(),
        })
        .parse(await backend.request('bondIssue', { key: { issue_id: issueId } }, session, signal));
    if (response.result.outcome !== 'ok' || !response.bond_issue) return;
    const issue = response.bond_issue;
    if (issue.issue_id !== issueId || issue.tenant_id !== session.tenantId)
        throw new AppError('booking_mismatch', 'The bond issue is outside the selected tenant.');
    return {
        issue,
        answer: markdownText(
            `OreStudio issue ${issue.security_id} (${issue.issuer}, ${issue.issue_id}).`,
        ),
    };
}

const unsupported = [
    [
        /\brisk[ -]reversal\b/i,
        'EUR/USD risk reversal',
        'The two FX option legs require one package booking with both strikes, directions, expiry and premiums. This flow cannot confirm the package.',
    ],
    [
        /\bCDX\s*\.?\s*(?:NA\s*)?IG\b|\bCDXIG\b/i,
        'CDX IG',
        'An index CDS needs a verified index series and version, protection direction, coupon, upfront and schedule. The agent has no verified index-CDS booking adapter.',
    ],
    [
        /\b(?:CDS|credit default swap)\b/i,
        'Single-name CDS',
        'A Boeing CDS needs a verified reference entity and obligation, protection direction and contract terms. The agent has no verified single-name CDS booking adapter.',
    ],
    [
        /\bcollar\b/i,
        'SPX collar',
        'A collar needs a linked package for the underlying exposure and both option legs, including separate strikes and quantities. This flow cannot confirm the package.',
    ],
    [
        /\bequity options?\b|\bNVDA\b.*\b(?:options?|calls?|puts?)\b|\b(?:options?|calls?|puts?)\b.*\bNVDA\b/i,
        'Equity option',
        'An NVDA option needs an identified listed contract and a verified contract multiplier, strike, expiry, exercise style and premium. The agent has no verified listed-option booking adapter.',
    ],
] as const;

export function unsupportedBooking(message: string) {
    const product = unsupported.find(([pattern]) => pattern.test(message));
    return product && { product: product[1], explanation: product[2] };
}
