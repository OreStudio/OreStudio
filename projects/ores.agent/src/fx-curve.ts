// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { marketId } from './market.js';
import { finite, interpolate } from './fx-numerics.js';

export const quoteValue = z.union([
    z.number().finite(),
    z
        .object({
            seriesId: marketId,
            observedAt: z.iso.datetime(),
            source: z.string().min(1).max(200),
            uri: z.string().min(1).max(2048),
        })
        .strict(),
]);
export const toleranceSchema = z
    .object({
        absolute: z.number().finite().min(0).max(0.01),
        relative: z.number().finite().min(0).max(0.01),
    })
    .strict();
export const curveInput = z
    .object({
        valuationDate: z.iso.date(),
        spotDate: z.iso.date(),
        inputReference: z.string().min(1).max(200),
        spot: quoteValue,
        quoteConvention: z.enum(['outright', 'points']),
        pipSize: z.number().finite().positive().max(1),
        calendar: z
            .object({
                businessDayConvention: z.enum(['unadjusted', 'following', 'modified_following']),
                endOfMonth: z.boolean(),
                holidays: z.array(z.iso.date()).max(200),
            })
            .strict(),
        usdCurve: z
            .object({
                reference: z.string().min(1).max(200),
                points: z
                    .array(z.object({ date: z.iso.date(), discountFactor: quoteValue }).strict())
                    .min(2)
                    .max(32),
            })
            .strict(),
        forwards: z
            .array(
                z
                    .object({
                        tenor: z.string().regex(/^[1-9]\d?[DWMY]$/),
                        date: z.iso.date(),
                        value: quoteValue,
                    })
                    .strict(),
            )
            .min(2)
            .max(24),
        tolerance: toleranceSchema,
    })
    .strict();

export type CurveInput = z.infer<typeof curveInput>;
export function positive(value: unknown, label: string): number {
    if (typeof value !== 'number' || !Number.isFinite(value) || value <= 0)
        throw new Error(`${label} must be positive and finite.`);
    return value;
}
const stamp = (date: string) => Date.parse(date);
const iso = (date: Date) => date.toISOString().slice(0, 10);
const business = (date: Date, calendar: CurveInput['calendar']) =>
    ![0, 6].includes(date.getUTCDay()) && !calendar.holidays.includes(iso(date));

function adjust(date: Date, calendar: CurveInput['calendar']): string {
    const original = new Date(date),
        month = date.getUTCMonth();
    if (calendar.businessDayConvention !== 'unadjusted') {
        let count = 0;
        while (!business(date, calendar)) {
            date.setUTCDate(date.getUTCDate() + 1);
            if (++count > 370) throw new Error('Calendar has no reachable business day.');
        }
        if (
            calendar.businessDayConvention === 'modified_following' &&
            date.getUTCMonth() !== month
        ) {
            date.setTime(original.getTime());
            while (!business(date, calendar)) date.setUTCDate(date.getUTCDate() - 1);
        }
    }
    return iso(date);
}

export function maturity(
    spotDate: string,
    months: number,
    calendar: CurveInput['calendar'],
): string {
    const d = new Date(spotDate),
        day = d.getUTCDate();
    d.setUTCDate(1);
    d.setUTCMonth(d.getUTCMonth() + months);
    const end = new Date(d);
    end.setUTCMonth(end.getUTCMonth() + 1);
    end.setUTCDate(0);
    const spot = new Date(spotDate),
        spotEnd = new Date(spot);
    spotEnd.setUTCMonth(spotEnd.getUTCMonth() + 1, 0);
    if (calendar.businessDayConvention !== 'unadjusted')
        while (!business(spotEnd, calendar)) spotEnd.setUTCDate(spotEnd.getUTCDate() - 1);
    d.setUTCDate(
        calendar.endOfMonth && day === spotEnd.getUTCDate()
            ? end.getUTCDate()
            : Math.min(day, end.getUTCDate()),
    );
    return adjust(d, calendar);
}

function forwardDate(input: CurveInput, tenor: string): string {
    const count = Number(tenor.slice(0, -1)),
        unit = tenor.at(-1)!;
    if (unit === 'M' || unit === 'Y')
        return maturity(input.spotDate, count * (unit === 'Y' ? 12 : 1), input.calendar);
    const date = new Date(input.spotDate);
    date.setUTCDate(date.getUTCDate() + count * (unit === 'W' ? 7 : 1));
    return adjust(date, input.calendar);
}

export function buildCurve(input: CurveInput) {
    if (input.valuationDate > input.spotDate) throw new Error('Spot date precedes valuation date.');
    if (
        input.calendar.businessDayConvention !== 'unadjusted' &&
        !business(new Date(input.spotDate), input.calendar)
    )
        throw new Error('Spot settlement is not a business day in the supplied calendar.');
    const spot = positive(input.spot, 'EUR/USD spot');
    const usd = input.usdCurve.points.map((p) => ({
        x: stamp(p.date),
        y: Math.log(positive(p.discountFactor, 'USD discount factor')),
    }));
    if (usd[0]!.x !== stamp(input.valuationDate) || Math.abs(usd[0]!.y) > 1e-12)
        throw new Error('USD curve must start at valuationDate with discount factor 1.');
    for (let i = 1; i < usd.length; i++)
        if (usd[i]!.x <= usd[i - 1]!.x)
            throw new Error('USD curve dates must be strictly increasing.');
    const usdAt = (date: string) => Math.exp(interpolate(usd, stamp(date)));
    const usdSpot = usdAt(input.spotDate);
    if (
        new Set(input.forwards.map((q) => q.date)).size !== input.forwards.length ||
        new Set(input.forwards.map((q) => q.tenor)).size !== input.forwards.length
    )
        throw new Error('Duplicate forward maturity or tenor.');
    const quotes = input.forwards.map((q) => {
        if (q.date <= input.spotDate)
            throw new Error('Forward maturity must follow spot settlement.');
        if (typeof q.value !== 'number') throw new Error('Unresolved forward input.');
        const outright = positive(
            input.quoteConvention === 'points' ? spot + q.value * input.pipSize : q.value,
            'Forward outright',
        );
        if (forwardDate(input, q.tenor) !== q.date)
            throw new Error(
                `Forward ${q.tenor} date conflicts with the supplied calendar and roll convention.`,
            );
        return {
            ...q,
            outright,
            eurDiscount: positive(
                ((outright / spot) * usdAt(q.date)) / usdSpot,
                'EUR discount factor',
            ),
        };
    });
    const sorted = [...quotes].sort((a, b) => a.date.localeCompare(b.date));
    const eur = [
        { x: stamp(input.spotDate), y: 0 },
        ...sorted.map((q) => ({ x: stamp(q.date), y: Math.log(q.eurDiscount) })),
    ];
    const eurAt = (date: string) => Math.exp(interpolate(eur, stamp(date)));
    const forwardAt = (date: string) =>
        finite((spot * eurAt(date)) / (usdAt(date) / usdSpot), 'Forward');
    const within = (actual: number, expected: number) =>
        Math.abs(actual - expected) <=
        input.tolerance.absolute + input.tolerance.relative * Math.abs(expected);
    const repricing = quotes.map((q) => {
        const outright = forwardAt(q.date);
        const recovered = finite(
            input.quoteConvention === 'points' ? (outright - spot) / input.pipSize : outright,
            'Recovered quote',
        );
        return {
            tenor: q.tenor,
            date: q.date,
            outright,
            eurDiscount: q.eurDiscount,
            marketInput: q.value as number,
            recovered,
            residual: recovered - (q.value as number),
            passed: within(recovered, q.value as number),
            classification: 'quoted',
        };
    });
    const warnings =
        input.valuationDate === input.spotDate
            ? []
            : [
                  'EUR discounts are relative to spot settlement; the inputs do not determine a valuation-date EUR short-end discount factor.',
              ];
    const targets = [2, 4, 9].map((months) => {
        const date = maturity(input.spotDate, months, input.calendar),
            tenor = `${months}M`;
        try {
            return {
                tenor,
                date,
                outright: forwardAt(date),
                classification: quotes.some((q) => q.date === date) ? 'quoted' : 'interpolated',
            };
        } catch {
            warnings.push(
                `${tenor} is outside input coverage; no extrapolated forward is supplied.`,
            );
            return { tenor, date, classification: 'unavailable' };
        }
    });
    return {
        spot,
        usdAt,
        eurAt,
        forwardAt,
        result: {
            pair: 'EUR/USD',
            referenceDate: input.spotDate,
            valuationDate: input.valuationDate,
            normalization: 'spot-relative EUR discount factors',
            units: 'USD per EUR',
            repricingUnits: input.quoteConvention === 'points' ? 'forward points' : 'USD per EUR',
            interpolation: 'log-linear discount factors on actual UTC dates',
            tolerance: input.tolerance,
            repricing,
            targets,
            warnings,
            targetsComplete: targets.every((t) => t.classification !== 'unavailable'),
            validated: repricing.every((q) => q.passed),
        },
    };
}
