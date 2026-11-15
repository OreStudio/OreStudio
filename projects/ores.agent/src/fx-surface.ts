// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { buildCurve, curveInput, positive, quoteValue, toleranceSchema } from './fx-curve.js';
import { bisect, finite, normalCdf, normalInverse, smileSpline, years } from './fx-numerics.js';

export const surfaceInput = z
    .object({
        curve: curveInput,
        inputReference: z.string().min(1).max(200),
        conventions: z
            .object({
                delta: z.enum([
                    'spot',
                    'forward',
                    'premium_adjusted_spot',
                    'premium_adjusted_forward',
                ]),
                atm: z.enum(['forward', 'spot', 'delta_neutral']),
                butterfly: z.enum(['smile', 'broker']),
                riskReversal: z.literal('call_minus_put'),
                volatilityUnits: z.literal('decimal'),
                premiumCurrency: z.literal('USD'),
                dayCount: z.literal('ACT/365F'),
                wingExtrapolation: z.enum(['reject', 'flat_volatility']),
            })
            .strict(),
        quotes: z
            .array(
                z
                    .object({
                        expiryDate: z.iso.date(),
                        deliveryDate: z.iso.date(),
                        atm: quoteValue,
                        rr10: quoteValue,
                        bf10: quoteValue,
                        rr25: quoteValue,
                        bf25: quoteValue,
                    })
                    .strict(),
            )
            .min(1)
            .max(8),
        grid: z
            .array(
                z
                    .object({
                        expiryDate: z.iso.date(),
                        deliveryDate: z.iso.date(),
                        strike: z.number().finite().positive(),
                    })
                    .strict(),
            )
            .max(24)
            .default([]),
        tolerance: toleranceSchema,
    })
    .strict();
type SurfaceInput = z.infer<typeof surfaceInput>;
type Convention = SurfaceInput['conventions'];
type Market = {
    forward: number;
    foreignDiscount: number;
    domesticDiscount: number;
    time: number;
    spot: number;
};
type Node = { label: string; strike: number; volatility: number };

export function blackPrice(
    forward: number,
    strike: number,
    vol: number,
    time: number,
    discount: number,
    call: boolean,
): number {
    const s = vol * Math.sqrt(time),
        d1 = Math.log(forward / strike) / s + s / 2,
        d2 = d1 - s;
    return (
        discount *
        (call
            ? forward * normalCdf(d1) - strike * normalCdf(d2)
            : strike * normalCdf(-d2) - forward * normalCdf(-d1))
    );
}

export function fxDelta(
    m: Market,
    strike: number,
    vol: number,
    c: Convention,
    call: boolean,
): number {
    const s = vol * Math.sqrt(m.time),
        d1 = Math.log(m.forward / strike) / s + s / 2;
    const sign = call ? 1 : -1,
        scale = c.delta.endsWith('spot') ? m.foreignDiscount : 1;
    return (
        sign *
        scale *
        (c.delta.startsWith('premium_adjusted')
            ? (strike / m.forward) * normalCdf(sign * (d1 - s))
            : normalCdf(sign * d1))
    );
}

export function deltaStrike(
    m: Market,
    vol: number,
    delta: number,
    c: Convention,
    call: boolean,
): number {
    positive(vol, 'Wing volatility');
    const scale = c.delta.endsWith('spot') ? m.foreignDiscount : 1;
    if (!(delta / scale > 0 && delta / scale < 1))
        throw new Error('Delta is unattainable with these discount factors.');
    const s = vol * Math.sqrt(m.time);
    if (!c.delta.startsWith('premium_adjusted')) {
        const d1 = (call ? 1 : -1) * normalInverse(delta / scale);
        return positive(m.forward * Math.exp(-d1 * s + (s * s) / 2), 'Delta strike');
    }
    // The premium-adjusted call delta is not globally monotone. Select only the
    // OTM branch and reject a target outside it instead of silently taking an ITM root.
    const fn = (logStrike: number) =>
        Math.abs(fxDelta(m, m.forward * Math.exp(logStrike), vol, c, call)) - delta;
    const bound = Math.max(2, 12 * s + s * s);
    const root = call ? bisect(fn, 0, bound) : bisect(fn, -bound, 0);
    return positive(m.forward * Math.exp(root), 'Premium-adjusted strike');
}

function atmStrike(m: Market, vol: number, c: Convention): number {
    if (c.atm === 'spot') return m.spot;
    if (c.atm === 'forward') return m.forward;
    return (
        m.forward *
        Math.exp(((c.delta.startsWith('premium_adjusted') ? -1 : 1) * vol * vol * m.time) / 2)
    );
}

function makeSmile(m: Market, atm: number, rr: number[], bf: number[], c: Convention) {
    const nodes: Node[] = [{ label: 'ATM', strike: atmStrike(m, atm, c), volatility: atm }];
    for (let i = 0; i < 2; i++) {
        const delta = [0.1, 0.25][i]!;
        for (const call of [false, true]) {
            const vol = positive(atm + bf[i]! + ((call ? 1 : -1) * rr[i]!) / 2, 'Wing volatility');
            nodes.push({
                label: `${delta * 100}${call ? 'C' : 'P'}`,
                strike: deltaStrike(m, vol, delta, c, call),
                volatility: vol,
            });
        }
    }
    nodes.sort((a, b) => a.strike - b.strike);
    const expected = ['10P', '25P', 'ATM', '25C', '10C'];
    if (nodes.some((n, i) => n.label !== expected[i]))
        throw new Error('ATM and wing conventions produce overlapping or inverted strike nodes.');
    const knots = nodes.map((n) => ({
        x: Math.log(n.strike / m.forward),
        y: n.volatility * n.volatility * m.time,
    }));
    const spline = smileSpline(knots);
    const variance = (strike: number, allowWing: boolean) => {
        const outside = strike < nodes[0]!.strike || strike > nodes.at(-1)!.strike;
        if (outside) {
            if (!allowWing)
                throw new Error('Strike is outside quoted wings; extrapolation is disabled.');
            const node = strike < nodes[0]!.strike ? nodes[0]! : nodes.at(-1)!;
            return node.volatility * node.volatility * m.time;
        }
        return positive(spline(Math.log(strike / m.forward)), 'Interpolated total variance');
    };
    return { nodes, variance };
}

function strangle(
    m: Market,
    sigma: number,
    c: Convention,
    delta: number,
    value: (strike: number) => number,
): number {
    return [false, true].reduce((sum, call) => {
        const strike = deltaStrike(m, sigma, delta, c, call);
        return sum + blackPrice(m.forward, strike, value(strike), m.time, m.domesticDiscount, call);
    }, 0);
}

function fitSmile(m: Market, atm: number, rr: number[], bf: number[], c: Convention) {
    const adjusted = [...bf];
    let iterations = 0;
    const residuals = (values: number[]) => {
        const smile = makeSmile(m, atm, rr, values, c);
        return [0.1, 0.25].map((d, i) => {
            const sigma = positive(atm + bf[i]!, 'Broker strangle volatility');
            const market = strangle(m, sigma, c, d, () => sigma);
            const fit = strangle(m, sigma, c, d, (k) =>
                Math.sqrt(smile.variance(k, c.wingExtrapolation === 'flat_volatility') / m.time),
            );
            return (fit - market) / m.domesticDiscount / m.forward;
        });
    };
    if (c.butterfly === 'broker') {
        let converged = false;
        for (; iterations < 40; iterations++) {
            const r = residuals(adjusted),
                norm = Math.max(...r.map(Math.abs));
            if (norm < 1e-13) {
                converged = true;
                break;
            }
            const h = 1e-6;
            const a = residuals([adjusted[0]! + h, adjusted[1]!]);
            const b = residuals([adjusted[0]!, adjusted[1]! + h]);
            const j00 = (a[0]! - r[0]!) / h,
                j10 = (a[1]! - r[1]!) / h;
            const j01 = (b[0]! - r[0]!) / h,
                j11 = (b[1]! - r[1]!) / h;
            const determinant = j00 * j11 - j01 * j10;
            if (Math.abs(determinant) < 1e-15)
                throw new Error('Broker butterfly calibration has a singular Jacobian.');
            const dx = (-r[0]! * j11 + r[1]! * j01) / determinant;
            const dy = (-r[1]! * j00 + r[0]! * j10) / determinant;
            let accepted = false;
            for (let step = 1; step >= 1 / 1024; step /= 2) {
                const next = [adjusted[0]! + step * dx, adjusted[1]! + step * dy];
                try {
                    if (Math.max(...residuals(next).map(Math.abs)) < norm) {
                        adjusted.splice(0, 2, ...next);
                        accepted = true;
                        break;
                    }
                } catch {
                    /* Reject inadmissible trial smile; backtrack. */
                }
            }
            if (!accepted) throw new Error('Broker butterfly calibration did not make progress.');
        }
        if (!converged) throw new Error('Broker butterfly calibration did not converge.');
    }
    const smile = makeSmile(m, atm, rr, adjusted, c);
    const volAt = (k: number) =>
        Math.sqrt(smile.variance(k, c.wingExtrapolation === 'flat_volatility') / m.time);
    const recoveredBF = bf.map((v, i) => {
        if (c.butterfly === 'smile') return adjusted[i]!;
        const sigma = atm + v,
            delta = [0.1, 0.25][i]!;
        // Recover the market-strangle quote in volatility units, at its quoted
        // reference strikes. Re-solving the moving-strike convention also checks
        // that the calibrated smile quotes back to the supplied broker number.
        const fn = (candidate: number) =>
            strangle(m, candidate, c, delta, volAt) -
            strangle(m, candidate, c, delta, () => candidate);
        let width = Math.max(1e-7, sigma * 1e-4);
        for (let n = 0; n < 12; n++, width *= 2) {
            try {
                return bisect(fn, Math.max(1e-6, sigma - width), sigma + width) - atm;
            } catch {
                /* Expand only within the supported smile; otherwise fail. */
            }
        }
        throw new Error('Could not recover the broker butterfly quote.');
    });
    return { ...smile, iterations, adjusted, recoveredBF };
}

export function buildSurface(input: SurfaceInput) {
    const curve = buildCurve(input.curve);
    if (!curve.result.validated) throw new Error('The input FX curve failed repricing validation.');
    const c = input.conventions,
        valuation = input.curve.valuationDate;
    const market = (expiry: string, delivery: string): Market => {
        if (expiry <= valuation || delivery < expiry || delivery < input.curve.spotDate)
            throw new Error(
                'Expiry/delivery dates are inconsistent with valuation and spot settlement.',
            );
        const forward = curve.forwardAt(delivery),
            domesticDiscount = curve.usdAt(delivery);
        // Spot delta discounts from valuation, so an absolute EUR short-end
        // anchor is needed when spot settlement differs from valuation.
        if (c.delta.endsWith('spot') && valuation !== input.curve.spotDate)
            throw new Error(
                'Spot delta requires a valuation-date EUR discount anchor; the supplied curve is spot-relative. Use forward delta or supply a spot-date valuation.',
            );
        return {
            forward,
            domesticDiscount,
            foreignDiscount: curve.eurAt(delivery),
            time: years(valuation, expiry),
            spot: curve.spot,
        };
    };
    const quotes = [...input.quotes].sort((a, b) => a.expiryDate.localeCompare(b.expiryDate));
    if (new Set(quotes.map((q) => q.expiryDate)).size !== quotes.length)
        throw new Error('Duplicate volatility expiry.');
    const smiles = quotes.map((q) => {
        const m = market(q.expiryDate, q.deliveryDate),
            atm = positive(q.atm, 'ATM volatility');
        const numbers = [q.rr10, q.rr25, q.bf10, q.bf25];
        if (numbers.some((v) => typeof v !== 'number'))
            throw new Error('Unresolved volatility quote.');
        const rr = [q.rr10 as number, q.rr25 as number],
            bf = [q.bf10 as number, q.bf25 as number];
        const smile = fitSmile(m, atm, rr, bf, c);
        const byLabel = (label: string) => smile.nodes.find((n) => n.label === label)!;
        const recovered = [
            byLabel('ATM').volatility,
            byLabel('10C').volatility - byLabel('10P').volatility,
            smile.recoveredBF[0]!,
            byLabel('25C').volatility - byLabel('25P').volatility,
            smile.recoveredBF[1]!,
        ];
        const observed = [
            atm,
            q.rr10 as number,
            q.bf10 as number,
            q.rr25 as number,
            q.bf25 as number,
        ];
        const recovery = ['ATM', '10RR', '10BF', '25RR', '25BF'].map((label, i) => ({
            label,
            observed: observed[i]!,
            recovered: recovered[i]!,
            error: recovered[i]! - observed[i]!,
            passed:
                Math.abs(recovered[i]! - observed[i]!) <=
                input.tolerance.absolute + input.tolerance.relative * Math.abs(observed[i]!),
        }));
        return { ...smile, m, expiryDate: q.expiryDate, deliveryDate: q.deliveryDate, recovery };
    });
    const diagnostics: { check: string; passed: boolean; domain: string; violations: number }[] =
        [];
    for (const s of smiles) {
        const lo = s.nodes[0]!.strike,
            hi = s.nodes.at(-1)!.strike;
        const strikes = Array.from({ length: 161 }, (_, i) => lo + ((hi - lo) * i) / 160);
        let bounds = 0,
            monotonic = 0,
            convex = 0;
        const prices = strikes.map((k) => {
            const vol = Math.sqrt(s.variance(k, false) / s.m.time);
            const price = finite(
                blackPrice(s.m.forward, k, vol, s.m.time, s.m.domesticDiscount, true),
                'Diagnostic price',
            );
            const epsilon = 1e-10 * s.m.forward * s.m.domesticDiscount;
            if (
                price < s.m.domesticDiscount * Math.max(s.m.forward - k, 0) - epsilon ||
                price > s.m.domesticDiscount * s.m.forward + epsilon
            )
                bounds++;
            return price;
        });
        const step = (hi - lo) / 160,
            epsilon = 1e-8 * s.m.domesticDiscount;
        for (let i = 1; i < prices.length; i++) {
            const slope = (prices[i]! - prices[i - 1]!) / step;
            if (slope > epsilon || slope < -s.m.domesticDiscount - epsilon) monotonic++;
            if (i > 1 && slope < (prices[i - 1]! - prices[i - 2]!) / step - epsilon) convex++;
        }
        for (const [check, violations] of [
            ['price_bounds', bounds],
            ['strike_monotonicity', monotonic],
            ['butterfly_convexity', convex],
        ] as const)
            diagnostics.push({
                check,
                passed: violations === 0,
                violations,
                domain: `${s.expiryDate}: 161 strikes within [${lo}, ${hi}]`,
            });
    }
    for (let i = 1; i < smiles.length; i++) {
        const left = smiles[i - 1]!,
            right = smiles[i]!;
        const lo = Math.max(
            Math.log(left.nodes[0]!.strike / left.m.forward),
            Math.log(right.nodes[0]!.strike / right.m.forward),
        );
        const hi = Math.min(
            Math.log(left.nodes.at(-1)!.strike / left.m.forward),
            Math.log(right.nodes.at(-1)!.strike / right.m.forward),
        );
        if (lo >= hi) throw new Error('No common log-moneyness domain for calendar diagnostics.');
        let violations = 0;
        for (let j = 0; j <= 160; j++) {
            const k = lo + ((hi - lo) * j) / 160;
            if (
                right.variance(right.m.forward * Math.exp(k), false) <
                left.variance(left.m.forward * Math.exp(k), false) - 1e-10
            )
                violations++;
        }
        diagnostics.push({
            check: 'calendar_total_variance',
            passed: violations === 0,
            violations,
            domain: `${left.expiryDate} to ${right.expiryDate}: 161 common forward log-moneyness points`,
        });
    }
    const warnings = [
        'Arbitrage checks cover finite grids inside quoted wings, not a global arbitrage guarantee. Calendar checks compare total variance at fixed forward log-moneyness.',
    ];
    if (c.butterfly === 'broker')
        for (const [index, s] of smiles.entries()) {
            const q = quotes[index]!;
            for (const [delta, bf] of [
                [0.1, q.bf10],
                [0.25, q.bf25],
            ] as const) {
                for (const call of [false, true]) {
                    const k = deltaStrike(s.m, (q.atm as number) + (bf as number), delta, c, call);
                    if (k < s.nodes[0]!.strike || k > s.nodes.at(-1)!.strike)
                        warnings.push(
                            `Broker ${delta * 100}-delta quote recovery uses flat-volatility wing extrapolation at ${s.expiryDate}, strike ${k}; arbitrage diagnostics do not cover this tail.`,
                        );
                }
            }
        }
    if (smiles.length === 1)
        warnings.push('Calendar arbitrage is unmeasured: only one expiry was supplied.');
    const grid = input.grid.map((g) => {
        try {
            const m = market(g.expiryDate, g.deliveryDate),
                t = m.time;
            const exact = smiles.find((s) => s.expiryDate === g.expiryDate);
            if (exact && exact.deliveryDate !== g.deliveryDate)
                throw new Error('Delivery date differs from the quoted expiry.');
            const i = smiles.findIndex((s) => s.m.time > t);
            if (!exact && i <= 0) throw new Error('Expiry extrapolation is disabled.');
            const left = exact ?? smiles[i - 1]!,
                right = exact ?? smiles[i]!;
            const k = Math.log(g.strike / m.forward);
            const wing = [left, right].some((s) => {
                const strike = s.m.forward * Math.exp(k);
                return strike < s.nodes[0]!.strike || strike > s.nodes.at(-1)!.strike;
            });
            const a = left.variance(
                left.m.forward * Math.exp(k),
                c.wingExtrapolation === 'flat_volatility',
            );
            const b = right.variance(
                right.m.forward * Math.exp(k),
                c.wingExtrapolation === 'flat_volatility',
            );
            const variance = exact
                ? a
                : a + ((b - a) * (t - left.m.time)) / (right.m.time - left.m.time);
            const node = exact?.nodes.some((n) => n.strike === g.strike);
            if (wing)
                warnings.push(
                    `Flat-volatility wing extrapolation at ${g.expiryDate}, strike ${g.strike}.`,
                );
            return {
                ...g,
                volatility: Math.sqrt(positive(variance, 'Grid total variance') / t),
                classification: wing ? 'extrapolated' : node ? 'calibrated' : 'interpolated',
            };
        } catch (error) {
            const reason = (error as Error).message;
            warnings.push(`${g.expiryDate}, strike ${g.strike}: ${reason}`);
            return { ...g, classification: 'unavailable', reason };
        }
    });
    const quoteRecoveryPassed = smiles.every((s) => s.recovery.every((r) => r.passed));
    return {
        pair: 'EUR/USD',
        numericalSettings: {
            smile: 'natural cubic total-variance spline, natural boundaries',
            expiry: 'linear total variance at fixed forward log-moneyness',
            brokerSolver: 'damped two-variable Newton',
            maxBrokerIterations: 40,
            normalizedPriceTolerance: 1e-13,
            jacobianStep: 1e-6,
            rootSolver: 'bisection',
            maxRootIterations: 100,
            rootAbsoluteTolerance: 1e-13,
            diagnosticSamples: 161,
            priceBoundsRelativeTolerance: 1e-10,
            strikeSlopeTolerance: 1e-8,
            calendarVarianceTolerance: 1e-10,
        },
        method: 'natural cubic spline of total variance in log-strike; linear total variance in expiry at fixed forward log-moneyness',
        units: 'decimal annualized Black volatility',
        tolerance: input.tolerance,
        quoteRecoveryPassed,
        diagnosticsPassed: diagnostics.every((d) => d.passed),
        validated: quoteRecoveryPassed && diagnostics.every((d) => d.passed),
        expiries: smiles.map((s) => ({
            expiryDate: s.expiryDate,
            deliveryDate: s.deliveryDate,
            forward: s.m.forward,
            nodes: s.nodes,
            recovery: s.recovery,
            solverIterations: s.iterations,
        })),
        grid,
        gridComplete: grid.every((g) => g.classification !== 'unavailable'),
        diagnostics,
        warnings,
    };
}
