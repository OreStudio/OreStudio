// SPDX-License-Identifier: GPL-3.0-or-later
export const flatCurve = () => ({
    valuationDate: '2026-01-01', spotDate: '2026-01-01', inputReference: 'LABELLED FLAT FX FIXTURE',
    spot: 1.1, quoteConvention: 'outright', pipSize: 0.0001,
    calendar: { businessDayConvention: 'unadjusted', endOfMonth: false, holidays: [] },
    usdCurve: { reference: 'LABELLED USD DF=1 FIXTURE', points: [
        { date: '2026-01-01', discountFactor: 1 }, { date: '2027-01-01', discountFactor: 1 },
    ] },
    forwards: [
        { tenor: '1M', date: '2026-02-01', value: 1.1 },
        { tenor: '3M', date: '2026-04-01', value: 1.1 },
        { tenor: '6M', date: '2026-07-01', value: 1.1 },
        { tenor: '1Y', date: '2027-01-01', value: 1.1 },
    ], tolerance: { absolute: 1e-10, relative: 1e-10 },
});
export const flatSurface = () => ({
    curve: flatCurve(), inputReference: 'LABELLED BLACK 12 PERCENT FIXTURE',
    conventions: { delta: 'forward', atm: 'forward', butterfly: 'smile', riskReversal: 'call_minus_put', volatilityUnits: 'decimal', premiumCurrency: 'USD', dayCount: 'ACT/365F', wingExtrapolation: 'reject' },
    quotes: [
        { expiryDate: '2026-04-01', deliveryDate: '2026-04-01', atm: 0.12, rr10: 0, bf10: 0, rr25: 0, bf25: 0 },
        { expiryDate: '2026-07-01', deliveryDate: '2026-07-01', atm: 0.12, rr10: 0, bf10: 0, rr25: 0, bf25: 0 },
    ],
    grid: [{ expiryDate: '2026-05-01', deliveryDate: '2026-05-01', strike: 1.1 }],
    tolerance: { absolute: 1e-9, relative: 1e-9 },
});
