// SPDX-License-Identifier: GPL-3.0-or-later
// Independent reference vectors: Python stdlib math.exp and statistics.NormalDist.
// No imports from the agent's calculation modules or self-reported pass flags.
export function gradeFxAnalytics(id, reply) {
    const result = reply?.evidence?.[0]?.result, failures = [];
    const near = (actual, expected, label, tolerance = 1e-11) => {
        if (typeof actual !== 'number' || !Number.isFinite(actual) || Math.abs(actual - expected) > tolerance)
            failures.push(`Independent FX reference differs: ${label}.`);
    };
    if (id === 'fx-curve-exponential-carry') {
        const forwards = [1.10280631337416, 1.108167156350149, 1.1164867135448515, 1.1334999873488687];
        const discounts = [.9983028117186761, .9950806330663632, .9901312110523333, .9801986733067553];
        for (let i = 0; i < forwards.length; i++) {
            near(result?.repricing?.[i]?.outright, forwards[i], `forward ${i}`);
            near(result?.repricing?.[i]?.eurDiscount, discounts[i], `EUR discount ${i}`);
            near(result?.repricing?.[i]?.residual, 0, `residual ${i}`, 1e-8);
        }
        for (const [i, expected] of [1.1053472012284242, 1.1109029948764757, 1.1249611885208617].entries())
            near(result?.targets?.[i]?.outright, expected, ['2M', '4M', '9M'][i]);
    }
    if (id === 'fx-surface-flat-reference') {
        const nodes = {'10P':1.0209370780047404, '25P':1.0585438959151103, ATM:1.1, '25C':1.1471455930581398, '10C':1.1894013758720343};
        for (const [label, strike] of Object.entries(nodes)) {
            const node = result?.expiries?.[0]?.nodes?.find(n => n.label === label);
            near(node?.strike, strike, `${label} strike`);
            near(node?.volatility, .12, `${label} volatility`);
        }
        near(result?.grid?.[0]?.volatility, .12, 'interpolated volatility');
    }
    return failures;
}
