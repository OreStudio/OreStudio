// SPDX-License-Identifier: GPL-3.0-or-later
import { createHash } from 'node:crypto';
export const datasetVersion = 'agent-e2e-fx-curve-v1';
export const datasetProvider = 'agent-e2e';
export const datasetCurve = 'USD-AGENT-E2E';
const nil = '00000000-0000-0000-0000-000000000000';
// Frozen independent reference: S=1.0552; USD=5%, EUR=2%, continuous ACT/365F.
// F=S*exp((.05-.02)*t); D_USD=exp(-.05*t). Python stdlib math.exp.
const pillars = [
    ['1M', '2017-01-30', '1.057892019884012', '0.9957624286087757', '0.9983028117186761'],
    ['3M', '2017-03-30', '1.06303453034607', '0.9877469207606989', '0.9950806330663632'],
    ['6M', '2017-06-30', '1.0711032869347172', '0.9753767163648953', '0.9900769587737215'],
    ['1Y', '2017-12-30', '1.087335624227751', '0.951229424500714', '0.9801986733067553'],
];
export const expectedTargets = [1.0604165782509278, 1.0657465376367097, 1.0792332878489517];
function id(namespace, role) {
    const bytes = createHash('sha256')
        .update(`${datasetVersion}/${namespace}/${role}`)
        .digest()
        .subarray(0, 16);
    bytes[6] = (bytes[6] & 15) | 128;
    bytes[8] = (bytes[8] & 63) | 128;
    const h = bytes.toString('hex');
    return `${h.slice(0, 8)}-${h.slice(8, 12)}-${h.slice(12, 16)}-${h.slice(16, 20)}-${h.slice(20)}`;
}
export function fxDataset(tenantId, partyId) {
    if (!/^[0-9a-f-]{36}$/i.test(tenantId) || !/^[0-9a-f-]{36}$/i.test(partyId))
        throw Error('Explicit tenant and party UUIDs are required.');
    const namespace = `${tenantId}/${partyId}`;
    const series = [
        ['spot', 'oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD', 'spot'],
        ['forward', 'oresmd://fx/EUR?type=series&instrument=fx_fwd&quote=rate&ccy=USD', 'forward'],
        [
            'discount',
            `oresmd://ir/USD?type=series&instrument=discount&quote=rate&curve_id=${datasetCurve}`,
            'yield',
        ],
    ].map(([role, uri, subclass]) => ({
        role,
        id: id(namespace, role),
        party_id: partyId,
        oresmd_uri: uri,
        provider_code: datasetProvider,
        series_subclass: subclass,
        derivation_kind: 'OBSERVED',
        derivation_config_id: nil,
        derivation_config_version: 0,
    }));
    const point = (s, term, value) => ({
        id: id(namespace, `${s.role}/${term ?? 'spot'}`),
        party_id: partyId,
        series_id: s.id,
        observation_datetime: '2016-12-30T00:00:00Z',
        oresmd_uri:
            s.oresmd_uri.replace('type=series', 'type=quote') + (term ? '&term=' + term : ''),
        value,
        source: `synthetic.${datasetProvider}.${datasetVersion}`,
        feed_id: nil,
    });
    const observations = [
        point(series[0], null, '1.0552'),
        ...pillars.map((p) => point(series[1], p[0], p[2])),
        ...pillars.map((p) => point(series[2], p[0], p[3])),
    ];
    return {
        version: datasetVersion,
        tenantId,
        partyId,
        provider: {
            code: datasetProvider,
            name: 'Agent E2E synthetic test data',
            kind: 'reference',
            description: `${datasetVersion}: fixed synthetic FX curve validation inputs; not live market prices.`,
            display_order: 999,
            terms_url: '',
            terms_credit:
                'Synthetic test data; historical spot value copied from the existing 2016-12-30 reference.',
            allows_storage: true,
            credential_ref: '',
        },
        series: series.map(({ role, ...s }) => s),
        observations,
    };
}
export const datasetQuery =
    'Use EUR/USD spot and forward quotes from provider agent-e2e, and USD curve USD-AGENT-E2E, as of 2016-12-30.';
export const datasetConventions =
    'Spot date 2016-12-30; forward quotes are outrights; unadjusted dates; no end of month; USD tenors use valuation date.';
export function checkDatasetResult(evidence) {
    const failures = [],
        r = evidence?.result;
    const near = (a, b, label) => {
        if (typeof a !== 'number' || !Number.isFinite(a) || Math.abs(a - b) > 1e-10)
            failures.push(`Independent reference failed: ${label}`);
    };
    if (evidence?.status !== 'ok' || r?.validated !== true)
        failures.push('Curve was not successfully validated.');
    for (let i = 0; i < pillars.length; i++) {
        near(r?.repricing?.[i]?.outright, Number(pillars[i][2]), `forward ${pillars[i][0]}`);
        near(
            r?.repricing?.[i]?.eurDiscount,
            Number(pillars[i][4]),
            `EUR discount ${pillars[i][0]}`,
        );
        near(r?.repricing?.[i]?.residual, 0, `residual ${pillars[i][0]}`);
    }
    expectedTargets.forEach((v, i) => {
        near(r?.targets?.[i]?.outright, v, `target ${i}`);
        if (r?.targets?.[i]?.classification !== 'interpolated')
            failures.push('Target was not labelled interpolated.');
    });
    if (evidence?.reproduction?.observations?.length !== 9)
        failures.push('All nine supplied market inputs must be read from stored observations.');
    return failures;
}
