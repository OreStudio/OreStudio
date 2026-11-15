// SPDX-License-Identifier: GPL-3.0-or-later
import { createHash } from 'node:crypto';
import { fxDataset } from './fx-e2e-dataset.mjs';
import { loadFxDataset } from '../scripts/fx-e2e-loader.mjs';

export function nativeMarketData(tenantId, partyId) {
    const fx = fxDataset(tenantId, partyId);
    const provider = 'agent-native-eval';
    const date = '2016-12-30';
    const curve = 'GBP-AGENT-NATIVE-EVAL';
    function id(key) {
        const b = createHash('sha256')
            .update([provider, tenantId, partyId, key].join('/'))
            .digest()
            .subarray(0, 16);
        b[6] = (b[6] & 15) | 128;
        b[8] = (b[8] & 63) | 128;
        const h = b.toString('hex');
        return [h.slice(0, 8), h.slice(8, 12), h.slice(12, 16), h.slice(16, 20), h.slice(20)].join(
            '-',
        );
    }
    const source = 'synthetic.agent-native-eval';
    const definitions = {
        spot: ['fx/EUR', 'fx_spot', 'spot', '&ccy=USD', null, '1.05'],
        swap: [
            'ir/GBP',
            'ir_swap',
            'yield',
            '&index_name=GBP-SONIA&fwd_start=1Y&tenor=3M',
            '5Y',
            '0.0375',
        ],
        deposit: ['ir/GBP', 'mm', 'yield', '&fwd_start=0D', '6M', '0.03125'],
        zero: [
            'ir/GBP',
            'zero',
            'yield',
            '&curve_id=' + curve + '&day_counter=A365',
            '6M',
            '0.035',
        ],
        discount: ['ir/GBP', 'discount', 'yield', '&curve_id=' + curve, '6M', '0.9825'],
    };
    const series = [],
        observations = [],
        ir = { date };
    for (const [name, [asset, instrument, subclass, parameters, term, value]] of Object.entries(
        definitions,
    )) {
        const template = fx.series[0];
        const seriesId = id(name);
        const uri =
            'oresmd://' +
            asset +
            '?type=series&instrument=' +
            instrument +
            '&quote=rate' +
            parameters;
        series.push({
            ...template,
            id: seriesId,
            provider_code: provider,
            series_subclass: subclass,
            oresmd_uri: uri,
        });
        observations.push({
            ...fx.observations[0],
            id: id(name + '/point'),
            series_id: seriesId,
            value,
            source,
            oresmd_uri: uri.replace('type=series', 'type=quote') + (term ? '&term=' + term : ''),
        });
        if (term) ir[name] = { id: seriesId, provider, value, curve };
    }
    return {
        ir,
        datasets: [
            fx,
            {
                ...fx,
                version: 'agent-native-eval-v1',
                series,
                observations,
                provider: {
                    ...fx.provider,
                    code: provider,
                    name: 'Agent native integration data',
                    description:
                        'Synthetic historical interest rates and alternate FX spot for native agent integration tests.',
                    terms_credit: 'Synthetic test observations.',
                },
            },
        ],
    };
}

export async function seedNativeMarket(tenantId, partyId, writer, system, tenant) {
    const { datasets } = nativeMarketData(tenantId, partyId);
    const reports = [];
    for (const dataset of datasets)
        reports.push(
            await loadFxDataset({
                tenantId,
                partyId,
                dataset,
                systemRequest: writer.requestFor(system),
                tenantRequest: writer.requestFor(tenant),
                apply: true,
            }),
        );
    return reports;
}
