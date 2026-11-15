// SPDX-License-Identifier: GPL-3.0-or-later
import { marketId } from './fx-market-fixtures.mjs';

export function datedMarketFixtureBackend(_scenario, caseId) {
    const owner = { tenant_id: marketId(1), party_id: marketId(2) };
    const series = (id, provider_code, asset, instrument, dimensions) => ({
        ...owner, id: marketId(id), provider_code, series_subclass: instrument,
        oresmd_uri: 'oresmd://' + asset + '?type=series&quote=rate&instrument=' + instrument + dimensions,
    });
    const deposit = series(71, 'ore', 'ir/GBP', 'mm', '');
    const swap = series(72, 'bloomberg', 'ir/GBP', 'ir_swap', '&index_name=GBP-SONIA&tenor=3M&fwd_start=1Y');
    const otherStart = series(73, 'bloomberg', 'ir/GBP', 'ir_swap', '&index_name=GBP-SONIA&tenor=3M&fwd_start=0D');
    const discount = series(74, 'ore', 'ir/GBP', 'discount', '&curve_id=GBP-SONIA');
    const oreSpot = series(75, 'ore', 'fx/EUR', 'fx_spot', '&ccy=USD');
    const vendorSpot = series(76, 'bloomberg', 'fx/EUR', 'fx_spot', '&ccy=USD');
    const rows = caseId === 'dated-ir-series-dialogue' ? [deposit, swap] : [deposit, swap, otherStart, discount, oreSpot, vendorSpot];
    const point = (s, value, term = '') => ({
        ...owner, series_id: s.id, value, source: 'LABELLED MARKET FIXTURE',
        observation_datetime: '2026-11-06 16:00:00.000000Z',
        oresmd_uri: s.oresmd_uri.replace('type=series', 'type=quote') + (term ? '&term=' + term : ''),
    });
    const points = [
        point(deposit, '0.03125', '6M'), point(swap, '0.03415', '6M'), point(swap, '0.03750', '5Y'),
        point(otherStart, '0.041', '5Y'), point(discount, '0.9825', '6M'),
        point(oreSpot, '1.151'), point(vendorSpot, '1.1520000000000000001'),
    ].sort((a, b) => a.oresmd_uri.localeCompare(b.oresmd_uri));
    return {
        ready: () => true,
        close: async () => {},
        request: async (operation, args, _session, signal) => {
            signal?.throwIfAborted();
            let matched, key;
            if (operation === 'series') {
                key = 'market_series';
                matched = rows.filter(row =>
                    (!args.filter?.provider_code || row.provider_code === args.filter.provider_code) &&
                    (!args.filter?.id_one_of || args.filter.id_one_of.includes(row.id)));
            } else if (operation === 'observations') {
                key = 'market_observations';
                matched = points.filter(row => row.series_id === args.series_id &&
                    Date.parse(row.observation_datetime) <= Date.parse(args.filter.observation_datetime_to));
            } else throw Error('The dated market fixture only supports series and observations.');
            return { result: { outcome: 'ok' }, total: matched.length, [key]: matched.slice(args.offset, args.offset + args.limit) };
        },
    };
}
