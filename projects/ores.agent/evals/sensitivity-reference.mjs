// SPDX-License-Identifier: GPL-3.0-or-later
import { readFileSync } from 'node:fs';
import { fixtureIds, id } from './fixtures.mjs';

const ore = new URL('../../../external/ore/examples/', import.meta.url);
const references = {
    'risk-rate-reference': {
        file: 'Legacy/Example_15/ExpectedOutput/sensitivity.csv',
        trades: ['FX_CALL_OPTION', 'FX_PUT_OPTION'],
        factors: ['DiscountCurve/EUR/6/10Y', 'DiscountCurve/EUR/7/15Y', 'DiscountCurve/USD/6/10Y', 'DiscountCurve/USD/7/15Y'],
    },
    'risk-fx-volatility-reference': {
        file: 'Performance/ExpectedOutput/sensi/gpu/sensitivity.csv',
        trades: ['5:TaRF', '6:TaRF_2'],
        factors: ['FXVolatility/EURUSD/122/3M/-0.05', 'FXVolatility/EURUSD/134/3M/ATM'],
    },
};

export function referenceSensitivities(caseId) {
    const reference = references[caseId];
    if (!reference) return;
    const csv = readFileSync(new URL(reference.file, ore), 'utf8').trimEnd().split(/\r?\n/);
    const metrics = csv.slice(1).flatMap((record, index) => {
        const [trade, isPar, factor, shift, factor2, shift2, currency, , delta, gamma] = record.split(',');
        const position = reference.trades.indexOf(trade);
        if (position < 0 || isPar !== 'false' || factor2 || !reference.factors.includes(factor)) return [];
        return [['Delta', delta], ['Gamma', gamma]].flatMap(([measure, value], column) => value === '#N/A' ? [] : [{
            id: id(30000 + index * 2 + column), tenant_id: fixtureIds.tenant, party_id: fixtureIds.party,
            report_instance_id: fixtureIds.run, report_definition_id: fixtureIds.definition,
            result_id: id(50000), line: index + 2, as_of: '2026-11-06T00:00:00Z',
            subject_type: 'trade', subject_id: id(5 + position), measure, value: Number(value),
            source: 'sensitivity.csv', family: 'sensitivity_report', currency, scenario: '', factor: '',
            qualifiers: JSON.stringify({ IsPar: isPar, Factor_1: factor, ShiftSize_1: shift, Factor_2: factor2, ShiftSize_2: shift2, Currency: currency }),
        }]);
    });
    return { ...reference, metrics };
}
