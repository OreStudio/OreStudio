// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { AppError } from './backend.js';
import { marketId, observationTime } from './market.js';

const currency = z.string().regex(/^[A-Z]{3}$/);
export const npvMetric = z.object({
    id: marketId,
    tenant_id: marketId,
    party_id: marketId,
    report_instance_id: marketId,
    report_definition_id: marketId,
    subject_id: marketId,
    subject_type: z.string(),
    as_of: z.string(),
    measure: z.string(),
    family: z.string(),
    scenario: z.string(),
    factor: z.string(),
    currency: z.string(),
    value: z.number().finite(),
    source: z.string(),
    qualifiers: z.string().max(16384),
});
type Result = Omit<
    z.infer<typeof npvMetric>,
    'value' | 'family' | 'scenario' | 'factor' | 'qualifiers'
> & { figure: number };

function valuationInstant(text: string) {
    const timestamp = z.iso.date().safeParse(text).success ? text + 'T00:00:00Z' : text;
    const fraction = /\.(\d{1,6})Z$/.exec(timestamp)?.[1] ?? '';
    return BigInt(observationTime(timestamp)) * 1000n + BigInt(fraction.padEnd(6, '0').slice(3));
}

export function baseNpvCurrency(row: Result, metric: z.infer<typeof npvMetric>): string {
    if (
        row.measure !== 'npv_base' ||
        metric.measure !== 'NPV(Base)' ||
        metric.family !== 'npv_report' ||
        metric.scenario ||
        metric.factor ||
        row.source !== 'npv.csv' ||
        metric.source !== row.source ||
        metric.value !== row.figure ||
        valuationInstant(metric.as_of) !== valuationInstant(row.as_of) ||
        (
            [
                'id',
                'tenant_id',
                'party_id',
                'report_instance_id',
                'report_definition_id',
                'subject_type',
                'subject_id',
            ] as const
        ).some((key) => metric[key] !== row[key])
    )
        throw new AppError(
            'inconsistent_currency_metadata',
            'Stored base NPV and its typed metric disagree on provenance or value.',
            422,
        );
    let unit: string;
    try {
        unit = z
            .object({ BaseCurrency: currency })
            .parse(JSON.parse(metric.qualifiers)).BaseCurrency;
    } catch {
        throw new AppError(
            'invalid_currency_metadata',
            'The stored NPV(Base) metric has no valid BaseCurrency qualifier.',
            422,
        );
    }
    if ((row.currency && row.currency !== unit) || (metric.currency && metric.currency !== unit))
        throw new AppError(
            'inconsistent_currency_metadata',
            'Stored currency and the typed BaseCurrency qualifier disagree.',
            422,
        );
    return unit;
}
