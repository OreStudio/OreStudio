// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { decimalProduct, decimalQuotient, decimalTotal } from './decimal.js';
import { markdownTable } from './markdown.js';
import { observationTime } from './market.js';

const decimal = z.string().regex(/^[+-]?\d+(?:\.\d+)?(?:e[+-]?\d+)?$/i);
const scope = z.object({
    tool: z.literal('summarize_scope_results'),
    status: z.enum(['ok', 'stale']),
    total: z.object({ currency: z.string() }),
    contributors: z.array(z.object({ figure: decimal, currency: z.string() })),
});
const series = z.object({
    tool: z.literal('read_market_observations'),
    rows: z.array(
        z.object({
            observation_datetime: z
                .string()
                .refine((value) => Number.isFinite(observationTime(value))),
            value: decimal,
            source: z.string(),
        }),
    ),
});
export const analysisPlan = z
    .object({
        operation: z.enum(['absolute_share', 'observation_changes']),
        topCount: z
            .number()
            .int()
            .min(1)
            .max(25)
            .optional()
            .describe(
                'For absolute_share only: number of largest displayed contributors in the numerator.',
            ),
    })
    .strict();
export const planReadAnalysis = {
    type: 'function',
    function: {
        name: 'plan_read_analysis',
        description:
            'Choose absolute_share for the share of displayed absolute NPVs from the top N rows. Choose observation_changes for consecutive changes in stored observations. Pass only the operation and any required count.',
        parameters: z.toJSONSchema(analysisPlan, { io: 'input' }),
    },
};
export const planObservationAnalysis = {
    ...planReadAnalysis,
    function: {
        ...planReadAnalysis.function,
        description:
            'Choose observation_changes to calculate consecutive changes from the stored observations.',
        parameters: z.toJSONSchema(
            z.object({ operation: z.literal('observation_changes') }).strict(),
            { io: 'input' },
        ),
    },
};

export function requestedShareCount(request: string) {
    const clause = request.split(/\b(?:share|concentration|proportion|percentage)\b/i).at(-1);
    const words = 'one two three four five six seven eight nine ten'.split(' ');
    const count =
        /\b(?:top\s+(\d+|one|two|three|four|five|six|seven|eight|nine|ten)|(?:largest\s+)(\d+|one|two|three|four|five|six|seven|eight|nine|ten)|(\d+|one|two|three|four|five|six|seven|eight|nine|ten)\s+largest)\b/i.exec(
            clause ?? '',
        );
    const value = (count?.[1] ?? count?.[2] ?? count?.[3])?.toLowerCase();
    return value === undefined
        ? undefined
        : /^\d+$/.test(value)
          ? Number(value)
          : words.indexOf(value) + 1;
}

export function prepareReadAnalysis(raw: unknown) {
    const report = scope.safeParse(raw);
    if (report.success) {
        const { contributors, total } = report.data;
        if (!contributors.length || contributors.some((row) => row.currency !== total.currency))
            return;
        return { kind: 'scope' as const, contributors };
    }
    const observations = series.safeParse(raw);
    if (!observations.success || observations.data.rows.length < 2) return;
    const rows = observations.data.rows
        .map((row) => ({ ...row, instant: observationTime(row.observation_datetime) }))
        .sort((a, b) => a.instant - b.instant);
    if (rows.some((row, index) => row.instant === rows[index - 1]?.instant)) return;
    return { kind: 'observations' as const, rows };
}

export function processReadAnalysis(
    data: NonNullable<ReturnType<typeof prepareReadAnalysis>>,
    plan: z.infer<typeof analysisPlan>,
) {
    if (data.kind === 'scope' && plan.operation === 'absolute_share') {
        if (!plan.topCount || plan.topCount > data.contributors.length) return;
        const figures = data.contributors.map((row) => row.figure.replace(/^[+-]/, ''));
        const denominator = decimalTotal(figures);
        if (denominator === '0') return;
        const part = decimalTotal(figures.slice(0, plan.topCount));
        const value = decimalQuotient(decimalProduct(part, '100'), denominator, 2);
        return {
            evidence: {
                tool: 'process_read_analysis',
                status: 'ok',
                operation: plan.operation,
                topCount: plan.topCount,
                count: figures.length,
                value,
            },
            answer: `The top ${plan.topCount} account for **${value}%** of the sum of the ${figures.length} displayed absolute NPVs.`,
        };
    }
    if (
        data.kind !== 'observations' ||
        plan.operation !== 'observation_changes' ||
        plan.topCount !== undefined
    )
        return;
    const changes = data.rows.slice(1).map((row, index) => {
        const previous = data.rows[index]!;
        return {
            from: previous.observation_datetime,
            to: row.observation_datetime,
            previous: previous.value,
            value: row.value,
            change: decimalTotal([row.value, decimalProduct(previous.value, '-1')]),
            fromSource: previous.source,
            source: row.source,
        };
    });
    return {
        evidence: {
            tool: 'process_read_analysis',
            status: 'ok',
            operation: plan.operation,
            changes,
        },
        answer: [
            'Changes between consecutive stored observations, in quote units.',
            markdownTable(
                ['From', 'To', 'Previous', 'Value', 'Change', 'Sources'],
                changes.map((row) => [
                    row.from,
                    row.to,
                    row.previous,
                    row.value,
                    row.change,
                    row.fromSource === row.source
                        ? row.source
                        : row.fromSource + ' → ' + row.source,
                ]),
            ),
        ].join('\n\n'),
    };
}
