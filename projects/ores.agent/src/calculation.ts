// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { decimalProduct, decimalQuotient, decimalTotal } from './decimal.js';

const decimal = z
    .string()
    .max(64)
    .regex(/^[+-]?\d+(?:\.\d+)?$/);
const places = z.number().int().min(0).max(18);

// llama.cpp needs the arguments at the top level of the function schema.
// The executor still validates the operation-specific combinations below.
export const calculationModelInput = z
    .object({
        operation: z.enum([
            'sum',
            'difference',
            'product',
            'divide',
            'percentage',
            'basis_point_change',
        ]),
        values: z
            .array(decimal)
            .min(1)
            .max(100)
            .optional()
            .describe('Decimal strings for sum only.'),
        left: decimal.optional().describe('First operand for difference, product or divide.'),
        right: decimal.optional().describe('Second operand for difference, product or divide.'),
        part: decimal.optional().describe('Numerator for percentage.'),
        whole: decimal.optional().describe('Denominator for percentage.'),
        places: places.optional().describe('Required for divide and percentage only.'),
        oldRate: decimal.optional().describe('Starting rate as a decimal for basis_point_change.'),
        newRate: decimal.optional().describe('Ending rate as a decimal for basis_point_change.'),
    })
    .strict();
export function requestsComputation(message: string) {
    const keywords =
        /\b(?:python|calculate|compute|arithmetic|add|subtract|multiply|divide|sum|average|mean|median|percentiles?|percent(?:age)?|basis points?|correlation|regression|statistics|volatility|outliers?|rank|compare|differences?|standard deviation)\b/i;
    const expression = /\d\s*[+*/×÷−]\s*\d|\d\s+-\s+\d|^\s*\d+(?:\.\d+)?-\d+(?:\.\d+)?\s*\??\s*$/;
    return keywords.test(message) || expression.test(message);
}

export const calculationTool = {
    description:
        'Use for exact arithmetic on known numbers. “Change from 2% to 2.125% in basis points” → {"operation":"basis_point_change","oldRate":"0.02","newRate":"0.02125"}. Pass decimal strings without units. Division and percentage round half away from zero to the requested precision. Use run_python for custom statistics, OreStudio result tools for official valuations.',
    schema: z.discriminatedUnion('operation', [
        z
            .object({ operation: z.literal('sum'), values: z.array(decimal).min(1).max(100) })
            .strict(),
        z
            .object({ operation: z.enum(['difference', 'product']), left: decimal, right: decimal })
            .strict(),
        z
            .object({
                operation: z.literal('divide'),
                left: decimal,
                right: decimal,
                places,
            })
            .strict(),
        z
            .object({
                operation: z.literal('percentage'),
                part: decimal,
                whole: decimal,
                places,
            })
            .strict(),
        z
            .object({
                operation: z.literal('basis_point_change'),
                oldRate: decimal,
                newRate: decimal,
            })
            .strict(),
    ]),
};

export function calculate(input: z.infer<typeof calculationTool.schema>) {
    let value: string;
    switch (input.operation) {
        case 'sum':
            value = decimalTotal(input.values);
            break;
        case 'difference':
            value = decimalTotal([input.left, decimalProduct(input.right, '-1')]);
            break;
        case 'product':
            value = decimalProduct(input.left, input.right);
            break;
        case 'divide':
            value = decimalQuotient(input.left, input.right, input.places);
            break;
        case 'percentage':
            value = decimalQuotient(decimalProduct(input.part, '100'), input.whole, input.places);
            break;
        case 'basis_point_change':
            value = decimalProduct(
                decimalTotal([input.newRate, decimalProduct(input.oldRate, '-1')]),
                '10000',
            );
            break;
    }
    return { tool: 'calculate', status: 'ok', operation: input.operation, value };
}
