// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';

export const riskMeasure = z.enum(['delta', 'gamma', 'vega', 'dv01']);
export type RiskMeasure = z.infer<typeof riskMeasure>;
export const riskMaturity = z.string().regex(/^[1-9]\d{0,2}[DWMY]$/);
export const riskPair = z
    .string()
    .regex(/^[A-Z]{6}$/)
    .refine((value) => value.slice(0, 3) !== value.slice(3));
const words = [
    'one',
    'two',
    'three',
    'four',
    'five',
    'six',
    'seven',
    'eight',
    'nine',
    'ten',
    'eleven',
    'twelve',
];

export function riskSelectors(text: string) {
    const pairs = [
        ...new Set(
            [...text.matchAll(/\b([A-Z]{3})\s*\/\s*([A-Z]{3})\b/gi)].map((match) =>
                (match[1]! + match[2]!).toUpperCase(),
            ),
        ),
    ];
    const terms = [
        ...new Set(
            [
                ...text.matchAll(
                    /\b(\d{1,3}|one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve)(?:\s*-\s*|\s*)(days?|weeks?|months?|years?|[DWMY])\b/gi,
                ),
            ].map((match) => {
                const length = /^\d/.test(match[1]!)
                    ? Number(match[1])
                    : words.indexOf(match[1]!.toLowerCase()) + 1;
                return length + match[2]![0]!.toUpperCase();
            }),
        ),
    ];
    if (
        pairs.length > 1 ||
        terms.length > 1 ||
        (pairs[0] && !riskPair.safeParse(pairs[0]).success) ||
        (terms[0] && !riskMaturity.safeParse(terms[0]).success)
    )
        return {
            clarification: 'Choose one FX pair and one maturity for this sensitivity request.',
        };
    return {
        fxPair: pairs[0],
        maturity: terms[0],
        smile: /\b(?:atm|at[- ]the[- ]money)\b/i.test(text) ? ('ATM' as const) : undefined,
    };
}

// ORE Factor_1 uses type/name/pillar index/pillar label, followed by a smile label for volatility.
// A numeric pillar index is not a maturity and the pair direction is significant.
export function factorPillar(factor: string) {
    const [type, name, index, label, smile] = factor.split('/');
    return {
        type,
        pair: type?.startsWith('FX') && name && riskPair.safeParse(name).success ? name : undefined,
        maturity:
            index && /^\d+$/.test(index) && label && riskMaturity.safeParse(label).success
                ? label
                : undefined,
        smile,
    };
}
