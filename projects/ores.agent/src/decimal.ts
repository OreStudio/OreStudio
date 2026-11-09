// SPDX-License-Identifier: GPL-3.0-or-later
import { AppError } from './backend.js';
// Exact arithmetic over canonical decimal renderings of stored doubles;
// it cannot recover precision already lost in backend storage.
export function decimalParts(value: number): { coefficient: bigint; scale: number } {
    if (!Number.isFinite(value)) throw new AppError('invalid_figure', 'Non-finite stored figure.');
    return textParts(String(value));
}
function textParts(value: string): { coefficient: bigint; scale: number } {
    const match = /^([+-]?)(\d+)(?:\.(\d+))?(?:e([+-]?\d+))?$/i.exec(value);
    if (!match || value.length > 128 || Math.abs(Number(match[4] ?? 0)) > 400)
        throw new AppError('invalid_figure', 'Invalid or oversized decimal.');
    let coefficient = BigInt((match[1] || '') + match[2] + (match[3] || ''));
    let scale = (match[3]?.length ?? 0) - Number(match[4] ?? 0);
    if (scale < 0) {
        coefficient *= 10n ** BigInt(-scale);
        scale = 0;
    }
    return { coefficient, scale };
}
export function decimalSum(values: number[]): string {
    const parts = values.map(decimalParts),
        scale = Math.max(0, ...parts.map((p) => p.scale));
    const sum = parts.reduce(
        (total, p) => total + p.coefficient * 10n ** BigInt(scale - p.scale),
        0n,
    );
    return renderDecimal(sum, scale);
}
export function decimalText(value: string): string {
    const { coefficient, scale } = textParts(value);
    return renderDecimal(coefficient, scale);
}
function renderDecimal(sum: bigint, scale: number): string {
    const digits = (sum < 0n ? -sum : sum).toString().padStart(scale + 1, '0');
    const text = scale
        ? (digits.slice(0, -scale) + '.' + digits.slice(-scale)).replace(/\.?0+$/, '')
        : digits;
    return (sum < 0n ? '-' : '') + text;
}
