/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { show } from '../refdata/records.js';
import type { ParameterLine } from './irCurve.js';

/** The four questions the simple view of an IR feed asks, by the names the service uses. */
export const SIMPLE_NAMES = [
    'start_rate',
    'long_run_rate',
    'half_life_years',
    'yearly_move_bp',
] as const;

export type SimpleName = (typeof SIMPLE_NAMES)[number];
export type SimpleView = Readonly<Record<SimpleName, number>>;

/** The answers a person can pick instead of typing, by the word they would use. */
export const HALF_LIFE_PRESETS = [
    { key: 'slow', years: 5 },
    { key: 'medium', years: 2 },
    { key: 'fast', years: 0.5 },
] as const;

export const MOVE_PRESETS = [
    { key: 'calm', bp: 50 },
    { key: 'normal', bp: 100 },
    { key: 'stressed', bp: 200 },
] as const;

/** A number as a person would type it, without the float's trailing noise. */
export function plain(value: number, digits = 10): string {
    return String(Number(value.toPrecision(digits)));
}

/** A decimal rate as the percent a person says: 0.03 is "3". */
export function percentText(rate: number): string {
    return plain(rate * 100, 8);
}

/** The percent a person typed as the decimal rate the service reads. */
export function rateOf(percent: number): number {
    return Number(plain(percent / 100, 12));
}

/**
 * The values on the screen by parameter name, or undefined while one is
 * missing or is not a number, because the service cannot read half a set.
 */
export function parameterValues(
    lines: readonly ParameterLine[],
    typed: Readonly<Record<string, string>>,
): Readonly<Record<string, number>> | undefined {
    const out: Record<string, number> = {};
    for (const line of lines) {
        const text = typed[show(line.definition['id'])] ?? line.value;
        const value = Number(text);
        if (text.trim() === '' || !Number.isFinite(value)) {
            return undefined;
        }
        out[show(line.definition['parameter_name'])] = value;
    }
    return lines.length === 0 ? undefined : out;
}

/** The text to put in the draft for each parameter the service returned, by definition id. */
export function typedFrom(
    lines: readonly ParameterLine[],
    values: Readonly<Record<string, number>>,
): Readonly<Record<string, string>> {
    const out: Record<string, string> = {};
    for (const line of lines) {
        const value = values[show(line.definition['parameter_name'])];
        if (value !== undefined) {
            out[show(line.definition['id'])] = plain(value);
        }
    }
    return out;
}

/** Which way the curve leans: from where rates start to where they settle. */
export function leanOf(view: SimpleView): 'up' | 'down' | 'flat' {
    const gap = view.long_run_rate - view.start_rate;
    if (Math.abs(gap) < 0.0005) {
        return 'flat';
    }
    return gap > 0 ? 'up' : 'down';
}

/** The simple view out of the values the service returned, when it returned all four. */
export function viewOf(values: Readonly<Record<string, number>>): SimpleView | undefined {
    const [start, longRun, halfLife, move] = SIMPLE_NAMES.map((name) => values[name]);
    return start === undefined ||
        longRun === undefined ||
        halfLife === undefined ||
        move === undefined
        ? undefined
        : {
              start_rate: start,
              long_run_rate: longRun,
              half_life_years: halfLife,
              yearly_move_bp: move,
          };
}

/** The note under a parameter in the advanced table, in plain words: the view says what it means. */
export function readoutOf(
    name: string,
    view: SimpleView,
): { readonly key: 'halfLife' | 'rate' | 'move'; readonly value: string } | undefined {
    switch (name) {
        case 'kappa':
        case 'kappa_x':
            return { key: 'halfLife', value: plain(view.half_life_years, 3) };
        case 'theta':
            return { key: 'rate', value: percentText(view.long_run_rate) };
        case 'initial_rate':
            return { key: 'rate', value: percentText(view.start_rate) };
        case 'sigma':
        case 'sigma_x':
            return { key: 'move', value: plain(view.yearly_move_bp, 4) };
        default:
            return undefined;
    }
}
