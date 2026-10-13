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

import { show } from '../../refdata/records.js';

/** The index families a curve can stand for, in the order the Qt editor offers them. */
export const INDEX_FAMILIES = [
    'libor',
    'euribor',
    'sofr',
    'estr',
    'sonia',
    'tona',
    'saron',
    'aonia',
    'corra',
    'honia',
    'sora',
    'swestr',
    'nowa',
    'kofr',
    'mibor',
    'zaronia',
    'destr',
    'polonia',
    'nzonia',
    'shibor',
    'tiie',
    'taibor',
] as const;

/** The families whose index has a term, such as EURIBOR 6M. The others are overnight rates. */
export const TERM_FAMILIES: readonly string[] = ['libor', 'euribor'];

/** The tenor of an overnight index, which the oresmd series states as `1D`. */
const OVERNIGHT_TENOR = '1D';

/** The meeting-dated variant of an overnight curve, which is not an index tenor. */
const FOMC = 'FOMC';

/**
 * The tenor of the index a curve stands for, as the `tenor` field of the
 * series its pillars land in: `1D` for an overnight index and for the
 * meeting-dated FOMC variant, and the curve's own period for a term index.
 * The synthetic service states the same rule.
 */
export function indexTenorOf(feed: Readonly<Record<string, unknown>>): string {
    const tenor = show(feed['tenor']);
    return tenor === '' || tenor === FOMC ? OVERNIGHT_TENOR : tenor;
}

/**
 * The oresmd series a curve's pillars that start at spot land in. A curve is a
 * series, so this is the curve's name in market data.
 */
export function irSeriesUri(feed: Readonly<Record<string, unknown>>): string {
    return `oresmd://ir/${show(feed['currency_code'])}?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=${indexTenorOf(feed)}`;
}

/** The curve as the Qt editor names it: the currency and the index, such as `USD SOFR` or `EUR EURIBOR 6M`. */
export function curveOf(feed: Readonly<Record<string, unknown>>): string {
    const tenor = show(feed['tenor']);
    return [
        show(feed['currency_code']),
        show(feed['index_family']).toUpperCase(),
        ...(tenor === '' ? [] : [tenor]),
    ].join(' ');
}

/**
 * The fields that follow from the others: a source name from the collection's
 * currency and index when none is given, a tenor only for an index that has
 * one, and the vintage fields the price source does not use emptied.
 */
export function completeIrFeed(write: Readonly<Record<string, unknown>>): Record<string, unknown> {
    const fixed = write['price_source'] === 'fixed';
    const family = show(write['index_family']);
    const tenor = show(write['tenor']);
    const keepsTenor = TERM_FAMILIES.includes(family) || (family === 'sofr' && tenor === FOMC);
    return {
        ...write,
        tenor: keepsTenor ? tenor : '',
        source_name:
            show(write['source_name']) === ''
                ? `synthetic.${show(write['currency_code'])}${family}${keepsTenor ? tenor : ''}`.toLowerCase()
                : write['source_name'],
        vintage_source: fixed ? '' : write['vintage_source'],
        vintage_date: fixed ? '' : write['vintage_date'],
        vintage_series_uri: fixed ? '' : write['vintage_series_uri'],
    };
}

/** One parameter of the engine on the Process tab: its definition, the row that holds it and its value as typed. */
export interface ParameterLine {
    readonly definition: Readonly<Record<string, unknown>>;
    /** The saved value row, when the feed holds one for this parameter. */
    readonly saved: Readonly<Record<string, unknown>> | undefined;
    /** The value as text: the saved one, or the definition's default. */
    readonly value: string;
}

/** The engine's parameters in screen order, each with the value the feed holds or the default. */
export function parameterLines(
    definitions: readonly Readonly<Record<string, unknown>>[],
    values: readonly Readonly<Record<string, unknown>>[],
    engine: string,
): readonly ParameterLine[] {
    return definitions
        .filter((definition) => show(definition['process_type_code']) === engine)
        .sort((a, b) => Number(a['display_order']) - Number(b['display_order']))
        .map((definition) => {
            const saved = values.find(
                (row) => show(row['parameter_definition_id']) === show(definition['id']),
            );
            return {
                definition,
                saved,
                value: show(saved?.['parameter_value'] ?? definition['default_value']),
            };
        });
}

/** Whether a value typed for a parameter is a number inside the definition's bounds. */
export function parameterError(
    definition: Readonly<Record<string, unknown>>,
    typed: string,
): 'number' | 'min' | 'max' | undefined {
    const value = Number(typed);
    if (typed.trim() === '' || !Number.isFinite(value)) {
        return 'number';
    }
    const min = definition['min_value'];
    const max = definition['max_value'];
    if (typeof min === 'number' && value < min) {
        return 'min';
    }
    return typeof max === 'number' && value > max ? 'max' : undefined;
}

/**
 * The writes that make the saved parameters the typed ones: each parameter of
 * the engine whose value is new or changed, against the version read, and
 * each saved value of another engine's parameter, to be removed. A value that
 * did not change is not written.
 */
export function parameterPlan(
    feedId: string,
    lines: readonly ParameterLine[],
    typed: Readonly<Record<string, string>>,
    saved: readonly Readonly<Record<string, unknown>>[],
    newId: () => string,
): {
    readonly changes: readonly {
        readonly write: Readonly<Record<string, unknown>>;
        readonly version: number | null;
    }[];
    readonly removals: readonly { readonly key: string; readonly version: number | null }[];
} {
    const changes = lines.flatMap((line) => {
        const text = typed[show(line.definition['id'])] ?? line.value;
        const value = Number(text);
        if (line.saved !== undefined && Number(line.saved['parameter_value']) === value) {
            return [];
        }
        return [
            {
                write: {
                    id: line.saved === undefined ? newId() : show(line.saved['id']),
                    config_id: feedId,
                    parameter_definition_id: show(line.definition['id']),
                    parameter_value: value,
                },
                version: line.saved === undefined ? null : Number(line.saved['version']),
            },
        ];
    });
    const kept = new Set(
        lines.flatMap((line) => (line.saved === undefined ? [] : [show(line.saved['id'])])),
    );
    const removals = saved
        .filter((row) => !kept.has(show(row['id'])))
        .map((row) => ({ key: show(row['id']), version: Number(row['version']) }));
    return { changes, removals };
}

/** Which limits a parameter has, for the words that say what it accepts. */
export function parameterLimits(
    min: number | undefined,
    max: number | undefined,
): { readonly kind: 'none' | 'min' | 'max' | 'both' } {
    if (min !== undefined && max !== undefined) return { kind: 'both' };
    if (min !== undefined) return { kind: 'min' };
    return max !== undefined ? { kind: 'max' } : { kind: 'none' };
}
