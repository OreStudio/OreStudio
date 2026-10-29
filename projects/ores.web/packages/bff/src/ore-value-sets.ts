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
 */

import { subjects as businessDayConventionTypeSubjects } from '@ores/wire-protocol/generated/refdata/protocol/business_day_convention_type_protocol';
import { subjects as calendarSubjects } from '@ores/wire-protocol/generated/refdata/protocol/calendar_protocol';
import { subjects as currencyPairSubjects } from '@ores/wire-protocol/generated/refdata/protocol/currency_pair_protocol';
import { subjects as currencySubjects } from '@ores/wire-protocol/generated/refdata/protocol/currency_protocol';
import { subjects as dayCounterSubjects } from '@ores/wire-protocol/generated/refdata/protocol/day_counter_protocol';
import { subjects as instrumentCodeSubjects } from '@ores/wire-protocol/generated/refdata/protocol/instrument_code_protocol';
import { subjects as templateSubjects } from '@ores/wire-protocol/generated/analytics/protocol/sensitivity_template_protocol';
import { subjects as tenorSubjects } from '@ores/wire-protocol/generated/refdata/protocol/tenor_protocol';
import type { LiveSession } from './sessions.js';
import { readAll, text } from './refdata-calls.js';

/**
 * The allowed values of an ORE parameter, read from the value set tables, as
 * every screen of ORE leaves draws its controls from them. A set's catalogue is
 * read through the reader its relation names, once however many sets draw on it.
 */

/** One choice a combo offers: the value a configuration holds, and the words a person reads. */
export interface Choice {
    readonly value: string;
    readonly label: string;
}

/** The allowed values of one parameter, as the value set tables state them. */
export interface ValueFields {
    readonly valueSet: string;
    readonly sourceKind: string;
    /** The catalogue the set draws on, or empty for a set that states its own values. */
    readonly catalogue: string;
    readonly choices: readonly Choice[];
    readonly minimum: number | null;
    readonly maximum: number | null;
    readonly pattern: string;
}

/** A parameter whose setting names no value set carries these fields empty. */
/** The fields a parameter carries when its setting names no value set. */
export const NO_VALUE_SET: ValueFields = {
    valueSet: '',
    sourceKind: '',
    catalogue: '',
    choices: [],
    minimum: null,
    maximum: null,
    pattern: '',
};

/** The built-in values of a boolean set. */
const BOOLEAN_CHOICES: readonly Choice[] = [
    { value: 'true', label: 'true' },
    { value: 'false', label: 'false' },
];

/** The scope a setting's link row names, in the words the tables use. */
const LINK_SCOPE: Readonly<Record<string, string>> = {
    model: 'ModelParameters',
    engine: 'EngineParameters',
    GlobalParameters: 'GlobalParameters',
};

function linkScopeOf(scope: string): string {
    return LINK_SCOPE[scope] ?? scope;
}

/** How one catalogue is read: the list, and the columns that hold its value and its label. */
interface CatalogueReader {
    readonly subject: string;
    readonly key: string;
    readonly valueOf: (row: Readonly<Record<string, unknown>>) => string;
    readonly labelOf: (row: Readonly<Record<string, unknown>>) => string;
}

/**
 * The catalogue relations a value set may name, each read as the pair a combo
 * offers. A set names its relation in a row of the catalogue table, so only
 * these relations are ever read; a row that names another one is a seed error
 * and is refused rather than drawn empty.
 */
const CATALOGUE_READERS: ReadonlyMap<string, CatalogueReader> = new Map([
    [
        'ores_refdata_tenor_catalogue_v',
        {
            subject: tenorSubjects.list_tenors_request,
            key: 'tenors',
            valueOf: (row) => text(row['code']),
            labelOf: (row) => text(row['display_name']),
        },
    ],
    [
        'ores_refdata_currency_catalogue_v',
        {
            subject: currencySubjects.list_currencies_request,
            key: 'currencies',
            valueOf: (row) => text(row['iso_code']),
            labelOf: (row) => text(row['name']),
        },
    ],
    [
        'ores_refdata_currency_pair_catalogue_v',
        {
            subject: currencyPairSubjects.list_currency_pairs_request,
            key: 'pairs',
            valueOf: (row) => text(row['pair_code']),
            labelOf: (row) => text(row['pair_code']),
        },
    ],
    [
        'ores_analytics_sensitivity_template_catalogue_v',
        {
            subject: templateSubjects.list_sensitivity_templates_request,
            key: 'templates',
            valueOf: (row) => text(row['code']),
            labelOf: (row) => text(row['code']),
        },
    ],
    [
        'ores_refdata_instrument_code_catalogue_v',
        {
            subject: instrumentCodeSubjects.list_instrument_codes_request,
            key: 'instruments',
            valueOf: (row) => text(row['code']),
            labelOf: (row) => text(row['name']),
        },
    ],
    [
        'ores_refdata_calendar_catalogue_v',
        {
            subject: calendarSubjects.list_calendars_request,
            key: 'calendars',
            valueOf: (row) => text(row['code']),
            labelOf: (row) => text(row['name']),
        },
    ],
    [
        'ores_refdata_day_counter_catalogue_v',
        {
            subject: dayCounterSubjects.list_day_counters_request,
            key: 'day_counters',
            valueOf: (row) => text(row['code']),
            labelOf: (row) => text(row['description']),
        },
    ],
    [
        'ores_refdata_business_day_convention_catalogue_v',
        {
            subject: businessDayConventionTypeSubjects.list_business_day_convention_types_request,
            key: 'types',
            valueOf: (row) => text(row['code']),
            labelOf: (row) => text(row['name']),
        },
    ],
]);

/** The number a set row carries, or null when it carries none. */
function optionalNumber(value: unknown): number | null {
    return typeof value === 'number' ? value : null;
}

/** The four value set tables as the service returned them. */
export interface ValueSetTables {
    readonly sets: readonly Readonly<Record<string, unknown>>[];
    readonly values: readonly Readonly<Record<string, unknown>>[];
    readonly catalogues: readonly Readonly<Record<string, unknown>>[];
    readonly links: readonly Readonly<Record<string, unknown>>[];
}

/**
 * Reads the value set tables and answers the allowed values of a setting by
 * (scope, name). Every catalogue set is resolved once here, before the reply is
 * built, and each relation is read once however many sets draw on it.
 */
export async function readValueSets(
    session: LiveSession,
    tables: ValueSetTables,
): Promise<(scope: string, name: string) => ValueFields> {
    const setForSetting = new Map(
        tables.links.map((row) => [
            `${text(row['scope'])}|${text(row['name'])}`,
            text(row['value_set_code']),
        ]),
    );
    const setOf = new Map(tables.sets.map((row) => [text(row['code']), row]));
    const relationOf = new Map(
        tables.catalogues.map((row) => [text(row['code']), text(row['relation'])]),
    );
    const ordered = new Map<string, { readonly order: number; readonly choice: Choice }[]>();
    for (const row of tables.values) {
        const code = text(row['value_set_code']);
        const entries = ordered.get(code) ?? [];
        entries.push({
            order: optionalNumber(row['display_order']) ?? 0,
            choice: { value: text(row['value']), label: text(row['label']) },
        });
        ordered.set(code, entries);
    }
    const enumerated = new Map<string, readonly Choice[]>(
        [...ordered].map(([code, entries]) => [
            code,
            [...entries].sort((a, b) => a.order - b.order).map((entry) => entry.choice),
        ]),
    );

    const relationChoices = new Map<string, Promise<readonly Choice[]>>();
    function readRelation(setCode: string, relation: string): Promise<readonly Choice[]> {
        const reader = CATALOGUE_READERS.get(relation);
        if (reader === undefined) {
            throw new Error(
                `The value set "${setCode}" names the catalogue relation "${relation}", which no reader knows.`,
            );
        }
        let pending = relationChoices.get(relation);
        if (pending === undefined) {
            pending = readAll(session, reader.subject, reader.key, {
                filter: null,
                as_of: null,
            }).then((rows) =>
                rows
                    .map((row) => ({ value: reader.valueOf(row), label: reader.labelOf(row) }))
                    .filter((choice) => choice.value !== ''),
            );
            relationChoices.set(relation, pending);
        }
        return pending;
    }

    const resolved = new Map<string, readonly Choice[]>(
        await Promise.all(
            [...setOf].flatMap(([code, row]) => {
                if (text(row['source_kind']) !== 'catalogue') return [];
                const catalogueCode = text(row['catalogue_code']);
                const relation = relationOf.get(catalogueCode);
                if (relation === undefined) {
                    throw new Error(
                        `The value set "${code}" names the catalogue "${catalogueCode}", which no catalogue row holds.`,
                    );
                }
                return [readRelation(code, relation).then((choices) => [code, choices] as const)];
            }),
        ),
    );

    function choicesFor(code: string, kind: string): readonly Choice[] {
        if (kind === 'boolean') return BOOLEAN_CHOICES;
        if (kind === 'catalogue') return resolved.get(code) ?? [];
        if (kind === 'enumerated') return enumerated.get(code) ?? [];
        return [];
    }

    const fields = new Map<string, ValueFields>();
    return (scope, name) => {
        const key = `${linkScopeOf(scope)}|${name}`;
        const known = fields.get(key);
        if (known !== undefined) return known;
        const code = setForSetting.get(key) ?? '';
        const row = setOf.get(code);
        const found: ValueFields =
            row === undefined
                ? NO_VALUE_SET
                : {
                      valueSet: code,
                      sourceKind: text(row['source_kind']),
                      catalogue: text(row['catalogue_code']),
                      choices: choicesFor(code, text(row['source_kind'])),
                      minimum: optionalNumber(row['minimum']),
                      maximum: optionalNumber(row['maximum']),
                      pattern: text(row['pattern']),
                  };
        fields.set(key, found);
        return found;
    };
}
