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

import type { OresmdComposeRequest, OresmdParseResult, OresmdSchema } from '@ores/contracts';

/**
 * What the oresmd URL editor holds while a person edits one: the kind of URI,
 * the instrument or the index family, the quote, the path and the text of each
 * other field. The server's codec decides whether they make a URI, so nothing
 * here knows a rule of the grammar. This file only arranges the parts the
 * server's schema describes.
 */

export const KINDS = ['quote', 'series', 'fixing'] as const;
export type Kind = (typeof KINDS)[number];

export interface EditorState {
    readonly kind: Kind;
    /** An instrument's URI code, or an index family's code for a fixing. */
    readonly instrument: string;
    readonly quote: string;
    /** The text of the URI's path. */
    readonly subject: string;
    /** The text of each field the person has typed, by the field's name. */
    readonly values: Readonly<Record<string, string>>;
}

export const EMPTY: EditorState = {
    kind: 'quote',
    instrument: '',
    quote: '',
    subject: '',
    values: {},
};

function isKind(text: string): text is Kind {
    return (KINDS as readonly string[]).includes(text);
}

/** The state a parsed URI gives, or the empty state when the codec refused it. */
export function fromParse(result: OresmdParseResult): EditorState {
    if (!result.success || !isKind(result.kind)) return EMPTY;
    return {
        kind: result.kind,
        instrument: result.instrument,
        quote: result.quote,
        subject: result.subject,
        values: Object.fromEntries(result.fields.map((field) => [field.name, field.text])),
    };
}

/** One input the editor draws. */
export interface EditorField {
    readonly name: string;
    /** Whether the field names the series or a point within it. */
    readonly role: string;
    /** Whether a URI may leave the field out. */
    readonly optional: boolean;
    /** The kind of value: text, term, decimal, strike, code or strike_label. */
    readonly kind: string;
    /** The tokens of a closed set. Empty when the field takes any text. */
    readonly codes: readonly string[];
}

/** The inputs of the chosen instrument or index family. */
export interface EditorRow {
    /** The name of the field the URI writes as its path. */
    readonly subject: string;
    /** The asset class, which the URI writes as its authority. */
    readonly asset: string;
    /** The fields other than the path, in the order a key writes them. */
    readonly fields: readonly EditorField[];
}

/**
 * The inputs for the state, or null when no instrument is chosen or the schema
 * has none by that code. A series holds only the identity fields of its row.
 */
export function rowOf(schema: OresmdSchema, state: EditorState): EditorRow | null {
    if (state.kind === 'fixing') {
        const family = schema.indexes.find((candidate) => candidate.code === state.instrument);
        if (family === undefined) return null;
        return {
            subject: family.subject,
            asset: family.asset,
            fields: family.fields.map((field) => ({
                name: field.name,
                role: 'identity',
                optional: field.optional,
                kind: 'text',
                codes: [],
            })),
        };
    }
    const instrument = schema.instruments.find((candidate) => candidate.code === state.instrument);
    if (instrument === undefined) return null;
    return {
        subject: instrument.subject,
        asset: instrument.asset,
        fields: instrument.fields
            .filter((field) => field.name !== instrument.subject)
            .filter((field) => state.kind !== 'series' || field.role === 'identity')
            .map((field) => ({
                name: field.name,
                role: field.role,
                optional: field.may_be_none,
                kind: field.kind,
                codes: field.codes,
            })),
    };
}

/**
 * The parts to send the codec, or null when there is nothing to ask yet: no
 * instrument is chosen, or a quote or a series has no quote type. Only the values
 * of fields the current row holds are sent, so a value typed under another
 * instrument does not travel with this one.
 */
export function composeRequestOf(
    schema: OresmdSchema,
    state: EditorState,
): OresmdComposeRequest | null {
    const row = rowOf(schema, state);
    if (row === null) return null;
    if (state.kind !== 'fixing' && state.quote === '') return null;
    return {
        kind: state.kind,
        instrument: state.instrument,
        quote: state.kind === 'fixing' ? '' : state.quote,
        subject: state.subject,
        fields: row.fields.map((field) => ({
            name: field.name,
            text: state.values[field.name] ?? '',
        })),
    };
}

/** The state with another kind. A fixing and a quote choose from different lists, so the instrument is cleared when the lists differ. */
export function withKind(state: EditorState, kind: Kind): EditorState {
    if (kind === state.kind) return state;
    const sameList = (kind === 'fixing') === (state.kind === 'fixing');
    return { ...state, kind, instrument: sameList ? state.instrument : '' };
}

/** The state with one field's text. Blank text is kept, because a blank is a field left out. */
export function withValue(state: EditorState, name: string, text: string): EditorState {
    return { ...state, values: { ...state.values, [name]: text } };
}

/** A stable text for a request, so a query can key on it. */
export function keyOf(request: OresmdComposeRequest | null): string {
    return request === null ? '' : JSON.stringify(request);
}
