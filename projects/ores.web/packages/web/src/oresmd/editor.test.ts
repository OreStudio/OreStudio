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

import { describe, expect, it } from 'vitest';
import type { OresmdParseResult } from '@ores/contracts';
import {
    EMPTY,
    composeRequestOf,
    fromParse,
    keyOf,
    rowOf,
    withKind,
    withValue,
    type EditorState,
} from './editor.js';
import { SCHEMA } from './testSchema.js';

const parsed = (over: Partial<OresmdParseResult> = {}): OresmdParseResult => ({
    success: true,
    message: '',
    kind: 'quote',
    instrument: 'capfloor',
    quote: 'rate',
    asset: 'ir',
    subject: 'EUR',
    fields: [{ name: 'term', text: '5Y' }],
    ...over,
});

const state = (over: Partial<EditorState> = {}): EditorState => ({
    kind: 'quote',
    instrument: 'capfloor',
    quote: 'rate',
    subject: 'EUR',
    values: { term: '5Y' },
    ...over,
});

describe('fromParse', () => {
    it('gives the state a parsed URI states', () => {
        expect(fromParse(parsed())).toEqual({
            kind: 'quote',
            instrument: 'capfloor',
            quote: 'rate',
            subject: 'EUR',
            values: { term: '5Y' },
        });
    });

    it('gives the empty state for a URI the codec refused, or a kind it does not know', () => {
        expect(fromParse(parsed({ success: false, message: 'no' }))).toEqual(EMPTY);
        expect(fromParse(parsed({ kind: 'curve' }))).toEqual(EMPTY);
    });
});

describe('rowOf', () => {
    it('is null until an instrument the schema holds is chosen', () => {
        expect(rowOf(SCHEMA, EMPTY)).toBeNull();
        expect(rowOf(SCHEMA, state({ instrument: 'nonsense' }))).toBeNull();
    });

    it('leaves the path field out of the others, and keeps the order of the key', () => {
        const row = rowOf(SCHEMA, state());
        expect(row?.subject).toBe('index_name');
        expect(row?.asset).toBe('ir');
        expect(row?.fields.map((field) => field.name)).toEqual(['cap_floor', 'term']);
    });

    it('carries the optional flag, the role and the closed set of a field', () => {
        const [capFloor, term] = rowOf(SCHEMA, state())?.fields ?? [];
        expect(capFloor).toMatchObject({
            optional: true,
            role: 'identity',
            codes: ['Cap', 'Floor'],
        });
        expect(term).toMatchObject({ optional: false, role: 'coordinate', codes: [] });
    });

    it('holds only the identity fields of a series', () => {
        const row = rowOf(SCHEMA, state({ kind: 'series' }));
        expect(row?.fields.map((field) => field.name)).toEqual(['cap_floor']);
    });

    it('draws a fixing from the index family, with the optional fields marked', () => {
        const row = rowOf(SCHEMA, state({ kind: 'fixing', instrument: 'ibor' }));
        expect(row?.subject).toBe('ccy');
        expect(row?.fields).toEqual([
            { name: 'name', role: 'identity', optional: false, kind: 'text', codes: [] },
            { name: 'tenor', role: 'identity', optional: true, kind: 'text', codes: [] },
        ]);
        expect(rowOf(SCHEMA, state({ kind: 'fixing' }))).toBeNull();
    });
});

describe('composeRequestOf', () => {
    it('has nothing to ask until an instrument and a quote type are chosen', () => {
        expect(composeRequestOf(SCHEMA, EMPTY)).toBeNull();
        expect(composeRequestOf(SCHEMA, state({ quote: '' }))).toBeNull();
    });

    it("sends the row's fields in order, blank where the person typed nothing", () => {
        expect(composeRequestOf(SCHEMA, state())).toEqual({
            kind: 'quote',
            instrument: 'capfloor',
            quote: 'rate',
            subject: 'EUR',
            fields: [
                { name: 'cap_floor', text: '' },
                { name: 'term', text: '5Y' },
            ],
        });
    });

    it('does not send a value typed under another instrument', () => {
        const request = composeRequestOf(SCHEMA, state({ values: { term: '5Y', month: 'JAN' } }));
        expect(request?.fields.map((field) => field.name)).toEqual(['cap_floor', 'term']);
    });

    it('sends a fixing with no quote type, and asks without one', () => {
        const request = composeRequestOf(
            SCHEMA,
            state({
                kind: 'fixing',
                instrument: 'ibor',
                quote: 'rate',
                subject: 'EUR',
                values: { name: 'EURIBOR' },
            }),
        );
        expect(request).toMatchObject({ kind: 'fixing', quote: '', subject: 'EUR' });
    });
});

describe('the transitions', () => {
    it('keeps the instrument between a quote and a series, and clears it for a fixing', () => {
        expect(withKind(state(), 'series').instrument).toBe('capfloor');
        expect(withKind(state(), 'fixing').instrument).toBe('');
        expect(withKind(state({ kind: 'fixing', instrument: 'ibor' }), 'quote').instrument).toBe(
            '',
        );
        const same = state();
        expect(withKind(same, 'quote')).toBe(same);
    });

    it('sets one value and keeps the rest', () => {
        const next = withValue(state(), 'cap_floor', 'Cap');
        expect(next.values).toEqual({ term: '5Y', cap_floor: 'Cap' });
        expect(withValue(next, 'term', '').values.term).toBe('');
    });

    it('keys a request by its text, and nothing by the empty string', () => {
        expect(keyOf(null)).toBe('');
        expect(keyOf(composeRequestOf(SCHEMA, state()))).toBe(
            keyOf(composeRequestOf(SCHEMA, state())),
        );
        expect(keyOf(composeRequestOf(SCHEMA, state()))).not.toBe(
            keyOf(composeRequestOf(SCHEMA, state({ subject: 'USD' }))),
        );
    });
});
