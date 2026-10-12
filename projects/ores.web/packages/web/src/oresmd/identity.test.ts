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
import type { CurveSeriesList, ListedSeries, OresmdParseResult } from '@ores/contracts';
import type { IdentityForm } from '../market/evolutionView.js';
import { evolutionRoute, seriesOfUrl, seriesVerdict } from './identity.js';
import { SCHEMA } from './testSchema.js';

const parts = (over: Partial<OresmdParseResult> = {}): OresmdParseResult => ({
    success: true,
    message: '',
    kind: 'quote',
    instrument: 'capfloor',
    quote: 'rate',
    asset: 'ir',
    subject: 'EUR',
    fields: [
        { name: 'cap_floor', text: 'Cap' },
        { name: 'term', text: '5Y' },
    ],
    ...over,
});

describe('seriesOfUrl', () => {
    it('names the series of a quote: the identity fields of the row, and no coordinate', () => {
        const named = seriesOfUrl(SCHEMA, parts());
        expect(named).toEqual({
            ok: true,
            form: {
                asset: 'ir',
                scope: 'EUR',
                instrumentType: 'capfloor',
                quoteType: 'rate',
                fields: 'cap_floor=Cap',
                partyId: '',
            },
        });
    });

    it('names the series of a series URL, which states only identity fields', () => {
        const named = seriesOfUrl(
            SCHEMA,
            parts({ kind: 'series', fields: [{ name: 'cap_floor', text: 'Floor' }] }),
        );
        expect(named.ok && named.form.fields).toBe('cap_floor=Floor');
    });

    it('leaves out an identity field the URL does not state, and a blank one', () => {
        const named = seriesOfUrl(SCHEMA, parts({ fields: [{ name: 'cap_floor', text: '' }] }));
        expect(named.ok && named.form.fields).toBe('');
    });

    it('joins several identity fields one to a line', () => {
        const named = seriesOfUrl(
            SCHEMA,
            parts({
                instrument: 'seasonality',
                subject: 'UKRPI',
                asset: 'inflation',
                fields: [{ name: 'seasonality_type', text: 'MULT' }],
            }),
        );
        expect(named.ok && named.form.fields).toBe('seasonality_type=MULT');
    });

    it('names no series for a fixing', () => {
        expect(seriesOfUrl(SCHEMA, parts({ kind: 'fixing', instrument: 'ibor' }))).toEqual({
            ok: false,
            reason: 'fixing',
            message: '',
        });
    });

    it("gives the codec's words for a URL it refused", () => {
        expect(seriesOfUrl(SCHEMA, parts({ success: false, message: 'no way' }))).toEqual({
            ok: false,
            reason: 'refused',
            message: 'no way',
        });
    });

    it('says so for an instrument the schema does not hold', () => {
        const named = seriesOfUrl(SCHEMA, parts({ instrument: 'nonsense' }));
        expect(named).toMatchObject({ ok: false, reason: 'unknown', message: 'nonsense' });
    });
});

describe('evolutionRoute', () => {
    it('puts the URL in the address, encoded', () => {
        expect(evolutionRoute('oresmd://ir/EUR?type=series&x=1')).toBe(
            '/market-data/evolution?uri=oresmd%3A%2F%2Fir%2FEUR%3Ftype%3Dseries%26x%3D1',
        );
    });
});

describe('seriesVerdict', () => {
    const form: IdentityForm = {
        asset: 'ir',
        scope: 'EUR',
        instrumentType: 'capfloor',
        quoteType: 'rate',
        fields: 'cap_floor=Cap',
        partyId: '',
    };
    const listed = (fields: readonly [string, string][], axes: string[]): ListedSeries => ({
        series_id: 'series-1',
        axes,
        identity: {
            asset: 'ir',
            scope: 'EUR',
            instrument_type: 'capfloor',
            quote_type: 'rate',
            fields: fields.map(([name, text]) => ({ name, text })),
        },
    });
    const listOf = (...series: ListedSeries[]): CurveSeriesList => ({ truncated: false, series });

    it('shows a series the party has when it declares an axis', () => {
        const held = listed([['cap_floor', 'Cap']], ['term']);
        expect(seriesVerdict(listOf(held), form)).toEqual({ kind: 'shown', listed: held });
    });

    it('has no shape for a series the party has when it declares no axis', () => {
        const held = listed([['cap_floor', 'Cap']], []);
        expect(seriesVerdict(listOf(held), form)).toEqual({ kind: 'no-shape', listed: held });
    });

    it('is not listed when the party has no series with that identity', () => {
        expect(seriesVerdict(listOf(), form)).toEqual({ kind: 'not-listed' });
        const other = listed([['cap_floor', 'Floor']], ['term']);
        expect(seriesVerdict(listOf(other), form)).toEqual({ kind: 'not-listed' });
    });

    it('needs the same identity fields on both sides, an extra one on either side is another series', () => {
        const extra = listed(
            [
                ['cap_floor', 'Cap'],
                ['ccy', 'USD'],
            ],
            ['term'],
        );
        expect(seriesVerdict(listOf(extra), form).kind).toBe('not-listed');
        const missing = listed([], ['term']);
        expect(seriesVerdict(listOf(missing), form).kind).toBe('not-listed');
    });

    it('ignores the order of the fields and blank lines in the form', () => {
        const held = listed(
            [
                ['cap_floor', 'Cap'],
                ['ccy', 'USD'],
            ],
            ['term'],
        );
        const written: IdentityForm = { ...form, fields: '\nccy=USD\n  cap_floor=Cap  \n' };
        expect(seriesVerdict(listOf(held), written).kind).toBe('shown');
    });
});
