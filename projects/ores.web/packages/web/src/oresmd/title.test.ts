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
import type { OresmdSchema } from '@ores/contracts';
import { rawLabel, seriesTitle, titleOrRaw } from './title.js';

const row = (code: string, title: string) => ({
    code,
    ore_name: code.toUpperCase(),
    title,
    asset: 'fx',
    subject: 'unit_ccy',
    fields: [],
});

const SCHEMA: OresmdSchema = {
    assets: ['fx'],
    instruments: [row('fx_spot', '{subject}/{ccy} spot'), row('capfloor', '{subject} cap/floor')],
    quotes: ['rate', 'rate_lnvol', 'none'],
    quote_titles: ['rate', 'log-normal volatility', ''],
    indexes: [],
};

const identity = (
    instrument_type: string,
    quote_type: string,
    fields: [string, string][] = [],
    scope = 'AUD',
) => ({
    scope,
    instrument_type,
    quote_type,
    fields: fields.map(([name, text]) => ({ name, text })),
});

describe('seriesTitle', () => {
    it('fills the braces from the scope and the fields and puts the quote words after', () => {
        expect(seriesTitle(SCHEMA, identity('fx_spot', 'rate', [['ccy', 'USD']]))).toBe(
            'AUD/USD spot rate',
        );
    });

    it('uses the words of the quote type, such as a volatility', () => {
        expect(seriesTitle(SCHEMA, identity('capfloor', 'rate_lnvol', [], 'EUR'))).toBe(
            'EUR cap/floor log-normal volatility',
        );
    });

    it('adds no words for the quote type none', () => {
        expect(seriesTitle(SCHEMA, identity('capfloor', 'none', [], 'EUR'))).toBe('EUR cap/floor');
    });

    it('lists the fields the title does not name behind a dot', () => {
        const title = seriesTitle(
            SCHEMA,
            identity('capfloor', 'rate', [
                ['index_tenor', '6M'],
                ['cap_floor', 'Cap'],
            ]),
        );
        expect(title).toBe('AUD cap/floor rate · index_tenor=6M, cap_floor=Cap');
    });

    it('has no title when a field it names has no value, because the title would have a gap', () => {
        expect(seriesTitle(SCHEMA, identity('fx_spot', 'rate'))).toBeUndefined();
        expect(seriesTitle(SCHEMA, identity('fx_spot', 'rate', [['ccy', '']]))).toBeUndefined();
    });

    it('has no title for a type the schema does not know', () => {
        expect(seriesTitle(SCHEMA, identity('mystery', 'rate'))).toBeUndefined();
    });
});

describe('rawLabel and titleOrRaw', () => {
    it('names a series by its words and the fields that tell it apart', () => {
        expect(
            rawLabel({ ...identity('fx_option', 'rate_lnvol', [['ccy', 'USD']]), asset: 'fx' }),
        ).toBe('fx / AUD / fx_option / rate_lnvol · ccy=USD');
        expect(rawLabel(identity('fx_option', 'rate_lnvol'))).toBe('AUD / fx_option / rate_lnvol');
    });

    it('falls back to the raw label before the schema is known and when no title fits', () => {
        const spot = identity('fx_spot', 'rate');
        expect(titleOrRaw(undefined, spot)).toBe('AUD / fx_spot / rate');
        expect(titleOrRaw(SCHEMA, spot)).toBe('AUD / fx_spot / rate');
        expect(titleOrRaw(SCHEMA, identity('fx_spot', 'rate', [['ccy', 'USD']]))).toBe(
            'AUD/USD spot rate',
        );
    });
});
