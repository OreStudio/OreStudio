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
import type {
    CurveSeriesList,
    ListedSeries,
    OresmdParseResult,
    OresmdSchema,
} from '@ores/contracts';
import { seriesPageState, type SeriesPageReads, type SeriesRead } from './seriesState.js';

const field = (name: string) => ({
    name,
    role: 'identity',
    may_be_none: false,
    kind: 'code',
    codes: [],
});

const schema: OresmdSchema = {
    assets: ['FX', 'IR'],
    instruments: [
        {
            code: 'fx_spot',
            ore_name: 'FX spot',
            title: '{subject}/{ccy} {quote}',
            asset: 'FX',
            subject: 'AUD',
            fields: [field('ccy')],
        },
        {
            code: 'ir_swap',
            ore_name: 'IR swap',
            title: '{subject} {tenor} {quote}',
            asset: 'IR',
            subject: 'AUD',
            fields: [field('tenor')],
        },
    ],
    quotes: ['rate'],
    quote_titles: ['Rate'],
    indexes: [],
};

const spot: ListedSeries = {
    series_id: 'series-spot',
    identity: {
        asset: 'FX',
        scope: 'AUD',
        instrument_type: 'fx_spot',
        quote_type: 'rate',
        party_id: '',
        fields: [{ name: 'ccy', text: 'USD' }],
    },
    axes: [],
};

const swap: ListedSeries = {
    series_id: 'series-swap',
    identity: {
        asset: 'IR',
        scope: 'AUD',
        instrument_type: 'ir_swap',
        quote_type: 'rate',
        party_id: '',
        fields: [{ name: 'tenor', text: '5Y' }],
    },
    axes: ['tenor'],
};

const list: CurveSeriesList = { series: [spot, swap], truncated: false };

const spotParts: OresmdParseResult = {
    success: true,
    message: '',
    kind: 'series',
    instrument: 'fx_spot',
    quote: 'rate',
    asset: 'FX',
    subject: 'AUD',
    fields: [{ name: 'ccy', text: 'USD' }],
};

const answered = <T>(data: T): SeriesRead<T> => ({ kind: 'answer', data });
const waiting = <T>(): SeriesRead<T> => ({ kind: 'waiting' });
const failed = <T>(message: string): SeriesRead<T> => ({ kind: 'error', message });

const URL = 'oresmd://FX/AUD/fx_spot/ccy=USD/rate';

const reads = (over: Partial<SeriesPageReads>): SeriesPageReads => ({
    uri: URL,
    seriesId: '',
    parsed: answered(spotParts),
    schema: answered(schema),
    list: answered(list),
    ...over,
});

describe('seriesPageState', () => {
    it('says the address names no series when it carries neither a URL nor an id', () => {
        expect(seriesPageState(reads({ uri: '' }))).toEqual({ kind: 'no-uri' });
    });

    it('says the address names no series even while a read has failed', () => {
        expect(seriesPageState(reads({ uri: '', list: failed('the list broke') }))).toEqual({
            kind: 'no-uri',
        });
    });

    it('says the first failed read, because the other two cannot change its words', () => {
        expect(
            seriesPageState(
                reads({
                    parsed: failed('the parse broke'),
                    schema: failed('the schema broke'),
                    list: failed('the list broke'),
                }),
            ),
        ).toEqual({ kind: 'error', message: 'the parse broke' });
    });

    it('says the schema failure when the parse answered', () => {
        expect(
            seriesPageState(
                reads({ schema: failed('the schema broke'), list: failed('the list broke') }),
            ),
        ).toEqual({ kind: 'error', message: 'the schema broke' });
    });

    it('says the list failure when the parse and the schema answered', () => {
        expect(seriesPageState(reads({ list: failed('the list broke') }))).toEqual({
            kind: 'error',
            message: 'the list broke',
        });
    });

    it('waits while the series list has no answer', () => {
        expect(seriesPageState(reads({ list: waiting() }))).toEqual({ kind: 'loading' });
    });

    it('waits while the parts of the URL have not arrived', () => {
        expect(seriesPageState(reads({ parsed: waiting() }))).toEqual({ kind: 'loading' });
    });

    it('waits while the schema has not arrived', () => {
        expect(seriesPageState(reads({ schema: waiting() }))).toEqual({ kind: 'loading' });
    });

    it('uses the codec words for a URL it refuses', () => {
        expect(
            seriesPageState(
                reads({
                    parsed: answered({ ...spotParts, success: false, message: 'unknown segment' }),
                }),
            ),
        ).toEqual({ kind: 'refused', message: 'unknown segment' });
    });

    it('says a fixing names no series', () => {
        expect(
            seriesPageState(reads({ parsed: answered({ ...spotParts, kind: 'fixing' }) })),
        ).toEqual({ kind: 'fixing' });
    });

    it('names the instrument type the schema does not know', () => {
        expect(
            seriesPageState(reads({ parsed: answered({ ...spotParts, instrument: 'fx_cap' }) })),
        ).toEqual({ kind: 'unknown', name: 'fx_cap' });
    });

    it('says the series is not listed when the party does not have it', () => {
        expect(
            seriesPageState(
                reads({
                    parsed: answered({ ...spotParts, fields: [{ name: 'ccy', text: 'JPY' }] }),
                }),
            ),
        ).toEqual({ kind: 'not-listed' });
    });

    it('sends a series with an axis to the Evolution page', () => {
        const swapParts: OresmdParseResult = {
            ...spotParts,
            instrument: 'ir_swap',
            asset: 'IR',
            fields: [{ name: 'tenor', text: '5Y' }],
        };
        expect(seriesPageState(reads({ parsed: answered(swapParts) }))).toEqual({
            kind: 'has-shape',
            listed: swap,
        });
    });

    it('draws the values of a series with no axis', () => {
        expect(seriesPageState(reads({}))).toEqual({ kind: 'values', listed: spot });
    });

    it('finds the series the address names by its id, with no URL at all', () => {
        expect(seriesPageState(reads({ uri: '', seriesId: spot.series_id }))).toEqual({
            kind: 'values',
            listed: spot,
        });
        expect(seriesPageState(reads({ uri: '', seriesId: swap.series_id }))).toEqual({
            kind: 'has-shape',
            listed: swap,
        });
    });

    it('says a series id the party does not hold is not listed', () => {
        expect(seriesPageState(reads({ uri: '', seriesId: 'series-gone' }))).toEqual({
            kind: 'not-listed',
        });
    });

    it('waits for the series list with an id before it can say the series is not listed', () => {
        expect(
            seriesPageState(reads({ uri: '', seriesId: 'series-gone', list: waiting() })),
        ).toEqual({ kind: 'loading' });
    });

    it('lets a URL the codec refuses outrank the series id, as the address names two series', () => {
        expect(
            seriesPageState(reads({ seriesId: swap.series_id, parsed: failed('the parse broke') })),
        ).toEqual({ kind: 'error', message: 'the parse broke' });
        expect(
            seriesPageState(
                reads({
                    seriesId: swap.series_id,
                    parsed: answered({ ...spotParts, success: false, message: 'unknown segment' }),
                }),
            ),
        ).toEqual({ kind: 'refused', message: 'unknown segment' });
    });
});
