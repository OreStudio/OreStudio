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

import { describe, expect, it } from 'vitest';
import {
    feedOf,
    labelsOf,
    pageOf,
    QuoteBoard,
    type InstrumentShape,
    type QuoteTick,
} from './quotes.js';

const BLOOMBERG = '0a42775e-3da7-4c61-a732-6b62cb9f948f';
const LSEG = 'c0f53bf1-a03a-4557-9906-7213091fcd82';
const GBPUSD = 'oresmd://fx/GBP?type=quote&instrument=fx_spot&quote=rate&ccy=USD';
const SOFR_5Y =
    'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=3M&term=5Y';

const SHAPES = new Map<string, InstrumentShape>([
    [
        'fx_spot',
        {
            assetClass: 'fx',
            roles: new Map([
                ['unit_ccy', 'identity'],
                ['ccy', 'identity'],
            ]),
        },
    ],
    [
        'ir_swap',
        {
            assetClass: 'interest_rates',
            roles: new Map([
                ['ccy', 'identity'],
                ['index_name', 'identity'],
                ['fwd_start', 'identity'],
                ['tenor', 'identity'],
                ['term', 'coordinate'],
            ]),
        },
    ],
]);

function tick(uri: string, value: string): QuoteTick {
    return {
        oresmd_uri: uri,
        value,
        observation_time: '2026-11-19 12:00:00.000000Z',
        source: 'bloomberg-a',
    };
}

function subject(provider: string, feed: string): string {
    return `ores.dev.test.marketdata.v1.ops.market_tick.${provider}.${feed}.fx_spot.x._`;
}

function heardBoard(): QuoteBoard {
    const board = new QuoteBoard(() => () => undefined);
    board.take(subject('bloomberg', BLOOMBERG), tick(GBPUSD, '1.30810'));
    board.take(subject('lseg', LSEG), tick(GBPUSD, '1.30825'));
    board.take(subject('bloomberg', BLOOMBERG), tick(SOFR_5Y, '4.1533'));
    board.take(subject('bloomberg', BLOOMBERG), tick(GBPUSD, '1.30822'));
    return board;
}

describe('the quote board', () => {
    it('listens once on every feed subject', () => {
        const heard: string[] = [];
        const board = new QuoteBoard((s) => {
            heard.push(s);
            return () => undefined;
        });
        board.start();
        board.start();
        expect(heard).toEqual(['marketdata.v1.ops.market_tick.>']);
    });

    it('tries again at the next read when the transport cannot subscribe yet', () => {
        let connected = false;
        const heard: string[] = [];
        const board = new QuoteBoard((s) => {
            if (!connected) throw new Error('Cannot subscribe: not connected');
            heard.push(s);
            return () => undefined;
        });
        expect(() => board.start()).not.toThrow();
        connected = true;
        board.start();
        expect(heard).toEqual(['marketdata.v1.ops.market_tick.>']);
    });

    it('reads the provider and the feed from a subject, and nothing from another', () => {
        expect(feedOf(subject('bloomberg', BLOOMBERG))).toEqual({
            provider: 'bloomberg',
            feed: BLOOMBERG,
        });
        expect(feedOf('ores.dev.test.synthetic.v1.ops.tick.x')).toBeUndefined();
    });

    it('puts a newer quote of a datum on a feed in place of the older, keeping its value', () => {
        const rows = heardBoard().after(0, new Set([`bloomberg|${BLOOMBERG}`]));
        const gbp = rows.find((q) => q.uri === GBPUSD);
        expect(rows).toHaveLength(2);
        expect(gbp?.value).toBe('1.30822');
        expect(gbp?.previous).toBe('1.30810');
        expect(gbp?.sequence).toBe(4);
    });

    it('returns only the feeds a read names, and only what came after a sequence', () => {
        const board = heardBoard();
        expect(board.after(0, new Set([`lseg|${LSEG}`]))).toHaveLength(1);
        expect(board.after(3, new Set([`bloomberg|${BLOOMBERG}`, `lseg|${LSEG}`]))).toHaveLength(1);
        expect(board.feeds().get(`bloomberg|${BLOOMBERG}`)?.source).toBe('bloomberg-a');
    });

    it('states the sequence of the last quote it sent when a read is over the ceiling', () => {
        const rows = heardBoard().after(0, new Set([`bloomberg|${BLOOMBERG}`, `lseg|${LSEG}`]));
        const page = pageOf(rows, 4, 2);
        expect(page.rows).toHaveLength(2);
        expect(page.sequence).toBe(page.rows[1]?.sequence);
        expect(pageOf(rows, 4).sequence).toBe(4);
    });
});

describe('the labels of a quote', () => {
    it('name the type, the asset class, the series and the point of a pillar', () => {
        expect(labelsOf(SOFR_5Y, SHAPES)).toEqual({
            instrument_type: 'ir_swap',
            asset_class: 'interest_rates',
            series_uri:
                'oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=3M',
            series: 'USD 0D 3M',
            point: '5Y',
        });
    });

    it('give a datum with no coordinates no point', () => {
        expect(labelsOf(GBPUSD, SHAPES)).toEqual({
            instrument_type: 'fx_spot',
            asset_class: 'fx',
            series_uri: 'oresmd://fx/GBP?type=series&instrument=fx_spot&quote=rate&ccy=USD',
            series: 'GBP USD',
            point: '',
        });
    });

    it('keep a query pair with no value as it is', () => {
        expect(labelsOf(`${GBPUSD}&flag`, SHAPES)?.series_uri).toBe(
            'oresmd://fx/GBP?type=series&instrument=fx_spot&quote=rate&ccy=USD&flag',
        );
    });

    it('give nothing for a type the schema does not name', () => {
        expect(labelsOf('oresmd://fx/GBP?type=quote&instrument=nope', SHAPES)).toBeUndefined();
    });
});
