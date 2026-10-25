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
import { entriesOf, rowsOf } from './grid-report-results.js';

/**
 * The run result file: the engine's npv.csv, read by column name.
 *
 * The header the engine writes starts with a hash, and a counterparty name can
 * hold a comma inside quotes, so both are checked against a file shaped the way
 * the engine writes it.
 */

const CSV = [
    '#TradeId,TradeType,Maturity,MaturityTime,NPV,NpvCurrency,NPV(Base),BaseCurrency,Notional,NotionalCurrency,Notional(Base),NettingSet,CounterParty',
    '247917c2-b008-5482-930e-8e776fb73aa8,Bond,2030-05-05,4.0,7076092.454615,GBP,7076092.454615,GBP,7087413.92,GBP,7087413.92,NS-1,"Northwind Capital, Ltd"',
    '33bbf3d1-a35a-563a-b951-85f240c55db5,Bond,2031-05-05,5.0,1142973.707486,GBP,1142973.707486,GBP,1149669.91,GBP,1149669.91,NS-1,Acme Corporation Plc',
].join('\n');

/** A tar holding one file, in the plain ustar form the wrapper writes. */
function tarOf(name: string, content: string): Buffer {
    const body = Buffer.from(content, 'utf8');
    const header = Buffer.alloc(512);
    header.write(name, 0, 100, 'utf8');
    header.write('0000644', 100, 8, 'utf8');
    header.write(`${body.length.toString(8).padStart(11, '0')} `, 124, 12, 'utf8');
    header.write('00000000000 ', 136, 12, 'utf8');
    header.write('        ', 148, 8, 'utf8');
    header.write('0', 156, 1, 'utf8');
    header.write('ustar\0', 257, 6, 'utf8');
    const padding = Buffer.alloc((512 - (body.length % 512)) % 512);
    return Buffer.concat([header, body, padding, Buffer.alloc(1024)]);
}

describe('rowsOf', () => {
    it('reads the figures by name, past the hash the engine writes first', () => {
        const read = rowsOf(CSV);

        expect(read.rows).toHaveLength(2);
        expect(read.rows?.[0]).toEqual({
            trade_id: '247917c2-b008-5482-930e-8e776fb73aa8',
            trade_type: 'Bond',
            npv: '7076092.454615',
            npv_base: '7076092.454615',
            notional: '7087413.92',
            counterparty: 'Northwind Capital, Ltd',
        });
        expect(read.rows?.[1]?.npv).toBe('1142973.707486');
    });

    it('reads a file that starts with a byte-order mark', () => {
        const read = rowsOf(`\uFEFF${CSV}`);

        expect(read.rows).toHaveLength(2);
    });

    it('states the header it found when the figures are not in it', () => {
        const read = rowsOf('TradeType,Notional\nBond,1000');

        expect(read.rows).toBeNull();
        expect(read.header).toBe('TradeType,Notional');
    });

    it('reads a file with a header and no rows as no result, not as a failure', () => {
        const read = rowsOf('#TradeId,NPV\n');

        expect(read.rows).toEqual([]);
    });
});

describe('entriesOf', () => {
    it('finds the file the engine writes inside the archive', () => {
        const entries = entriesOf(tarOf('Output/npv.csv', CSV));

        expect([...entries.keys()]).toEqual(['Output/npv.csv']);
        expect(entries.get('Output/npv.csv')?.toString('utf8')).toBe(CSV);
    });
});
