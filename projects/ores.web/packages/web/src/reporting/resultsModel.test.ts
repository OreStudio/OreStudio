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
import { asFigure, toAggregateRows, toErrors, toTradeRows } from './resultsModel.js';

describe('toTradeRows', () => {
    it('reads a row whose fields are present', () => {
        const parsed = toTradeRows([
            { trade_id: 't1', trade_type: 'Bond', npv: '10.00', npv_currency: 'GBP' },
        ]);

        expect(parsed.unread).toBe(0);
        expect(parsed.rows[0]?.trade_id).toBe('t1');
        expect(parsed.rows[0]?.counterparty).toBe('');
    });

    it('reads a null field as empty rather than losing the row', () => {
        const parsed = toTradeRows([{ trade_id: 't1', counterparty_name: null, book_id: null }]);

        expect(parsed.rows).toHaveLength(1);
        expect(parsed.rows[0]?.counterparty_name).toBe('');
    });

    it('counts a row it cannot read rather than dropping it in silence', () => {
        const parsed = toTradeRows([{ trade_type: 'Bond' }, { trade_id: 't2' }]);

        expect(parsed.rows).toHaveLength(1);
        expect(parsed.unread).toBe(1);
    });
});

describe('toAggregateRows', () => {
    it('clamps a depth below zero and defaults the rest', () => {
        const parsed = toAggregateRows([{ key: 'b1', depth: -3, is_group: null }]);

        expect(parsed.rows[0]?.depth).toBe(0);
        expect(parsed.rows[0]?.is_group).toBe(false);
        expect(parsed.rows[0]?.totals).toEqual([]);
    });
});

describe('toErrors', () => {
    it('maps the wire words to the screen words and counts the rest', () => {
        const parsed = toErrors([
            { scope: 'Trade', subject_id: 't1', message: 'why' },
            { message: 7 },
        ]);

        expect(parsed.rows[0]).toEqual({
            scope: 'Trade',
            severity: '',
            message: 'why',
            cause: '',
            subjectId: 't1',
        });
        expect(parsed.unread).toBe(1);
    });
});

describe('asFigure', () => {
    it('groups and rounds a figure, and leaves text it cannot read as written', () => {
        expect(asFigure('1234.5')).toBe('1,234.50');
        expect(asFigure('n/a')).toBe('n/a');
    });
});
