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
import { cronText, cronValid, describeCron, parseCron } from './cron.js';

describe('a cron expression', () => {
    it.each([
        '* * * * *',
        '0 * * * *',
        '*/10 * * * *',
        '*/30 8-17 * * 1-5',
        '0 7 * * 1-5',
        '0 9 1,15 * *',
        '0 9 * jan,JUL mon',
        '5/15 * * * *',
        '0 0 * * 7',
    ])('is valid: %s', (expression) => {
        expect(cronValid(expression)).toBe(true);
    });

    it.each([
        '',
        '* * * *',
        '* * * * * *',
        '60 * * * *',
        '* 24 * * *',
        '* * 0 * *',
        '* * 32 * *',
        '* * * 13 *',
        '* * * * 8',
        '*/0 * * * *',
        '1-2-3 * * * *',
        '0 0 9 * * ?',
        'a b c d e',
    ])('is refused: "%s"', (expression) => {
        expect(cronValid(expression)).toBe(false);
    });

    it('reads a run of spaces as one separator, as the server does', () => {
        expect(cronValid('0   9 *  * 1-5')).toBe(true);
    });
});

describe('the builder models', () => {
    it.each([
        '* * * * *',
        '0 * * * *',
        '*/15 * * * *',
        '5/15 2/4 * * *',
        '0 7 * * 1-5',
        '0 9 1,15 * *',
        '30 6 * 1,6,9 0,6',
    ])('write back what they read: %s', (expression) => {
        expect(cronText(parseCron(expression))).toBe(expression);
    });

    it('keeps a field the three ways cannot state, until it is edited', () => {
        const expression = '0 9 1-5,10 * *';
        const models = parseCron(expression);
        expect(models[2]?.kept).toBe('1-5,10');
        expect(cronText(models)).toBe(expression);
    });

    it('starts from the top of every hour when the text is not five parts', () => {
        expect(cronText(parseCron('0 0 9 * * ?'))).toBe('0 * * * *');
    });

    it('writes a ticked set, and writes all or none as every value', () => {
        const [first] = parseCron('1,3 * * * *');
        expect(first?.picked).toEqual([1, 3]);
        expect(cronText(parseCron('0,1,2,3,4,5,6 * * * *'))).toBe('0,1,2,3,4,5,6 * * * *');
        expect(cronText([{ ...parseCron('* * * * *')[0]!, mode: 'choice', picked: [] }])).toBe(
            '* * * * *',
        );
    });
});

describe('the sentence for an expression', () => {
    it('names the common shapes', () => {
        expect(describeCron('* * * * *')?.key).toBe('cron.says.everyMinute');
        expect(describeCron('0 * * * *')?.key).toBe('cron.says.everyHour');
        expect(describeCron('0 7 * * *')).toEqual({
            key: 'cron.says.daily',
            values: { time: '07:00' },
        });
        expect(describeCron('0 7 * * 1-5')?.key).toBe('cron.says.weekdays');
        expect(describeCron('0 17 * * 1')).toEqual({
            key: 'cron.says.weekly',
            values: { time: '17:00' },
            weekday: 1,
        });
        expect(describeCron('0 9 15 * *')).toEqual({
            key: 'cron.says.monthly',
            values: { time: '09:00', day: '15' },
        });
    });

    it('reads Sunday as 0 or 7', () => {
        expect(describeCron('0 9 * * 7')?.weekday).toBe(0);
    });

    it('says nothing of an expression that is not a common shape or not valid', () => {
        expect(describeCron('*/10 8-17 * * 1-5')).toBeUndefined();
        expect(describeCron('nonsense')).toBeUndefined();
    });
});
