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
import { enFlat } from '../i18n/locales/en.js';
import { frFlat } from '../i18n/locales/fr.js';
import { ptFlat } from '../i18n/locales/pt.js';
import { interpolate } from '../i18n/translate.js';
import {
    PRESETS,
    cronOf,
    cronValid,
    describeCron,
    parseSchedule,
    problemWithCron,
    type Say,
} from './cron.js';

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

describe('the schedules the builder states', () => {
    it.each([
        '30 18 * * 1-5',
        '0 7 * * *',
        '15 9 * * 1,3,5',
        '0 6 * * 0,6',
        '0 9 15 * *',
        '*/15 * * * *',
        '*/5 8-17 * * 1-5',
        '0 * * * *',
        '0 8-18 * * 1-5',
        '0 */2 * * *',
        '0 8-18/4 * * 1-5',
    ])('write back as they were read: %s', (expression) => {
        const schedule = parseSchedule(expression);
        expect(schedule).toBeDefined();
        expect(cronOf(schedule!)).toBe(expression);
    });

    it.each([
        '5/15 * * * *',
        '0 9 1,15 * *',
        '0 9 * jan mon',
        '*/7 * * * *',
        '0 9 1-5 * *',
        '0 9 15 * 1',
        '0 0 * * 7',
        '0 0 9 * * ?',
    ])('are not claimed for an expression of another shape: %s', (expression) => {
        expect(parseSchedule(expression)).toBeUndefined();
    });

    it('reads the presets as themselves', () => {
        for (const preset of PRESETS) {
            expect(cronOf(parseSchedule(preset.expression)!)).toBe(preset.expression);
        }
    });

    it('writes no days, and all of them, as every day', () => {
        const base = parseSchedule('0 7 * * *')!;
        expect(cronOf({ ...base, days: [] })).toBe('0 7 * * *');
        expect(cronOf({ ...base, days: [0, 1, 2, 3, 4, 5, 6] })).toBe('0 7 * * *');
        expect(cronOf({ ...base, days: [5, 3, 1] })).toBe('0 7 * * 1,3,5');
    });
});

describe('the words for an expression', () => {
    it('come from one place and read as a sentence in English', () => {
        expect(describeCron('30 18 * * 1-5', 'en')).toBe('At 18:30, Monday through Friday');
        expect(describeCron('*/15 * * * *', 'en')).toBe('Every 15 minutes');
        expect(describeCron('0 9 15 * *', 'en')).toBe('At 09:00, on day 15 of the month');
    });

    it('come in the language asked for', () => {
        expect(describeCron('*/5 * * * *', 'fr')).toBe('Toutes les 5 minutes');
        expect(describeCron('*/5 * * * *', 'pt')).not.toBe(describeCron('*/5 * * * *', 'en'));
    });

    it('cover an expression the builder has no control for', () => {
        expect(describeCron('5/15 * * * *', 'en')).toBeDefined();
    });

    it('are none for an expression the server would refuse', () => {
        expect(describeCron('0 0 9 * * ?', 'en')).toBeUndefined();
        expect(describeCron('nonsense', 'en')).toBeUndefined();
        expect(describeCron('', 'en')).toBeUndefined();
    });
});

const sayIn =
    (flat: Record<string, string>): Say =>
    (key, values = {}) =>
        interpolate(flat[key] ?? key, values);

const say = sayIn(enFlat);

describe('problemWithCron', () => {
    it('says how many parts there are', () => {
        expect(problemWithCron('* * *', 'en', say)).toBe(
            'Expression has 3 parts. The scheduler needs exactly 5: minute, hour, day of month, month and day of week.',
        );
        expect(problemWithCron('0 0 9 * * ?', 'en', say)).toContain('has 6 parts');
    });

    it('names the part and the value that is out of range', () => {
        expect(problemWithCron('0 5 * * 1-511', 'en', say)).toBe(
            'The day of week part "1-511" is not valid: 511 is outside 0 to 7.',
        );
        expect(problemWithCron('61 * * * *', 'en', say)).toContain('61 is outside 0 to 59');
        expect(problemWithCron('0 5 * * foo', 'en', say)).toContain('"foo" is not a number');
    });

    it('speaks French and Portuguese', () => {
        expect(problemWithCron('61 * * * *', 'fr', sayIn(frFlat))).toBe(
            "La partie minute « 61 » n'est pas valide : 61 est hors de 0 à 59.",
        );
        expect(problemWithCron('* * *', 'pt', sayIn(ptFlat))).toContain('3 partes');
    });

    it('says nothing of a valid expression', () => {
        expect(problemWithCron('0 8 * * 1-5', 'en', say)).toBeUndefined();
    });
});
