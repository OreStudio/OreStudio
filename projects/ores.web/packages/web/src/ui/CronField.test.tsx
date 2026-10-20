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
import { renderToStaticMarkup } from 'react-dom/server';
import { TranslationProvider } from '../i18n/Provider.js';
import { CronBuilder, CronField } from './CronField.js';

function field(value: string, disabled = false): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <CronField
                label="Schedule"
                value={value}
                onChange={() => undefined}
                disabled={disabled}
            />
        </TranslationProvider>,
    );
}

function builder(expression: string): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <CronBuilder
                expression={expression}
                onApply={() => undefined}
                onCancel={() => undefined}
            />
        </TranslationProvider>,
    );
}

describe('the schedule field', () => {
    it('shows the schedule in words and cannot be typed into', () => {
        const html = field('30 18 * * 1-5');
        expect(html).toContain('value="At 18:30, Monday through Friday"');
        expect(html).toMatch(/<input[^>]*readOnly=""|<input[^>]*readonly=""/i);
        expect(html).not.toContain('30 18 * * 1-5');
    });

    it('puts even an expression the builder has no control for in words', () => {
        expect(field('5/15 * * * *')).toContain(
            'value="Every 15 minutes, starting at 5 minutes past the hour"',
        );
    });

    it('shows an expression the server would refuse as it is', () => {
        expect(field('0 0 9 * * ?')).toContain('value="0 0 9 * * ?"');
    });

    it('refuses what is not an expression, and says what it needs', () => {
        const html = field('0 0 9 * * ?');
        expect(html).toContain('aria-invalid="true"');
        expect(html).toContain('needs exactly 5');
    });

    it('does not call an empty field wrong; the form asks for it when it must', () => {
        const html = field('');
        expect(html).not.toContain('needs exactly 5');
        expect(html).toContain('placeholder="No schedule"');
    });

    it('offers Edit, and turns it off when the field is read only', () => {
        expect(field('0 * * * *')).toContain('Edit');
        expect(field('0 * * * *', true)).toMatch(/<button[^>]*disabled/);
    });
});

/** Which of the three groups is live, from the markup the builder draws. */
function liveGroup(html: string): string {
    const groups = html.split('<section').slice(1);
    const names = ['common', 'custom', 'advanced'];
    const live = groups.map((group, index) => ({
        name: names[index],
        on: group.includes('name="schedule-way" checked=""'),
        off: group.includes('<fieldset disabled=""'),
    }));
    expect(live.filter((group) => group.on)).toHaveLength(1);
    expect(live.filter((group) => !group.off)).toHaveLength(1);
    const on = live.find((group) => group.on);
    expect(on?.off).toBe(false);
    return on?.name ?? '';
}

describe('the schedule builder', () => {
    it('opens on the common schedules when the schedule is one of them', () => {
        const html = builder('30 18 * * 1-5');
        for (const name of ['Pre-market run', 'Intraday check', 'End of day batch']) {
            expect(html).toContain(name);
        }
        expect(html).toContain('At 18:30, Monday through Friday');
        expect(liveGroup(html)).toBe('common');
    });

    it('opens on the custom controls when the schedule is one of their shapes', () => {
        const html = builder('15 9 * * 1,3,5');
        expect(html).toContain('Once a day, at a time');
        expect(html).toContain('type="time"');
        expect(html).toContain('value="09:15"');
        expect(html).toContain('aria-pressed="true"');
        expect(html).toContain('aria-pressed="false"');
        expect(liveGroup(html)).toBe('custom');
    });

    it('offers an interval with its window', () => {
        const html = builder('*/15 8-17 * * 1-5');
        expect(html).toContain('Every 15 minutes, between 08:00 and 17:59, Monday through Friday');
        expect(html).toContain('between');
        expect(liveGroup(html)).toBe('custom');
    });

    it('opens on the cron expression for a schedule the other groups cannot show', () => {
        const html = builder('5/15 * * * *');
        expect(html).toContain('Advanced: the cron expression');
        expect(html).toContain('value="5/15 * * * *"');
        expect(html).toContain('Every 15 minutes, starting at 5 minutes past the hour');
        expect(html).not.toContain('needs exactly 5');
        expect(liveGroup(html)).toBe('advanced');
    });

    it('refuses an expression that does not parse: red, explained, and not applicable', () => {
        const html = builder('0 0 9 * * ?');
        expect(liveGroup(html)).toBe('advanced');
        expect(html).toContain('aria-invalid="true"');
        expect(html).toContain('border-down');
        expect(html).toContain('needs exactly 5');
        expect(html).toMatch(/<button[^>]*disabled=""[^>]*>.*Save/);
    });
});
