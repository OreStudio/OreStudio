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

describe('the cron field', () => {
    it('shows the expression as text with a Build button', () => {
        const html = field('0 7 * * 1-5');
        expect(html).toContain('value="0 7 * * 1-5"');
        expect(html).toContain('Build…');
    });

    it('says in words what a common expression means', () => {
        expect(field('0 7 * * 1-5')).toContain('At 07:00, Monday through Friday');
    });

    it('refuses what is not an expression, and says what it needs', () => {
        const html = field('0 0 9 * * ?');
        expect(html).toContain('aria-invalid="true"');
        expect(html).toContain('five parts');
    });

    it('does not call an empty field wrong; the form asks for it when it must', () => {
        expect(field('')).not.toContain('five parts');
    });

    it('turns the Build button off when the field is read only', () => {
        expect(field('0 * * * *', true)).toMatch(/<button[^>]*disabled/);
    });
});

describe('the cron builder', () => {
    it('offers one tab for each part of the expression', () => {
        const html = builder('0 * * * *');
        for (const name of ['Minutes', 'Hours', 'Days of month', 'Month', 'Days of week']) {
            expect(html).toContain(`>${name}<`);
        }
    });

    it('states the expression it will apply, with its words', () => {
        const html = builder('0 9 * * *');
        expect(html).toContain('0 9 * * *');
        expect(html).toContain('At 09:00, every day');
    });

    it('offers the three ways to state a part', () => {
        const html = builder('*/15 * * * *');
        for (const way of ['Frequency', 'Range', 'Choice']) {
            expect(html).toContain(way);
        }
    });

    it('says when a part is one it cannot show, and keeps it', () => {
        const html = builder('0 9 1-5,10 * *');
        expect(html).toContain('0 9 1-5,10 * *');
    });
});
