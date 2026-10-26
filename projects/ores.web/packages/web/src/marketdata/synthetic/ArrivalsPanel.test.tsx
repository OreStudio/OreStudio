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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { describe, expect, it } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import type { RecordRow } from '../../api/client.js';
import { TranslationProvider } from '../../i18n/Provider.js';
import { ArrivalsPanel } from './ArrivalsPanel.js';

const noSave = (): Promise<void> => Promise.resolve();

function feed(over: Record<string, unknown> = {}): RecordRow {
    return {
        id: 'feed-1',
        version: 1,
        arrival_mode: 'hawkes',
        branching_ratio: 0.6,
        decay: 20,
        sensitivity: 1.3,
        ...over,
    } as unknown as RecordRow;
}

function render(row: RecordRow, mayWrite = true): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <ArrivalsPanel feed={row} mayWriteFeed={mayWrite} save={noSave} />
        </TranslationProvider>,
    );
}

/** Whether the input that follows the given label is disabled. */
function disabledAfter(html: string, label: string): boolean {
    const at = html.indexOf(label);
    if (at < 0) return false;
    const input = html.indexOf('<input', at);
    if (input < 0) return false;
    const tag = html.slice(input, html.indexOf('>', input));
    return /\sdisabled(=|\s|\/?>)/.test(tag);
}

describe('a feed’s arrivals tab', () => {
    it('shows the model and the three numbers', () => {
        const html = render(feed());
        expect(html).toContain('Tick arrival model');
        expect(html).toContain('Branching ratio');
        expect(html).toContain('Decay, per second');
        expect(html).toContain('Sensitivity to the market');
    });

    it('enables the Hawkes numbers for a Hawkes feed', () => {
        const html = render(feed());
        expect(disabledAfter(html, 'Branching ratio')).toBe(false);
    });

    it('disables the Hawkes numbers for a model that does not read them', () => {
        // The values stay in the row, so a person switching models sees what
        // will be used; they are disabled rather than hidden.
        const html = render(feed({ arrival_mode: 'periodic' }));
        expect(disabledAfter(html, 'Branching ratio')).toBe(true);
        expect(html).toContain('0.6');
    });

    it('refuses to edit when the caller may not write', () => {
        const html = render(feed(), false);
        expect(html).toContain('disabled');
    });
});
