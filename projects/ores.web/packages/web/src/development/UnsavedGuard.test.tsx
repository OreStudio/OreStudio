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

import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { describe, expect, it } from 'vitest';
import { TranslationProvider } from '../i18n/Provider.js';
import { LeaveDialog, UnsavedGuard, leavesPage } from './UnsavedGuard.js';

const draw = (node: React.ReactNode): string =>
    renderToStaticMarkup(
        <TranslationProvider>
            <MemoryRouter>{node}</MemoryRouter>
        </TranslationProvider>,
    );

describe('the question before leaving with marks that are not saved', () => {
    it('asks, and offers to stay or to leave', () => {
        const html = draw(<LeaveDialog onStay={() => undefined} onLeave={() => undefined} />);
        expect(html).toContain('role="dialog"');
        expect(html).toContain('Leave without saving?');
        expect(html).toContain('Leaving drops them.');
        expect(html).toContain('>Stay</button>');
        expect(html).toContain('>Leave</button>');
    });

    it('makes Stay the main choice and Leave the one that warns', () => {
        const html = draw(<LeaveDialog onStay={() => undefined} onLeave={() => undefined} />);
        expect(html).toMatch(/bg-accent[^>]*>Stay</);
        expect(html).toMatch(/text-down[^>]*>Leave</);
    });

    it('draws nothing of its own under a plain router, so a page still renders', () => {
        expect(draw(<UnsavedGuard dirty />)).toBe('');
        expect(draw(<UnsavedGuard dirty={false} />)).toBe('');
    });
});

describe('what counts as leaving the page', () => {
    it('is a move to another path', () => {
        expect(leavesPage({ pathname: '/development/tests/1' }, { pathname: '/development' })).toBe(
            true,
        );
        expect(
            leavesPage({ pathname: '/development/tests/1' }, { pathname: '/development/tests/2' }),
        ).toBe(true);
    });

    it('is not a move to the same path, which is what a tab of the page does', () => {
        expect(
            leavesPage({ pathname: '/development/tests/1' }, { pathname: '/development/tests/1' }),
        ).toBe(false);
    });
});
