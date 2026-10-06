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
import type { ReactNode } from 'react';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { tabAddress, useTabs } from './Tabs.js';

const TABS = ['details', 'parties', 'people'] as const;

function Bar(): ReactNode {
    const { bar } = useTabs({ label: 'Tenant', tabs: TABS, titleOf: (tab) => tab });
    return bar;
}

function render(address: string): string {
    return renderToStaticMarkup(
        <MemoryRouter initialEntries={[address]}>
            <Bar />
        </MemoryRouter>,
    );
}

describe('the tabs of a page', () => {
    it('opens the tab the address names, and the first tab otherwise', () => {
        expect(render('/t?tab=people')).toMatch(/aria-selected="true"[^>]*>people</);
        expect(render('/t')).toMatch(/aria-selected="true"[^>]*>details</);
        expect(render('/t?tab=nonsense')).toMatch(/aria-selected="true"[^>]*>details</);
    });

    it('addresses a tab by its name alone, and the first tab by nothing', () => {
        expect(tabAddress(TABS, 'parties')).toEqual({ tab: 'parties' });
        expect(tabAddress(TABS, 'details')).toEqual({});
    });
});
