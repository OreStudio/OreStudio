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

import { afterEach, describe, expect, it, vi } from 'vitest';
import {
    DEFAULT_THEME,
    THEME_ATTRIBUTE,
    applyTheme,
    getTheme,
    initialTheme,
    isTheme,
    readStoredTheme,
    setTheme,
    storeTheme,
    subscribeToTheme,
    systemTheme,
} from './theme.js';

/**
 * The theme module's own contract.
 *
 * The choice is three facts in order: what the person stored, what the machine
 * asks for, and the palette the application is designed in. A test per fact
 * rather than per function, because the order is what a reader needs to know.
 */
function withStorage(initial: Record<string, string> = {}): Map<string, string> {
    const store = new Map(Object.entries(initial));
    vi.stubGlobal('localStorage', {
        getItem: (key: string) => store.get(key) ?? null,
        setItem: (key: string, value: string) => {
            store.set(key, value);
        },
    });
    return store;
}

afterEach(() => {
    vi.unstubAllGlobals();
});

describe('the theme', () => {
    it('stores the choice under the key the page reads before it paints', () => {
        const store = withStorage();
        storeTheme('light');
        // The inline script in index.html reads this exact key, so the two
        // cannot drift apart without this failing.
        expect(store.get('ores.web.theme')).toBe('light');
    });

    it('reads back only a palette it knows', () => {
        withStorage({ 'ores.web.theme': 'light' });
        expect(readStoredTheme()).toBe('light');

        withStorage({ 'ores.web.theme': 'sepia' });
        expect(readStoredTheme()).toBeUndefined();
    });

    it('starts in the stored palette before the system one', () => {
        withStorage({ 'ores.web.theme': 'light' });
        vi.stubGlobal('matchMedia', () => ({ matches: false }));
        expect(initialTheme()).toBe('light');
    });

    it('starts in the system palette when the person has not chosen', () => {
        withStorage();
        vi.stubGlobal('matchMedia', () => ({ matches: true }));
        expect(systemTheme()).toBe('light');
        expect(initialTheme()).toBe('light');
    });

    it('starts in the palette the application is designed in when nothing speaks', () => {
        withStorage();
        vi.stubGlobal('matchMedia', undefined);
        expect(systemTheme()).toBeUndefined();
        expect(initialTheme()).toBe(DEFAULT_THEME);
    });

    it('states the palette and the colour scheme on the document', () => {
        const root = {
            attributes: new Map<string, string>(),
            style: {} as Record<string, string>,
            setAttribute(name: string, value: string) {
                this.attributes.set(name, value);
            },
        };
        vi.stubGlobal('document', { documentElement: root });

        applyTheme('light');

        expect(root.attributes.get(THEME_ATTRIBUTE)).toBe('light');
        expect(root.style.colorScheme).toBe('light');
    });

    it('tells whoever is drawing when the palette changes, and stops when asked', () => {
        withStorage();
        vi.stubGlobal('matchMedia', undefined);
        vi.stubGlobal('document', {
            documentElement: { setAttribute: () => undefined, style: {} },
        });

        const seen: string[] = [];
        const stop = subscribeToTheme(() => {
            seen.push(getTheme());
        });
        setTheme('light');
        setTheme('dark');
        stop();
        setTheme('light');

        // One notice per change, and none after the subscription ends.
        expect(seen).toEqual(['light', 'dark']);
        expect(getTheme()).toBe('light');
    });

    it('knows the two palettes and nothing else', () => {
        expect(isTheme('light')).toBe(true);
        expect(isTheme('dark')).toBe(true);
        expect(isTheme('sepia')).toBe(false);
    });
});
