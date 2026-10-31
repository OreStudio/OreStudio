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

/**
 * The palettes this interface reads in.
 *
 * The dark one is the default, because the application is read for long
 * sessions and that is the palette it was designed in. The light one is the same
 * palette with its role tokens restated, so every screen reads in both without
 * a screen knowing which one it is in: the stylesheet says what a surface, a
 * line or a piece of ink is, and the palette says what colour that is.
 */
export const THEMES = ['light', 'dark'] as const;

export type Theme = (typeof THEMES)[number];

/** The palette a person gets before they choose one. */
export const DEFAULT_THEME: Theme = 'dark';

/** Where the document states the palette, which is where the stylesheet reads it. */
export const THEME_ATTRIBUTE = 'data-theme';

/**
 * Remembering the palette a person chose.
 *
 * Stored per browser rather than per account: it is a setting about the screen
 * in front of a person, and a signed-out visitor reads a screen too.
 */
const STORAGE_KEY = 'ores.web.theme';

export function isTheme(value: string): value is Theme {
    return (THEMES as readonly string[]).includes(value);
}

export function readStoredTheme(): Theme | undefined {
    try {
        const stored = globalThis.localStorage?.getItem(STORAGE_KEY);
        return stored === null || stored === undefined || !isTheme(stored) ? undefined : stored;
    } catch {
        // Storage can be unavailable, in a private window or with a policy
        // against it. A missing preference is not worth failing over.
        return undefined;
    }
}

export function storeTheme(theme: Theme): void {
    try {
        globalThis.localStorage?.setItem(STORAGE_KEY, theme);
    } catch {
        // As above: the choice simply does not persist.
    }
}

/**
 * What the operating system asks for.
 *
 * The question is asked as a preference rather than as a brightness, so it says
 * nothing when the machine has no opinion.
 */
export function systemTheme(): Theme | undefined {
    const query = globalThis.matchMedia?.('(prefers-color-scheme: light)');
    if (query === undefined) {
        return undefined;
    }
    return query.matches ? 'light' : 'dark';
}

/** The palette to start in: the stored choice, then the system's, then the default. */
export function initialTheme(): Theme {
    return readStoredTheme() ?? systemTheme() ?? DEFAULT_THEME;
}

/**
 * Puts a palette on the document.
 *
 * The attribute rather than a class, so the stylesheet's palette block selects
 * on it and nothing else has to agree on a name. The colour scheme goes with it,
 * so the browser paints its own furniture — scrollbars, form controls, the
 * canvas behind the page — in the same palette.
 */
export function applyTheme(theme: Theme): void {
    const root = globalThis.document?.documentElement;
    if (root === undefined) {
        return;
    }
    root.setAttribute(THEME_ATTRIBUTE, theme);
    root.style.colorScheme = theme;
}

/*
 * The palette in force, as one value for the whole document.
 *
 * It lives here as a module store rather than in a React context because it is
 * one fact about the document, not a value a component tree passes down: the
 * stylesheet reads the attribute, the control reads the same attribute and
 * writes it, and a second copy in a provider could disagree with it. A component
 * that holds the current palette subscribes to this store.
 */
let current: Theme | undefined;
const listeners = new Set<() => void>();

/** The palette in force, deciding it the first time anything asks. */
export function getTheme(): Theme {
    if (current === undefined) {
        current = initialTheme();
        applyTheme(current);
    }
    return current;
}

/** Puts a palette in force, remembers it, and tells whoever is drawing. */
export function setTheme(theme: Theme): void {
    if (theme === current) {
        return;
    }
    current = theme;
    storeTheme(theme);
    applyTheme(theme);
    for (const listener of listeners) {
        listener();
    }
}

/** Watches the palette for a change. Returns the way to stop watching. */
export function subscribeToTheme(listener: () => void): () => void {
    listeners.add(listener);
    return () => {
        listeners.delete(listener);
    };
}
