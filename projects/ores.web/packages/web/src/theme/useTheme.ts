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

import { useSyncExternalStore } from 'react';
import { THEMES, getTheme, setTheme, subscribeToTheme, type Theme } from './theme.js';

/**
 * The palette in force, and the way to change it.
 *
 * Read from the store rather than from a provider: the palette is a fact about
 * the document, so a component tree that passed it down could disagree with the
 * attribute the stylesheet reads. A screen that never asks for it costs nothing.
 */
export function useTheme(): {
    readonly theme: Theme;
    readonly themes: readonly Theme[];
    readonly setTheme: (theme: Theme) => void;
} {
    const theme = useSyncExternalStore(subscribeToTheme, getTheme, getTheme);
    return { theme, themes: THEMES, setTheme };
}
