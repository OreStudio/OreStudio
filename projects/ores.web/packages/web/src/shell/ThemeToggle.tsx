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

import type { ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { useTheme } from '../theme/useTheme.js';
import { Icon } from '../ui/Icon.js';

/**
 * The palette switch in the header.
 *
 * It draws the palette it will switch to rather than the one in force: a sun
 * while the screen is dark, and a moon while it is light. That is the idiom
 * every reader of a theme switch already knows, and the label says the same
 * thing in words for a person arriving at it with a screen reader.
 */
export function ThemeToggle(): ReactNode {
    const { t } = useTranslation();
    const { theme, setTheme } = useTheme();
    const next = theme === 'dark' ? 'light' : 'dark';
    const label = next === 'light' ? t('shell.theme.toLight') : t('shell.theme.toDark');

    return (
        <button
            type="button"
            aria-label={label}
            title={label}
            onClick={() => {
                setTheme(next);
            }}
            className="flex size-9 items-center justify-center rounded-full text-ink-muted hover:bg-surface-hover hover:text-ink focus-visible:outline focus-visible:outline-2 focus-visible:outline-accent"
        >
            <Icon name={next === 'light' ? 'sun' : 'moon'} />
        </button>
    );
}
