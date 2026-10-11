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

/**
 * The strip along the top of a list that says something changed which the page
 * cannot point at.
 *
 * A change that names a row on the page marks that row. A change that names a
 * record on another page, or that was too many to name, has no row to mark, so
 * the list says so in general and leaves the reading of it to Refresh.
 */
export function ChangedElsewhere(): ReactNode {
    const { t } = useTranslation();
    return (
        <div
            role="status"
            className="stale-strip flex items-center gap-2 border-b border-line px-3 py-1.5 text-xs text-ink-muted"
        >
            <span aria-hidden className="size-2 rounded-full bg-stale" />
            {t('common.changedElsewhere')}
        </div>
    );
}
