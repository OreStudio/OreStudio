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
import { loadedIso } from './loadedLabel.js';

/**
 * When the data on screen was loaded, written as quietly as a screen can.
 *
 * The one place a screen or a dialog says how old what it shows is. It is an ISO
 * timestamp in small, faint type, with no label: the tooltip says what it is. It
 * states the browser's clock at the moment the data arrived, and nothing more,
 * so it never disagrees with the clock that decides whether a screen is stale.
 * It draws nothing before the first load.
 */
export function LoadedAt({ at }: { readonly at: number | undefined }): ReactNode {
    const { t } = useTranslation();
    if (at === undefined || at <= 0) return null;
    const iso = loadedIso(at);
    return (
        <time
            dateTime={new Date(at).toISOString()}
            title={t('common.loadedAt', { time: iso })}
            className="block select-none font-mono text-[10px] leading-none tabular-nums text-ink-muted opacity-50"
        >
            {iso}
        </time>
    );
}
