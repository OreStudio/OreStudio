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
import { Icon } from './Icon.js';

/**
 * Whether the person may change what a panel shows: an icon and one word,
 * with the reason on hover. Screens never show permission codes; the server
 * decides, and this only says what it will allow.
 */
export function AccessMark({ canWrite }: { readonly canWrite: boolean }): ReactNode {
    const { t } = useTranslation();
    return (
        <span
            className={`inline-flex items-center gap-1.5 rounded-full border px-2.5 py-0.5 text-xs ${canWrite ? 'border-accent/40 text-accent' : 'border-line text-ink-muted'}`}
            title={canWrite ? t('accessMark.editableWhy') : t('accessMark.readOnlyWhy')}
        >
            <Icon name={canWrite ? 'edit' : 'locked'} size={16} />
            {canWrite ? t('accessMark.editable') : t('accessMark.readOnly')}
        </span>
    );
}
