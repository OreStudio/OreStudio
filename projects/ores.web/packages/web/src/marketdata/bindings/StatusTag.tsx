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
import { useTranslation } from '../../i18n/Provider.js';
import { Tag } from '../../ui/Primitives.js';
import type { BindingStatus } from './bindingView.js';

const TONES = {
    running: 'up',
    stopped: 'muted',
    disabled: 'muted',
    quiet: 'warn',
    unsupported: 'warn',
} as const satisfies Record<BindingStatus, 'up' | 'muted' | 'warn'>;

/** What a binding is doing, as a label; nothing while it is not yet known. */
export function StatusTag({ status }: { readonly status: BindingStatus | undefined }): ReactNode {
    const { t } = useTranslation();
    if (status === undefined) {
        return null;
    }
    return <Tag tone={TONES[status]}>{t(`marketdata.bindings.statuses.${status}`)}</Tag>;
}
