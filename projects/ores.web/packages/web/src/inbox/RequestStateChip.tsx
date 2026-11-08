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
import { Tag } from '../ui/Primitives.js';
import { autoApprovalRule, stateLabel, stateTone } from './words.js';

/**
 * A request's state, named and painted, wherever a request is drawn.
 *
 * A request a policy closed carries the rule beside its state, so a reader never
 * takes an auto-approved act for one somebody signed. A request a person decided
 * carries nothing extra.
 */
export function RequestStateChip({
    stateCode,
    autoApprovalCode = '',
}: {
    readonly stateCode: string;
    readonly autoApprovalCode?: string;
}): ReactNode {
    const { t } = useTranslation();
    const rule = autoApprovalRule(t, autoApprovalCode);
    return (
        <>
            <Tag tone={stateTone(stateCode)}>{stateLabel(t, stateCode)}</Tag>
            {rule === '' ? null : (
                <span title={t('inbox.autoApproval.why', { rule })}>
                    <Tag tone="muted">{t('inbox.autoApproval.chip')}</Tag>
                </span>
            )}
        </>
    );
}
