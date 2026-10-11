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
import { PageHeader } from '../ui/Primitives.js';
import { AreaTrail, type AreaName } from './AreaTrail.js';
import { areaOfCard, cardOf, type CardId } from './cards.js';

/**
 * The head of a screen that a file card leads to: the trail, the title and the
 * line about it, all taken from the card. A screen with no header of its own,
 * such as a journey, wears this, so it has the name the card gave it.
 */
export function CardScreen({
    card,
    children,
}: {
    readonly card: CardId;
    readonly children: ReactNode;
}): ReactNode {
    const { t } = useTranslation();
    const def = cardOf(card);
    const area = areaOfCard(card);
    return (
        <div className="space-y-6">
            <div>
                {area !== undefined && area !== 'tenants' && (
                    <AreaTrail area={area satisfies AreaName} screen={t(def.titleKey)} />
                )}
                <PageHeader title={t(def.titleKey)} description={t(def.bodyKey)} />
            </div>
            {children}
        </div>
    );
}
