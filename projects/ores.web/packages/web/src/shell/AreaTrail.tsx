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
import { Crumbs } from '../refdata/shared.js';

/** The areas a screen can belong to, with the address of each area's page. */
const AREAS = {
    organisation: { nameKey: 'shell.menu.organisation', to: '/organisation' },
    operations: { nameKey: 'shell.menu.operations', to: '/operations' },
} as const;

export type AreaName = keyof typeof AREAS;

/**
 * The trail of a screen that belongs to an area: Home, the area, the screen.
 *
 * Every screen has one area and states it in the same words, so a person
 * reaches the same screen by the same trail from wherever they arrived.
 */
export function AreaTrail({
    area,
    screen,
}: {
    readonly area: AreaName;
    readonly screen: string;
}): ReactNode {
    const { t } = useTranslation();
    const { nameKey, to } = AREAS[area];
    return (
        <Crumbs
            parts={[
                { label: t('shell.menu.home'), to: '/' },
                { label: t(nameKey), to },
                { label: screen },
            ]}
        />
    );
}
