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
 * The operations area: the screens that answer what the installation is doing.
 *
 * The area is entered from the system administration menu. A screen that is not
 * built yet is listed rather than left out, so the area states its own shape;
 * it carries no link, because a link that leads nowhere reads as a fault.
 *
 * The versions screen belongs to every session rather than to this area alone,
 * so it is also offered from Home.
 */

import type { ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { PageHeader } from '../ui/Primitives.js';
import { Tiles, type Tile } from '../ui/Tiles.js';

export function OperationsArea(): ReactNode {
    const { t } = useTranslation();
    const screens: readonly Tile[] = [
        {
            title: t('operations.screens.services'),
            body: t('operations.screens.servicesBody'),
        },
        {
            title: t('operations.screens.grid'),
            body: t('operations.screens.gridBody'),
        },
        {
            title: t('operations.screens.bus'),
            body: t('operations.screens.busBody'),
        },
        {
            title: t('operations.screens.logs'),
            body: t('operations.screens.logsBody'),
        },
        {
            title: t('operations.screens.versions'),
            body: t('operations.screens.versionsBody'),
            to: '/operations/versions',
        },
    ];

    return (
        <div className="space-y-6">
            <PageHeader
                title={t('operations.hub.title')}
                description={t('operations.hub.description')}
            />
            <Tiles tiles={screens} later={t('operations.hub.notBuilt')} />
        </div>
    );
}
