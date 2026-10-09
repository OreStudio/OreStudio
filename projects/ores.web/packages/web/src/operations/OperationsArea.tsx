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
 * The area is entered from the system administration menu. Every screen the
 * area names exists, so each one carries its link; the last, the telemetry
 * logs, was added by the fifth unit.
 *
 * The versions screen belongs to every session rather than to this area alone,
 * so it is also offered from Home.
 */

import { GitBranch, LayoutGrid, Network, ScrollText, Server } from 'lucide-react';
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
            to: '/operations/services',
            icon: Server,
        },
        {
            title: t('operations.screens.grid'),
            body: t('operations.screens.gridBody'),
            to: '/operations/grid',
            icon: LayoutGrid,
        },
        {
            title: t('operations.screens.bus'),
            body: t('operations.screens.busBody'),
            to: '/operations/bus',
            icon: Network,
        },
        {
            title: t('operations.screens.logs'),
            body: t('operations.screens.logsBody'),
            to: '/operations/logs',
            icon: ScrollText,
        },
        {
            title: t('operations.screens.versions'),
            body: t('operations.screens.versionsBody'),
            to: '/operations/versions',
            icon: GitBranch,
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
