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
 * The operations area: the screens that say how the installation, and the
 * tenant in it, are running.
 *
 * A card is drawn for a person who holds the read of its screen. Versions needs
 * no read, because every person must be able to say which build they use when
 * they report a problem, so the area is drawn for everybody. The screens that
 * read the whole installation are drawn only where the session acts on the
 * deployment, as the menu's installation scope says.
 */

import type { ReactNode } from 'react';
import type { SessionMode } from '@ores/wire-protocol/browser';
import { useHolds } from '../access/holds.js';
import { useTranslation } from '../i18n/Provider.js';
import { offered, type MenuItem } from '../shell/areas.js';
import { PageHeader } from '../ui/Primitives.js';
import { Tiles, type Tile } from '../ui/Tiles.js';
import { Crumbs } from '../refdata/shared.js';

type Screen = Tile & Pick<MenuItem, 'permission' | 'scope'>;

export function OperationsArea({ mode }: { readonly mode: SessionMode }): ReactNode {
    const { t } = useTranslation();
    const holds = useHolds();
    const screens: readonly Screen[] = [
        {
            title: t('operations.screens.services'),
            body: t('operations.screens.servicesBody'),
            to: '/operations/services',
            icon: 'server',
            scope: 'installation',
        },
        {
            title: t('operations.screens.grid'),
            body: t('operations.screens.gridBody'),
            to: '/operations/grid',
            icon: 'apps',
            scope: 'installation',
        },
        {
            title: t('operations.screens.bus'),
            body: t('operations.screens.busBody'),
            to: '/operations/bus',
            icon: 'bus',
            scope: 'installation',
        },
        {
            title: t('operations.screens.logs'),
            body: t('operations.screens.logsBody'),
            to: '/operations/logs',
            icon: 'log',
            scope: 'installation',
        },
        {
            title: t('home.tenant.audit'),
            body: t('home.tenant.auditBody'),
            to: '/audit',
            icon: 'record',
            permission: ['iam::sessions:read', 'iam::login_info:read'],
        },
        {
            title: t('operations.screens.versions'),
            body: t('operations.screens.versionsBody'),
            to: '/operations/versions',
            icon: 'history',
        },
    ];
    const drawn = screens.filter((screen) => offered(screen, holds, mode));

    return (
        <div className="space-y-6">
            <div>
                <Crumbs
                    parts={[
                        { label: t('shell.menu.home'), to: '/' },
                        { label: t('operations.hub.title') },
                    ]}
                />
                <PageHeader
                    title={t('operations.hub.title')}
                    description={t('operations.hub.description')}
                />
            </div>
            <Tiles tiles={drawn} later={t('operations.hub.notBuilt')} />
        </div>
    );
}
