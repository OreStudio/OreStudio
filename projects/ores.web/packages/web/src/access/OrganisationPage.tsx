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
import { Tiles, type Tile } from '../ui/Tiles.js';
import { Crumbs } from '../refdata/shared.js';
import { useHolds } from './holds.js';

/** Where the organisation area lives. */
export const ORGANISATION_PATH = '/organisation';

/** Where the staff list lives. */
export const STAFF_PATH = '/staff';

/** Where the reporting hierarchy lives. */
export const HIERARCHY_PATH = '/hierarchy';

/**
 * Organisation, the area: the staff of the tenant and the hierarchy they report in.
 * Each is a tile, the same tile the landing page draws.
 */
export function OrganisationPage(): ReactNode {
    const { t } = useTranslation();
    const holds = useHolds();
    // A member does not read accounts, so their staff list is the people of
    // their parties, which the organisation read answers.
    const tiles: readonly Tile[] = [
        ...(holds('iam::accounts:read') || holds('iam::organisation:read')
            ? ([
                  {
                      title: t('access.hub.staff'),
                      body: t('access.hub.staffBody'),
                      to: STAFF_PATH,
                      icon: 'person',
                  },
              ] satisfies Tile[])
            : []),
        {
            title: t('access.hub.hierarchy'),
            body: t('access.hub.hierarchyBody'),
            to: HIERARCHY_PATH,
            icon: 'people',
        },
        ...(holds('iam::roles:read')
            ? ([
                  {
                      title: t('home.tenant.roles'),
                      body: t('home.tenant.rolesBody'),
                      to: '/roles',
                      icon: 'access',
                  },
              ] satisfies Tile[])
            : []),
        ...(holds('iam::accounts:lock')
            ? ([
                  {
                      title: t('home.tenant.rescue'),
                      body: t('home.tenant.rescueBody'),
                      to: '/rescue',
                      icon: 'unlock',
                  },
              ] satisfies Tile[])
            : []),
    ];
    return (
        <div className="space-y-6">
            <div>
                <Crumbs
                    parts={[
                        { label: t('shell.menu.home'), to: '/' },
                        { label: t('access.hub.title') },
                    ]}
                />
                <PageHeader title={t('access.hub.title')} description={t('access.hub.lead')} />
            </div>
            <Tiles tiles={tiles} />
        </div>
    );
}
