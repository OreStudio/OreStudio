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
import { useHolds } from '../access/holds.js';
import { PERMISSION } from '../access/permissions.js';
import { useTranslation } from '../i18n/Provider.js';
import { Crumbs } from '../refdata/shared.js';
import { PageHeader } from '../ui/Primitives.js';
import { Tiles } from '../ui/Tiles.js';
import { fxFeedPath } from './fxFeeds.js';
import { irFeedPath } from './irFeeds.js';

/** The screens built, with their addresses and the read each needs. */
const BUILT = [
    {
        screen: 'fxFeeds',
        to: fxFeedPath(),
        icon: 'trend',
        read: PERMISSION.fxFeedsRead,
    },
    {
        screen: 'irFeeds',
        to: irFeedPath(),
        icon: 'trend',
        read: PERMISSION.irFeedsRead,
    },
    {
        screen: 'evolution',
        to: '/market-data/evolution',
        icon: 'chart',
        read: PERMISSION.curveSnapshotsRead,
    },
] as const;

/**
 * Market data, the area: the prices and curves the platform receives or
 * generates. Each screen is a card, drawn for a person who holds its read.
 */
export function MarketDataPage(): ReactNode {
    const { t } = useTranslation();
    const holds = useHolds();
    return (
        <div className="space-y-6">
            <div>
                <Crumbs parts={[{ label: t('marketdata.area.title') }]} />
                <PageHeader
                    title={t('marketdata.area.title')}
                    description={t('marketdata.area.lead')}
                />
            </div>
            <Tiles
                tiles={BUILT.filter(({ read }) => holds(read)).map(({ screen, to, icon }) => ({
                    title: t(`marketdata.area.screens.${screen}`),
                    body: t(`marketdata.area.screens.${screen}Body`),
                    to,
                    icon,
                }))}
            />
        </div>
    );
}
