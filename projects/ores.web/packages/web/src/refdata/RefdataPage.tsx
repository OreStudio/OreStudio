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

import { ArrowLeftRight, Briefcase, Calendar, Coins, Tags } from 'lucide-react';
import type { ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { PageHeader } from '../ui/Primitives.js';
import { Tiles } from '../ui/Tiles.js';
import { Crumbs, classificationsPath } from './shared.js';

/** The screens built, with their addresses. */
const BUILT = [
    { screen: 'calendars', to: '/refdata/calendars', icon: Calendar },
    { screen: 'currencies', to: '/refdata/currencies', icon: Coins },
    { screen: 'currencyPairs', to: '/refdata/currency-pairs', icon: ArrowLeftRight },
    { screen: 'deskGroups', to: '/refdata/desk-groups', icon: Briefcase },
] as const;

/**
 * Reference data, the area: the data every trade, curve and report is built
 * on. Each screen is a tile.
 */
export function RefdataPage(): ReactNode {
    const { t } = useTranslation();
    return (
        <div className="space-y-6">
            <div>
                <Crumbs parts={[{ label: t('refdata.area.title') }]} />
                <PageHeader title={t('refdata.area.title')} description={t('refdata.area.lead')} />
            </div>
            <Tiles
                tiles={[
                    ...BUILT.map(({ screen, to, icon }) => ({
                        title: t(`refdata.area.screens.${screen}`),
                        body: t(`refdata.area.screens.${screen}Body`),
                        to,
                        icon,
                    })),
                    {
                        title: t('refdata.classifications.title'),
                        body: t('refdata.area.classificationsBody'),
                        to: classificationsPath(),
                        icon: Tags,
                    },
                ]}
            />
        </div>
    );
}
