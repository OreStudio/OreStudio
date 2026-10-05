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
import { Tiles } from '../ui/Tiles.js';
import { Crumbs, classificationsPath } from './shared.js';

/** The screens reference data has, and the ones designed but not yet built. */
const COMING = ['currencies', 'currencyPairs', 'deskGroups', 'calendars'] as const;

/**
 * Reference data, the area: the data every trade, curve and report is built
 * on. Each screen is a tile; the ones not built yet stay, dimmed, so people
 * know they are coming.
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
                    {
                        title: t('refdata.classifications.title'),
                        body: t('refdata.area.classificationsBody'),
                        to: classificationsPath(),
                    },
                ]}
            />
            <Tiles
                tiles={COMING.map((screen) => ({
                    title: t(`refdata.area.coming.${screen}`),
                    body: t(`refdata.area.coming.${screen}Body`),
                }))}
                later={t('refdata.area.later')}
            />
        </div>
    );
}
