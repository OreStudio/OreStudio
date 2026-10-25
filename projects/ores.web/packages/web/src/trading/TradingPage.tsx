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
import { AreaCards } from '../shell/AreaCards.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { PageHeader } from '../ui/Primitives.js';

/**
 * Trading, the area: what a desk reads and writes. Its cards come from the one
 * registry that every area and Home draw from, so the trade list is offered by
 * the same name and the same line here as anywhere else.
 */
export function TradingPage(): ReactNode {
    const { t } = useTranslation();
    return (
        <div className="space-y-6">
            <div>
                <AreaTrail area="trading" />
                <PageHeader title={t('trading.area.title')} description={t('trading.area.lead')} />
            </div>
            <AreaCards area="trading" />
        </div>
    );
}
