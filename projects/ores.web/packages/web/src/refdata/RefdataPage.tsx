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
import type { SessionMode } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { AreaCards } from '../shell/AreaCards.js';
import { PageHeader } from '../ui/Primitives.js';
import { AreaTrail } from '../shell/AreaTrail.js';

/**
 * Reference data, the area: the data every trade, curve and report is built
 * on, in groups. The parties and everything about a party are here, with the
 * shared lists they connect to. The cards come from the one registry the other
 * areas and Home draw from.
 */
export function RefdataPage({ mode }: { readonly mode: SessionMode }): ReactNode {
    const { t } = useTranslation();
    return (
        <div className="space-y-6">
            <div>
                <AreaTrail area="refdata" />
                <PageHeader title={t('refdata.area.title')} description={t('refdata.area.lead')} />
            </div>
            <AreaCards area="refdata" mode={mode} />
        </div>
    );
}
