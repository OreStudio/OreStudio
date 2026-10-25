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
import { useTranslation } from '../../i18n/Provider.js';
import { RecordStoreProvider, syntheticStore } from '../../refdata/store.js';
import { AreaTrail } from '../../shell/AreaTrail.js';
import { PageHeader } from '../../ui/Primitives.js';
import { useTabs } from '../../ui/Tabs.js';
import { DashboardTab } from './DashboardTab.js';
import { ProviderTab } from './ProviderTab.js';

const TABS = ['dashboard', 'provider'] as const;

/**
 * Synthetic: the feeds the platform generates rather than receives, on one screen
 * with two tabs. The dashboard says what runs by provider; the Provider screen
 * lists one provider's feeds, grouped by a property of the feed, and starts and
 * stops the provider's feeds. Both read through the synthetic store, so the
 * providers and the collections come from the synthetic service.
 */
export function SyntheticScreen(): ReactNode {
    const { t } = useTranslation();
    const { tab, bar } = useTabs({
        label: t('marketdata.synthetic.title'),
        tabs: TABS,
        titleOf: (one) => t(`marketdata.synthetic.tabs.${one}`),
    });
    return (
        <RecordStoreProvider store={syntheticStore}>
            <div className="space-y-6">
                <div>
                    <AreaTrail area="marketdata" screen={t('marketdata.synthetic.title')} />
                    <PageHeader
                        title={t('marketdata.synthetic.title')}
                        description={t('marketdata.synthetic.lead')}
                    />
                </div>
                {bar}
                {tab === 'provider' ? <ProviderTab /> : <DashboardTab />}
            </div>
        </RecordStoreProvider>
    );
}
