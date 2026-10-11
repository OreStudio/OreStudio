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
import { Crumbs } from '../refdata/shared.js';
import { useTabs } from '../ui/Tabs.js';
import { TestsQueue } from './TestsQueue.js';

/** The tabs of the area. Board, Stories and Tasks join them with the next story. */
const TABS = ['tests'] as const;

/**
 * The development area: the work a team does on the product, starting with the
 * tests a tester can run. It is a regular module. It does not depend on the
 * developer tools, because a trader may run test scenarios.
 */
export function DevelopmentArea(): ReactNode {
    const { t } = useTranslation();
    const { tab, bar } = useTabs({
        label: t('development.tabs.label'),
        tabs: TABS,
        titleOf: (candidate) => t(`development.tabs.${candidate}`),
    });
    return (
        <div className="space-y-4">
            <Crumbs
                parts={[
                    { label: t('shell.menu.home'), to: '/' },
                    { label: t('shell.menu.development') },
                ]}
            />
            {bar}
            {tab === 'tests' && <TestsQueue />}
        </div>
    );
}
