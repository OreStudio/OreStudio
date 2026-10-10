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

import { useQuery } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { useParams } from 'react-router';
import { qa } from '../api/qa.js';
import { useTranslation } from '../i18n/Provider.js';
import { Crumbs } from '../refdata/shared.js';
import { Notice, PageHeader } from '../ui/Primitives.js';

/**
 * The page a queue row opens. It names the scenario and its steps, and says the
 * runner is not built yet. The runner replaces the body.
 */
export function ScenarioPage(): ReactNode {
    const { t } = useTranslation();
    const { id = '' } = useParams();
    const scenario = useQuery({ queryKey: ['qa-scenario', id], queryFn: () => qa.scenario(id) });

    return (
        <div className="space-y-4">
            <Crumbs
                parts={[
                    { label: t('shell.menu.home'), to: '/' },
                    { label: t('shell.menu.development'), to: '/development' },
                    { label: t('development.tabs.tests'), to: '/development' },
                ]}
            />
            {scenario.isError && <Notice tone="error">{scenario.error.message}</Notice>}
            {scenario.isPending && <p className="text-sm text-ink-muted">{t('common.loading')}</p>}
            {scenario.isSuccess && (
                <>
                    <PageHeader
                        title={scenario.data.title}
                        description={scenario.data.description}
                    />
                    <Notice>{t('development.scenario.notBuilt')}</Notice>
                    <ol className="list-decimal space-y-1 pl-6 text-sm">
                        {scenario.data.steps.map((step) => (
                            <li key={`${step.client ?? ''}/${step.title}`}>
                                {step.client === null
                                    ? step.title
                                    : `${step.client}: ${step.title}`}
                            </li>
                        ))}
                    </ol>
                </>
            )}
        </div>
    );
}
