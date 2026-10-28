/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { useQuery } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { Link } from 'react-router';
import { curveConfiguration } from '../api/curveConfiguration.js';
import { useTranslation } from '../i18n/Provider.js';
import { Crumbs } from '../refdata/shared.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { curveConfigurationPath } from '../shell/paths.js';
import { Notice, PageHeader } from '../ui/Primitives.js';

/**
 * The curve configurations the session's parties hold, one row each.
 *
 * A curve configuration is reached without a report that binds it, which is what
 * this list is for: the report page links the configurations it binds, and this
 * page is the way in for a person who starts from the configuration.
 */

/** Where the list of curve configurations is cached. */
export const curveConfigurationsKey = ['curve-configurations'] as const;

export function CurveConfigurationsPage(): ReactNode {
    const { t } = useTranslation();
    const area = useAreaRoot('reporting');
    const reading = useQuery({
        queryKey: curveConfigurationsKey,
        queryFn: () => curveConfiguration.list(),
    });
    return (
        <div className="space-y-4">
            <Crumbs parts={[...area, { label: t('curveConfiguration.list.title') }]} />
            <PageHeader
                title={t('curveConfiguration.list.title')}
                description={t('curveConfiguration.list.lead')}
            />
            {reading.isPending ? (
                <p className="text-sm text-ink-muted" role="status">
                    {t('common.loading')}
                </p>
            ) : reading.isError ? (
                <Notice tone="error">{reading.error.message}</Notice>
            ) : reading.data.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('curveConfiguration.list.none')}</p>
            ) : (
                <section className="card overflow-hidden">
                    <table className="w-full text-sm">
                        <thead>
                            <tr className="border-b border-line text-left text-xs text-ink-faint uppercase">
                                <th className="px-4 py-2 font-medium">
                                    {t('curveConfiguration.list.columns.name')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('curveConfiguration.list.columns.party')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('curveConfiguration.list.columns.sections')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('curveConfiguration.list.columns.curves')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('curveConfiguration.list.columns.version')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {reading.data.map((row) => (
                                <tr
                                    key={row.id}
                                    className="border-b border-line-subtle last:border-0"
                                >
                                    <td className="px-4 py-2">
                                        <Link
                                            to={curveConfigurationPath(row.configurationId)}
                                            className="text-accent hover:text-accent-bright"
                                        >
                                            {row.name === '' ? row.id.slice(0, 8) : row.name}
                                        </Link>
                                        {row.description !== '' && (
                                            <p className="text-xs text-ink-faint">
                                                {row.description}
                                            </p>
                                        )}
                                    </td>
                                    <td className="px-4 py-2 text-ink-muted">{row.partyName}</td>
                                    <td className="px-4 py-2 text-ink-muted">{row.sections}</td>
                                    <td className="px-4 py-2 text-ink-muted">{row.curves}</td>
                                    <td className="px-4 py-2 text-ink-muted">{row.version}</td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </section>
            )}
        </div>
    );
}
