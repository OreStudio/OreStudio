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

/**
 * The tables screen: the largest relations the installation holds.
 *
 * The housekeeping list starts from the catalogue and names ten stores; this
 * screen starts from the relations themselves, because the answer to "what fills
 * the disk" is usually a table nothing names. It is a page of its own rather than
 * a tab of the list because the two answer different questions with different
 * rows: one is every store and its bound, the other is the head of a list of
 * hundreds of tables.
 *
 * The read is live: it measures the relations when it is asked, so the screen says
 * what is large now and not what was large at the sampler's last firing. The
 * series that answers "what grew" is the capacity panel's.
 */

import { useQuery } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { Link } from 'react-router';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { PATHS, housekeepingStorePath } from '../shell/paths.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { Metric } from '../ui/Metric.js';
import { PresetBar } from '../ui/Presets.js';
import { Notice } from '../ui/Primitives.js';
import { PieChart } from '../ui/PieChart.js';
import { formatDateTime } from '../ui/Time.js';
import { useOptionalSession } from '../session/SessionProvider.js';
import { formatSize } from './housekeeping.js';
import {
    TABLE_ROWS,
    TOP_SLICES,
    growthOf,
    pieSlices,
    relationShare,
    type TableRowCount,
    type TopSliceCount,
} from './tables.js';

/** The rows the read asks for: enough for the longest table the screen shows. */
const READ_LIMIT = 200;

/**
 * The window the growth column states: a week, because a table's movement is read
 * over days and the sampler writes once a day.
 */
const GROWTH_RANGE = '7d' as const;

export function HousekeepingTablesPage(): ReactNode {
    const { t, language } = useTranslation();
    const [top, setTop] = useState<TopSliceCount>(5);
    const [shown, setShown] = useState<TableRowCount>(25);
    /*
     * The installation's relations are the system administrator's to read, and
     * the server refuses them to a tenant. The screen says so rather than
     * offering a read it knows will fail.
     */
    const session = useOptionalSession();
    const mode = session?.state.status === 'authenticated' ? session.state.session.mode : undefined;
    const system = mode === undefined || mode === 'system-administration';
    const tables = useQuery({
        queryKey: ['operations', 'housekeeping', 'tables', READ_LIMIT, GROWTH_RANGE],
        queryFn: () => api.housekeepingTables(READ_LIMIT, GROWTH_RANGE),
        enabled: system,
    });

    const trail = (
        <AreaTrail
            area="operations"
            steps={[
                { label: t('operations.screens.housekeeping'), to: PATHS.housekeeping },
                { label: t('operations.tables.title') },
            ]}
        />
    );

    if (!system) {
        return (
            <ScreenFrame
                trail={trail}
                title={t('operations.tables.title')}
                description={t('operations.tables.description')}
            >
                <Notice tone="warn">{t('operations.tables.systemOnly')}</Notice>
            </ScreenFrame>
        );
    }

    if (tables.isPending || tables.isError) {
        return (
            <ScreenFrame
                trail={trail}
                title={t('operations.tables.title')}
                description={t('operations.tables.description')}
            >
                {tables.isError ? (
                    <Notice tone="error">{tables.error.message}</Notice>
                ) : (
                    <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                )}
            </ScreenFrame>
        );
    }

    const view = tables.data;
    const rows = view.rows;
    const slices = pieSlices(rows, view.database_bytes, top, t('operations.tables.rest'));
    const named = rows.filter((row) => row.store_code !== null).length;

    return (
        <ScreenFrame
            trail={trail}
            title={t('operations.tables.title')}
            description={t('operations.tables.description')}
        >
            <section className="card grid gap-4 p-4 sm:grid-cols-2 lg:grid-cols-4">
                <Metric
                    label={t('operations.tables.figures.database')}
                    value={formatSize(view.database_bytes)}
                />
                <Metric
                    label={t('operations.tables.figures.relations')}
                    value={view.relation_count.toLocaleString()}
                />
                <Metric
                    label={t('operations.tables.figures.named')}
                    value={t('operations.tables.figures.namedValue', {
                        count: named.toLocaleString(),
                        rows: rows.length.toLocaleString(),
                    })}
                    tone={named === 0 ? 'warn' : 'neutral'}
                    {...(named === 0 ? { note: t('operations.tables.figures.namedNote') } : {})}
                />
                <Metric
                    label={t('operations.tables.figures.readAt')}
                    value={
                        view.read_at === null
                            ? t('operations.tables.figures.noRead')
                            : formatDateTime(view.read_at, language)
                    }
                />
            </section>

            <section className="card space-y-4 p-4">
                <header className="flex flex-wrap items-baseline justify-between gap-3">
                    <div className="space-y-1">
                        <h2 className="text-sm font-medium">
                            {t('operations.tables.pie.title', { count: String(top) })}
                        </h2>
                        <p className="text-xs text-ink-faint">{t('operations.tables.pie.lead')}</p>
                    </div>
                    <PresetBar
                        label={t('operations.tables.pie.label')}
                        presets={TOP_SLICES.map((count) => String(count))}
                        value={String(top)}
                        labelOf={(preset) => preset}
                        onChange={(preset) => setTop(Number(preset) as TopSliceCount)}
                    />
                </header>
                <PieChart
                    slices={slices}
                    format={formatSize}
                    caption={t('operations.tables.pie.caption', {
                        count: String(top),
                        size: formatSize(view.database_bytes),
                    })}
                />
            </section>

            <section className="card space-y-3 p-4">
                <header className="flex flex-wrap items-baseline justify-between gap-3">
                    <div className="space-y-1">
                        <h2 className="text-sm font-medium">{t('operations.tables.list.title')}</h2>
                        <p className="text-xs text-ink-faint">
                            {t('operations.tables.list.lead', {
                                days: String(
                                    view.from_at === null || view.to_at === null
                                        ? 0
                                        : Math.max(
                                              1,
                                              Math.round(
                                                  (Date.parse(view.to_at.replace(' ', 'T')) -
                                                      Date.parse(view.from_at.replace(' ', 'T'))) /
                                                      86_400_000,
                                              ),
                                          ),
                                ),
                            })}
                        </p>
                    </div>
                    <PresetBar
                        label={t('operations.tables.list.label')}
                        presets={TABLE_ROWS.map((count) => String(count))}
                        value={String(shown)}
                        labelOf={(preset) => preset}
                        onChange={(preset) => setShown(Number(preset) as TableRowCount)}
                    />
                </header>
                <table className="w-full text-sm">
                    <thead className="text-left text-xs text-ink-faint">
                        <tr>
                            <th className="py-1 font-normal">
                                {t('operations.tables.columns.table')}
                            </th>
                            <th className="py-1 font-normal">
                                {t('operations.tables.columns.named')}
                            </th>
                            <th className="py-1 text-right font-normal">
                                {t('operations.tables.columns.size')}
                            </th>
                            <th className="py-1 text-right font-normal">
                                {t('operations.tables.columns.growth')}
                            </th>
                            <th className="py-1 text-right font-normal">
                                {t('operations.tables.columns.share')}
                            </th>
                        </tr>
                    </thead>
                    <tbody className="divide-y divide-line-subtle">
                        {rows.slice(0, shown).map((row) => (
                            <tr
                                key={row.relation_name}
                                className="align-top hover:bg-surface-hover"
                            >
                                <td className="py-2 pr-3 font-mono text-xs text-ink-muted">
                                    {row.relation_name}
                                </td>
                                <td className="py-2 pr-3">
                                    {row.store_code === null ? (
                                        <span className="text-warn">
                                            {t('operations.tables.named.byNothing')}
                                        </span>
                                    ) : (
                                        <Link
                                            to={housekeepingStorePath(row.store_code)}
                                            className="text-accent hover:text-accent-bright"
                                        >
                                            {row.store_code}
                                        </Link>
                                    )}
                                </td>
                                <td className="py-2 pr-3 text-right tabular-nums">
                                    {formatSize(row.size_bytes)}
                                </td>
                                <td className="py-2 pr-3 text-right tabular-nums">
                                    {growthOf(row) === null ? (
                                        <span className="text-ink-faint">
                                            {t('operations.tables.growth.unknown')}
                                        </span>
                                    ) : (
                                        <span
                                            className={
                                                (growthOf(row) ?? 0) > 0
                                                    ? 'text-warn'
                                                    : 'text-ink-muted'
                                            }
                                        >
                                            {((growthOf(row) ?? 0) > 0 ? '+' : '') +
                                                formatSize(growthOf(row) ?? 0)}
                                        </span>
                                    )}
                                </td>
                                <td className="py-2 text-right tabular-nums text-ink-muted">
                                    {(
                                        relationShare(row.size_bytes, view.database_bytes) * 100
                                    ).toFixed(1)}
                                    %
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
                {rows.length > shown && (
                    <p className="text-xs text-ink-faint">
                        {t('operations.tables.list.more', {
                            shown: String(shown),
                            total: rows.length.toLocaleString(),
                        })}
                    </p>
                )}
            </section>

            <p className="text-xs text-ink-faint">{t('operations.tables.footnote')}</p>
        </ScreenFrame>
    );
}
