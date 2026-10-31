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
 */

/**
 * One market data source, on its own screen: what the adapter registry says it
 * is doing, its counts, and the ids it refused. The board links here, so a
 * source has an address a person can share and return to.
 */

import { useParams } from 'react-router';
import type { ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { PATHS } from '../shell/paths.js';
import { Detail, LinkButton, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import {
    formatTickAge,
    formatVendorId,
    sourceRows,
    stateTone,
    useAdapterStatuses,
} from './marketDataSources.js';

/** The state of one source, in the registry's own words, painted by what it means. */
export function SourceStateTag({ state }: { readonly state: string }): ReactNode {
    const { t } = useTranslation();
    const labels: Readonly<Record<string, string>> = {
        announced: t('operations.sources.state.announced'),
        ready: t('operations.sources.state.ready'),
        serving: t('operations.sources.state.serving'),
        stale: t('operations.sources.state.stale'),
        rejected: t('operations.sources.state.rejected'),
    };
    return <Tag tone={stateTone(state)}>{labels[state] ?? state}</Tag>;
}

/** The rejected ids of one source, with the reason the adapter gave for each. */
export function RejectedIds({
    row,
}: {
    readonly row: ReturnType<typeof sourceRows>[number];
}): ReactNode {
    const { t } = useTranslation();
    if (row.rejectedIds.length === 0) {
        return <p className="text-sm text-ink-muted">{t('operations.sources.rejected.none')}</p>;
    }
    return (
        <table className="w-full text-sm">
            <thead className="text-left text-xs text-ink-faint">
                <tr>
                    <th className="py-1 font-normal">{t('operations.sources.rejected.id')}</th>
                    <th className="py-1 font-normal">{t('operations.sources.rejected.reason')}</th>
                </tr>
            </thead>
            <tbody className="divide-y divide-line-subtle">
                {row.rejectedIds.map((entry) => (
                    <tr key={`${formatVendorId(entry.id)}-${entry.reason}`}>
                        <td className="py-1.5 font-mono text-xs">{formatVendorId(entry.id)}</td>
                        <td className="py-1.5 font-mono text-xs">{entry.reason}</td>
                    </tr>
                ))}
            </tbody>
        </table>
    );
}

/** One source as the record page draws it. */
export function MarketDataSourcePage(): ReactNode {
    const { t } = useTranslation();
    const { sourceName = '' } = useParams();
    const adapters = useAdapterStatuses();
    const trail = [
        { label: t('operations.screens.sources'), to: PATHS.marketDataSources },
        { label: t('operations.sources.record.crumb') },
    ];
    const row =
        adapters.data === undefined
            ? undefined
            : sourceRows(adapters.data).find((candidate) => candidate.name === sourceName);
    return (
        <div className="space-y-6">
            <div>
                <AreaTrail area="operations" steps={trail} screen={sourceName} />
                <PageHeader
                    title={sourceName}
                    description={t('operations.sources.record.description')}
                    actions={
                        <LinkButton to={PATHS.marketDataSources} variant="secondary">
                            {t('operations.sources.record.back')}
                        </LinkButton>
                    }
                />
            </div>
            {adapters.isError ? (
                <Notice tone="error">{adapters.error.message}</Notice>
            ) : adapters.isPending ? (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            ) : row === undefined ? (
                <Notice tone="warn">{t('operations.sources.record.missing')}</Notice>
            ) : (
                <section className="card space-y-4 p-6">
                    <header className="flex flex-wrap items-baseline justify-between gap-2">
                        <h2 className="text-lg font-medium">
                            {t('operations.sources.detail.title', { source: row.name })}
                        </h2>
                        <SourceStateTag state={row.state} />
                    </header>
                    <dl className="grid gap-x-6 gap-y-3 sm:grid-cols-2 lg:grid-cols-4">
                        <Detail
                            label={t('operations.sources.columns.source')}
                            value={row.name}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.columns.provider')}
                            value={row.provider}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.columns.kind')}
                            value={row.kind === '' ? '—' : row.kind}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.detail.heartbeatCounter')}
                            value={row.heartbeatCounter.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.columns.routes')}
                            value={row.routes.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.columns.demanded')}
                            value={row.demanded.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.columns.accepted')}
                            value={row.accepted.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.columns.rejected')}
                            value={row.rejected.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.detail.notServed')}
                            value={row.notServed.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.detail.served')}
                            value={row.served.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.detail.unknown')}
                            value={row.unknown.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.columns.ticks')}
                            value={row.ticksPerMinute.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.columns.lastTick')}
                            value={formatTickAge(row.lastTickAgeMs, t)}
                            mono
                        />
                    </dl>
                    <div className="space-y-2">
                        <h3 className="text-sm font-medium">
                            {t('operations.sources.rejected.title')}
                        </h3>
                        <RejectedIds row={row} />
                    </div>
                </section>
            )}
        </div>
    );
}
