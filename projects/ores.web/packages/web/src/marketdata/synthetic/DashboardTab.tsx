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
import { useState, type ReactNode } from 'react';
import { Link } from 'react-router';
import { api, type FeedFolder } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { show, useRecords } from '../../refdata/records.js';
import { PATHS } from '../../shell/paths.js';
import { Button, Notice, Tag, cx } from '../../ui/Primitives.js';
import {
    demandedNotRunning,
    demandedNotRunningAll,
    servedTotals,
    servesFeeds,
    useSources,
    type Source,
} from './adapterStatus.js';
import { folderKey, providerKey } from './feedTree.js';
import { providerName, providersOf, type ProviderEntry, type ProviderFacts } from './providers.js';
import { useSelectionRuns } from './selectionRuns.js';
import { stateOf, totalOf, totalsOf, type Totals } from './totals.js';
import { EVERY_MS, FOLDERS_KEY, useCollections } from './useCollections.js';

/** Running, stopped or some of the feeds of a node, as a tag. */
export function StateTag({ totals }: { readonly totals: Totals }): ReactNode {
    const { t } = useTranslation();
    switch (stateOf(totals)) {
        case 'running':
            return <Tag tone="up">{t('marketdata.synthetic.state.running')}</Tag>;
        case 'stopped':
            return <Tag tone="muted">{t('marketdata.synthetic.state.stopped')}</Tag>;
        case 'partial':
            return (
                <Tag tone="warn">
                    {t('marketdata.synthetic.state.partial', {
                        running: String(totals.running),
                        total: String(totalOf(totals)),
                    })}
                </Tag>
            );
        case 'empty':
            return <Tag tone="muted">{t('marketdata.synthetic.state.empty')}</Tag>;
    }
}

function Holds({ totals }: { readonly totals: Totals }): ReactNode {
    const { t } = useTranslation();
    return (
        <>
            {t('marketdata.synthetic.dashboard.holds', {
                fx: String(totals.fx),
                ir: String(totals.ir),
            })}
        </>
    );
}

/**
 * The state of a node whose feeds one provider's source serves: nothing while
 * the service has not yet said what that source serves, because a tag either
 * way would be a guess.
 */
function SourceStateTag({
    totals,
    providerCode,
    sources,
}: {
    readonly totals: Totals;
    readonly providerCode: string;
    readonly sources: ReadonlyMap<string, Source> | undefined;
}): ReactNode {
    if (sources === undefined) {
        return null;
    }
    return <StateTag totals={servedTotals(totals, sources.get(providerCode))} />;
}

/** The providers the catalogue names, with what each one says it simulates. */
export function useProviderFacts(): ReadonlyMap<string, ProviderFacts> {
    const catalogue = useRecords('providers');
    return new Map(
        (catalogue.data ?? []).map((row) => [
            show(row['code']),
            {
                name: show(row['name']),
                kind: show(row['kind']),
                description: show(row['description']),
            },
        ]),
    );
}

/**
 * The dashboard: what runs, by provider. Each provider says what it simulates and
 * how much of it runs, read from the adapter the market data service demands its
 * feeds from; one banner counts what the service demands and does not get, which
 * is what a generator that failed to start leaves behind. A provider can be
 * started or stopped as a whole, as an operator test, and its asset classes open
 * in the feed tree.
 */
export function DashboardTab(): ReactNode {
    const { t } = useTranslation();
    const runs = useSelectionRuns();
    const { collections, loaded, error } = useCollections();
    const facts = useProviderFacts();
    const sources = useSources();
    const providers = providersOf(collections);
    const demanded = demandedNotRunningAll(sources ?? new Map<string, Source>());
    const [chosen, setChosen] = useState<string | undefined>();
    const provider: ProviderEntry | undefined =
        providers.find((entry) => entry.code === chosen) ?? providers[0];

    const classes = useQuery({
        queryKey: [...FOLDERS_KEY, 'classes', provider?.code],
        queryFn: async () =>
            (
                await Promise.all(
                    (provider?.collections ?? []).map((one) => api.feedFolders(one.id)),
                )
            ).flat(),
        enabled: provider !== undefined,
        refetchInterval: EVERY_MS,
    });
    const classRows: readonly FeedFolder[] = classes.data ?? [];

    const ask = (action: 'start' | 'stop', entry: ProviderEntry): void =>
        runs.ask(action, {
            name: providerName(entry.code, facts),
            selection: { provider: entry.code },
        });

    return (
        <div className="space-y-4">
            {error !== '' && <Notice tone="error">{error}</Notice>}
            {!loaded && error === '' && (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            )}
            {loaded && collections.length === 0 && (
                <Notice tone="info">{t('marketdata.synthetic.dashboard.none')}</Notice>
            )}
            {demanded > 0 && (
                <Notice tone="warn">
                    <div className="flex flex-wrap items-center gap-3">
                        <span className="min-w-60 flex-1">
                            <strong>
                                {t('marketdata.synthetic.dashboard.idle', {
                                    count: demanded.toLocaleString(),
                                })}
                            </strong>{' '}
                            {t('marketdata.synthetic.dashboard.idleBody')}
                        </span>
                        {runs.mayRun && (
                            <Button
                                variant="primary"
                                size="sm"
                                onClick={() =>
                                    runs.ask('start', {
                                        name: t('marketdata.synthetic.dashboard.idleName'),
                                        selection: { shouldRun: true },
                                    })
                                }
                            >
                                {t('marketdata.synthetic.dashboard.startThem')}
                            </Button>
                        )}
                        <Link
                            className="text-sm text-accent hover:text-accent-bright"
                            to={`${PATHS.synthetic}?tab=tree&shouldRun=true`}
                        >
                            {t('marketdata.synthetic.dashboard.showThem')}
                        </Link>
                    </div>
                </Notice>
            )}
            {providers.length > 0 && (
                <div className="overflow-x-auto rounded-md border border-line">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                <th className="px-4 py-2 font-medium">
                                    {t('marketdata.synthetic.dashboard.provider')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('marketdata.synthetic.dashboard.simulates')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('marketdata.synthetic.dashboard.holdsHeader')}
                                </th>
                                <th className="px-4 py-2 text-right font-medium">
                                    {t('marketdata.synthetic.dashboard.feeds')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('marketdata.synthetic.dashboard.state')}
                                </th>
                                <th className="px-4 py-2 text-right font-medium">
                                    {t('marketdata.synthetic.dashboard.shouldRun')}
                                </th>
                                <th className="px-4 py-2" />
                            </tr>
                        </thead>
                        <tbody>
                            {providers.map((entry) => {
                                const served = sources?.get(entry.code);
                                const stopped = demandedNotRunning(served);
                                return (
                                    <tr
                                        key={entry.code}
                                        aria-selected={provider?.code === entry.code}
                                        className={cx(
                                            'cursor-pointer border-b border-line-subtle last:border-b-0 hover:bg-surface-hover',
                                            provider?.code === entry.code && 'bg-accent/10',
                                        )}
                                        onClick={() => setChosen(entry.code)}
                                    >
                                        <td className="px-4 py-2">
                                            <span className="font-medium">
                                                {providerName(entry.code, facts)}
                                            </span>{' '}
                                            {facts.has(entry.code) && (
                                                <Tag tone="muted">
                                                    {t(
                                                        `marketdata.synthetic.providerKind.${facts.get(entry.code)?.kind ?? ''}`,
                                                    )}
                                                </Tag>
                                            )}
                                        </td>
                                        <td className="max-w-md px-4 py-2 text-ink-muted">
                                            {facts.get(entry.code)?.description ?? ''}
                                        </td>
                                        <td className="px-4 py-2 text-ink-muted">
                                            <Holds totals={entry.totals} />
                                        </td>
                                        <td className="px-4 py-2 text-right tabular-nums">
                                            {totalOf(entry.totals).toLocaleString()}
                                        </td>
                                        <td className="px-4 py-2">
                                            <SourceStateTag
                                                totals={entry.totals}
                                                providerCode={entry.code}
                                                sources={sources}
                                            />
                                        </td>
                                        <td className="px-4 py-2 text-right">
                                            {stopped > 0 && (
                                                <Tag tone="warn">
                                                    {t(
                                                        'marketdata.synthetic.dashboard.stoppedCount',
                                                        {
                                                            count: stopped.toLocaleString(),
                                                        },
                                                    )}
                                                </Tag>
                                            )}
                                        </td>
                                        <td className="px-4 py-2 text-right">
                                            {runs.mayRun && (
                                                <span className="flex justify-end gap-2">
                                                    <Button
                                                        size="sm"
                                                        disabled={servesFeeds(served)}
                                                        onClick={() => ask('start', entry)}
                                                    >
                                                        {t('marketdata.synthetic.runs.start')}
                                                    </Button>
                                                    <Button
                                                        size="sm"
                                                        disabled={!servesFeeds(served)}
                                                        onClick={() => ask('stop', entry)}
                                                    >
                                                        {t('marketdata.synthetic.runs.stop')}
                                                    </Button>
                                                </span>
                                            )}
                                        </td>
                                    </tr>
                                );
                            })}
                        </tbody>
                    </table>
                </div>
            )}
            {provider !== undefined && (
                <section className="space-y-3 rounded-md border border-line p-4">
                    <div className="flex flex-wrap items-center gap-3">
                        <h2 className="text-lg font-semibold">
                            {providerName(provider.code, facts)}
                        </h2>
                        <SourceStateTag
                            totals={provider.totals}
                            providerCode={provider.code}
                            sources={sources}
                        />
                        <Link
                            className="text-sm text-accent hover:text-accent-bright"
                            to={`${PATHS.synthetic}?tab=tree&node=${encodeURIComponent(providerKey(provider.code))}`}
                        >
                            {t('marketdata.synthetic.dashboard.openInTree')}
                        </Link>
                    </div>
                    {classRows.length > 0 && (
                        <div className="overflow-x-auto rounded-md border border-line">
                            <table className="w-full text-left text-sm">
                                <thead>
                                    <tr className="border-b border-line text-xs text-ink-muted">
                                        <th className="px-4 py-2 font-medium">
                                            {t('marketdata.synthetic.dashboard.assetClass')}
                                        </th>
                                        <th className="px-4 py-2 text-right font-medium">
                                            {t('marketdata.synthetic.dashboard.feeds')}
                                        </th>
                                        <th className="px-4 py-2 font-medium">
                                            {t('marketdata.synthetic.dashboard.state')}
                                        </th>
                                        <th className="px-4 py-2" />
                                    </tr>
                                </thead>
                                <tbody>
                                    {classRows.map((row) => {
                                        const totals = totalsOf([row]);
                                        return (
                                            <tr
                                                key={row.id}
                                                className="border-b border-line-subtle last:border-b-0"
                                            >
                                                <td className="px-4 py-2 font-medium">
                                                    {row.name}
                                                </td>
                                                <td className="px-4 py-2 text-right tabular-nums">
                                                    {totalOf(totals).toLocaleString()}
                                                </td>
                                                <td className="px-4 py-2">
                                                    <SourceStateTag
                                                        totals={totals}
                                                        providerCode={provider.code}
                                                        sources={sources}
                                                    />
                                                </td>
                                                <td className="px-4 py-2 text-right">
                                                    <Link
                                                        className="text-sm text-accent hover:text-accent-bright"
                                                        to={`${PATHS.synthetic}?tab=tree&node=${encodeURIComponent(folderKey(row.id))}`}
                                                    >
                                                        {t(
                                                            'marketdata.synthetic.dashboard.openInTree',
                                                        )}
                                                    </Link>
                                                </td>
                                            </tr>
                                        );
                                    })}
                                </tbody>
                            </table>
                        </div>
                    )}
                </section>
            )}
            {runs.dialog}
        </div>
    );
}
