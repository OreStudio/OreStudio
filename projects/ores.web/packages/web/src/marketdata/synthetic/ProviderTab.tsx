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

import { keepPreviousData, useQuery } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { Link, useNavigate, useSearchParams } from 'react-router';
import { api, type FeedFilter, type FolderFeed } from '../../api/client.js';
import { FlagOf } from '../../images/flags.js';
import { useTranslation } from '../../i18n/Provider.js';
import { DEFAULT_PAGE_SIZE, Pager, pageBounds } from '../../ui/Pager.js';
import { Button, Notice, Select, Tag, cx } from '../../ui/Primitives.js';
import { AddFeed, useMayAddFeed } from './AddFeed.js';
import { servesFeeds, useSources } from './adapterStatus.js';
import { ServingTag, useProviderFacts } from './DashboardTab.js';

/** The asset classes the synthetic feeds are authored in. */
const ASSET_CLASSES = ['fx', 'interest_rates'] as const;
import { feedPageOf } from './feedTree.js';
import { frequencyOf } from './frequency.js';
import { engineName } from './IrProcessPanel.js';
import { providerName, providersOf, type ProviderEntry } from './providers.js';
import { FEEDS_KEY, useSelectionRuns } from './selectionRuns.js';
import { EVERY_MS, useCollections } from './useCollections.js';

function engineWord(t: (key: string) => string, feed: FolderFeed): string {
    return feed.kind === 'ir_curve'
        ? engineName(t, feed.engine)
        : t(`refdata.choices.process_type.${feed.engine}`);
}

function Underlying({ feed }: { readonly feed: FolderFeed }): ReactNode {
    return (
        <span className="inline-flex items-center gap-2">
            <FlagOf
                source={feed.kind === 'ir_curve' ? 'currency' : 'pair'}
                code={feed.underlying}
            />
            <span className="font-mono text-xs">{feed.underlying}</span>
        </span>
    );
}

/**
 * The Provider screen: one provider's feeds, gathered under a property of the
 * feed rather than under a folder tree. A provider combo chooses the provider,
 * a collection filter narrows it to one collection when it holds several, and
 * the group-by control chooses whether the grid gathers on asset class,
 * instrument or engine. Start and Stop in the header are the operator test
 * control: they act on every feed of the provider.
 */
export function ProviderTab(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const [params, setParams] = useSearchParams();
    const { collections, loaded, error } = useCollections();
    const facts = useProviderFacts();
    const sources = useSources();
    const providers = providersOf(collections);
    const chosen = params.get('provider') ?? '';
    const entry: ProviderEntry | undefined =
        providers.find((one) => one.code === chosen) ?? providers[0];
    const collection = params.get('collection') ?? '';
    const assetClass = params.get('assetClass') ?? '';
    const pageSize = Number(params.get('size')) || DEFAULT_PAGE_SIZE;
    const offset = Math.max(0, Number(params.get('offset')) || 0);

    const change = (patch: Readonly<Record<string, string | undefined>>): void =>
        setParams(
            (current) => {
                const next = new URLSearchParams(current);
                for (const [key, value] of Object.entries(patch)) {
                    if (value === undefined || value === '') {
                        next.delete(key);
                    } else {
                        next.set(key, value);
                    }
                }
                return next;
            },
            { replace: true },
        );
    const narrow = (patch: Readonly<Record<string, string | undefined>>): void =>
        change({ ...patch, offset: undefined });

    const filter: FeedFilter = {
        ...(entry === undefined ? {} : { provider: entry.code }),
        ...(collection === '' ? {} : { folder: collection }),
        ...(assetClass === '' ? {} : { assetClass }),
    };
    const filterKey = JSON.stringify(filter);

    const page = useQuery({
        queryKey: [...FEEDS_KEY, filterKey, offset, pageSize],
        queryFn: () => api.folderFeeds(filter, { offset, limit: Math.min(pageSize, 200) }),
        placeholderData: keepPreviousData,
        enabled: entry !== undefined,
        refetchInterval: EVERY_MS,
    });
    const feeds = page.data?.feeds ?? [];
    const total = page.data?.total ?? 0;

    const runs = useSelectionRuns();
    const mayAdd = useMayAddFeed();
    const [adding, setAdding] = useState(false);
    const source = entry === undefined ? undefined : sources?.get(entry.code);
    const ask = (action: 'start' | 'stop'): void => {
        if (entry !== undefined) {
            runs.ask(action, {
                name: providerName(entry.code, facts),
                selection: { provider: entry.code },
            });
        }
    };
    const { first, last } = pageBounds(offset, feeds.length);

    return (
        <div className="space-y-4">
            {error !== '' && <Notice tone="error">{error}</Notice>}
            {loaded && collections.length === 0 && (
                <Notice tone="info">{t('marketdata.synthetic.dashboard.none')}</Notice>
            )}
            {entry !== undefined && (
                <>
                    <div className="flex flex-wrap items-center gap-3">
                        <Select
                            aria-label={t('marketdata.synthetic.provider.provider')}
                            value={entry.code}
                            onChange={(event) =>
                                narrow({ provider: event.target.value, collection: undefined })
                            }
                        >
                            {providers.map((one) => (
                                <option key={one.code} value={one.code}>
                                    {providerName(one.code, facts)}
                                </option>
                            ))}
                        </Select>
                        <ServingTag source={source} />
                        <span className="text-sm text-ink-muted">
                            {t('marketdata.synthetic.provider.feedsCount', {
                                count: total.toLocaleString(),
                            })}
                        </span>
                        <span className="ml-auto flex items-center gap-2">
                            {runs.mayRun && (
                                <>
                                    <Button
                                        size="sm"
                                        disabled={servesFeeds(source)}
                                        onClick={() => ask('start')}
                                    >
                                        {t('marketdata.synthetic.runs.start')}
                                    </Button>
                                    <Button
                                        size="sm"
                                        disabled={!servesFeeds(source)}
                                        onClick={() => ask('stop')}
                                    >
                                        {t('marketdata.synthetic.runs.stop')}
                                    </Button>
                                </>
                            )}
                            {mayAdd && (
                                <Button variant="primary" onClick={() => setAdding(true)}>
                                    {t('marketdata.synthetic.provider.add')}
                                </Button>
                            )}
                        </span>
                    </div>
                    <div className="flex flex-wrap items-center gap-2 text-sm">
                        <span className="text-ink-muted">
                            {t('marketdata.synthetic.provider.assetClass')}
                        </span>
                        <div className="w-48 shrink-0">
                            <Select
                                aria-label={t('marketdata.synthetic.provider.assetClass')}
                                value={assetClass}
                                onChange={(event) => narrow({ assetClass: event.target.value })}
                            >
                                <option value="">
                                    {t('marketdata.synthetic.provider.allAssetClasses')}
                                </option>
                                {ASSET_CLASSES.map((code) => (
                                    <option key={code} value={code}>
                                        {t(`marketdata.synthetic.assetClass.${code}`)}
                                    </option>
                                ))}
                            </Select>
                        </div>
                        {entry.collections.length > 1 && (
                            <>
                                <span className="text-ink-muted">
                                    {t('marketdata.synthetic.provider.collection')}
                                </span>
                                <div className="w-64 shrink-0">
                                    <Select
                                        aria-label={t('marketdata.synthetic.provider.collection')}
                                        value={collection}
                                        onChange={(event) =>
                                            narrow({ collection: event.target.value })
                                        }
                                    >
                                        <option value="">
                                            {t('marketdata.synthetic.provider.allCollections')}
                                        </option>
                                        {entry.collections.map((one) => (
                                            <option key={one.id} value={one.id}>
                                                {one.name}
                                            </option>
                                        ))}
                                    </Select>
                                </div>
                            </>
                        )}
                    </div>
                    <div className="overflow-x-auto rounded-md border border-line">
                        <table className="w-full text-left text-sm">
                            <thead>
                                <tr className="border-b border-line text-xs text-ink-muted">
                                    {(
                                        [
                                            'underlying',
                                            'state',
                                            'enabled',
                                            'name',
                                            'engine',
                                            'frequency',
                                        ] as const
                                    ).map((column) => (
                                        <th key={column} className="px-3 py-2 font-medium">
                                            {t(`marketdata.synthetic.provider.columns.${column}`)}
                                        </th>
                                    ))}
                                </tr>
                            </thead>
                            <tbody>
                                {feeds.map((feed) => {
                                    const serving = servesFeeds(sources?.get(feed.provider_code));
                                    const every = frequencyOf(feed.ticks_per_hour);
                                    return (
                                        <tr
                                            key={feed.id}
                                            className="cursor-pointer border-b border-line-subtle last:border-b-0 hover:bg-surface-hover"
                                            onClick={() => void navigate(feedPageOf(feed))}
                                        >
                                            <td className="px-3 py-2">
                                                <Underlying feed={feed} />
                                            </td>
                                            <td className="px-3 py-2">
                                                <Tag tone={serving ? 'up' : 'muted'}>
                                                    {t(
                                                        serving
                                                            ? 'marketdata.synthetic.state.running'
                                                            : 'marketdata.synthetic.state.stopped',
                                                    )}
                                                </Tag>
                                            </td>
                                            <td className="px-3 py-2">
                                                <Tag tone={feed.enabled ? 'up' : 'down'}>
                                                    {t(
                                                        feed.enabled
                                                            ? 'marketdata.synthetic.provider.enabled'
                                                            : 'marketdata.synthetic.provider.disabled',
                                                    )}
                                                </Tag>
                                            </td>
                                            <td className="px-3 py-2 font-medium">
                                                <Link
                                                    className="hover:underline"
                                                    to={feedPageOf(feed)}
                                                    onClick={(event) => event.stopPropagation()}
                                                >
                                                    {feed.label}
                                                </Link>
                                            </td>
                                            <td className="px-3 py-2">{engineWord(t, feed)}</td>
                                            <td className="px-3 py-2 tabular-nums">
                                                {t(
                                                    `marketdata.synthetic.provider.every.${every.unit}`,
                                                    { n: every.n },
                                                )}
                                            </td>
                                        </tr>
                                    );
                                })}
                                {feeds.length === 0 && page.isSuccess && (
                                    <tr>
                                        <td colSpan={6} className="px-3 py-4 text-ink-muted">
                                            {t('marketdata.synthetic.provider.none')}
                                        </td>
                                    </tr>
                                )}
                            </tbody>
                        </table>
                    </div>
                    <Pager
                        offset={offset}
                        shown={feeds.length}
                        total={total}
                        pageSize={pageSize}
                        showing={t('marketdata.synthetic.provider.showing', {
                            first: String(first),
                            last: String(last),
                            total: total.toLocaleString(),
                        })}
                        onMove={(next) => change({ offset: next === 0 ? undefined : String(next) })}
                        onPageSize={(size) =>
                            change({
                                size: size === DEFAULT_PAGE_SIZE ? undefined : String(size),
                                offset: undefined,
                            })
                        }
                    />
                </>
            )}
            {adding && <AddFeed collectionId={collection} onClose={() => setAdding(false)} />}
            {runs.dialog}
        </div>
    );
}
