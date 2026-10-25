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
import { Link } from 'react-router';
import { useTranslation } from '../../i18n/Provider.js';
import { show, useRecords } from '../../refdata/records.js';
import { PATHS } from '../../shell/paths.js';
import { Button, Notice, Tag } from '../../ui/Primitives.js';
import {
    demandedNotServed,
    demandedNotServedAll,
    servesFeeds,
    useSources,
    type Source,
} from './adapterStatus.js';
import { providerName, providersOf, type ProviderEntry, type ProviderFacts } from './providers.js';
import { useSelectionRuns } from './selectionRuns.js';
import { useCollections } from './useCollections.js';
import { feedsHeld, useProviderFeedCounts, type FeedCounts } from './useProviderFeeds.js';

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

/** Whether a provider's feeds serve, as the source of that provider says. */
export function ServingTag({ source }: { readonly source: Source | undefined }): ReactNode {
    const { t } = useTranslation();
    if (source === undefined) {
        return null;
    }
    return servesFeeds(source) ? (
        <Tag tone="up">{t('marketdata.synthetic.state.serving')}</Tag>
    ) : (
        <Tag tone="muted">{t('marketdata.synthetic.state.stopped')}</Tag>
    );
}

/** One figure of the dashboard. */
function Tile({
    label,
    value,
    tone = 'plain',
}: {
    readonly label: string;
    readonly value: string;
    readonly tone?: 'plain' | 'up' | 'warn';
}): ReactNode {
    const colour = tone === 'up' ? 'text-up' : tone === 'warn' ? 'text-warn' : '';
    return (
        <div className="rounded-md border border-line p-3">
            <div className="text-xs uppercase tracking-wide text-ink-faint">{label}</div>
            <div className={`font-mono text-2xl tabular-nums ${colour}`}>{value}</div>
        </div>
    );
}

/** The figures over the provider list, every one of them counted by the same rule. */
interface Figures {
    readonly serving: number;
    readonly stopped: number;
    readonly demanded: number;
    readonly ticksPerMinute: number;
}

function figuresOf(
    providers: readonly ProviderEntry[],
    counts: ReadonlyMap<string, FeedCounts>,
    sources: ReadonlyMap<string, Source>,
): Figures {
    let serving = 0;
    let stopped = 0;
    for (const entry of providers) {
        const held = feedsHeld(counts.get(entry.code));
        if (servesFeeds(sources.get(entry.code))) {
            serving += held;
        } else {
            stopped += held;
        }
    }
    const every = [...sources.values()];
    return {
        serving,
        stopped,
        demanded: every.reduce((count, source) => count + source.demanded, 0),
        ticksPerMinute: every.reduce((count, source) => count + source.ticksPerMinute, 0),
    };
}

/**
 * The dashboard: what runs, by provider, and the figures that follow from it.
 * Each provider says what it simulates, how many feeds it holds and whether it
 * serves; a feed serves when its provider's source serves and the service
 * demands at least one id, so this screen counts nothing beside that state. The
 * alert is the demand that state does not serve. A provider's Start and Stop are
 * the operator test control, and act on every feed of the provider.
 */
export function DashboardTab(): ReactNode {
    const { t } = useTranslation();
    const runs = useSelectionRuns();
    const { collections, loaded, error } = useCollections();
    const facts = useProviderFacts();
    const sources = useSources();
    const providers = providersOf(collections);
    const counts = useProviderFeedCounts(providers.map((entry) => entry.code));
    const ready = loaded && counts !== undefined && sources !== undefined;
    const notServed = sources === undefined ? 0 : demandedNotServedAll(sources);
    const figures =
        counts === undefined || sources === undefined
            ? undefined
            : figuresOf(providers, counts, sources);

    const ask = (action: 'start' | 'stop', entry: ProviderEntry): void =>
        runs.ask(action, {
            name: providerName(entry.code, facts),
            selection: { provider: entry.code },
        });

    return (
        <div className="space-y-4">
            {error !== '' && <Notice tone="error">{error}</Notice>}
            {!ready && error === '' && providers.length > 0 && (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            )}
            {loaded && collections.length === 0 && (
                <Notice tone="info">{t('marketdata.synthetic.dashboard.none')}</Notice>
            )}
            {ready && notServed > 0 && (
                <Notice tone="warn">
                    <strong>
                        {t('marketdata.synthetic.dashboard.idle', {
                            count: notServed.toLocaleString(),
                        })}
                    </strong>{' '}
                    {t('marketdata.synthetic.dashboard.idleBody')}
                </Notice>
            )}
            {ready && figures !== undefined && providers.length > 0 && (
                <>
                    <div className="grid gap-3 sm:grid-cols-3 lg:grid-cols-5">
                        <Tile
                            label={t('marketdata.synthetic.dashboard.tiles.providers')}
                            value={providers.length.toLocaleString()}
                        />
                        <Tile
                            label={t('marketdata.synthetic.dashboard.tiles.running')}
                            value={figures.serving.toLocaleString()}
                            tone="up"
                        />
                        <Tile
                            label={t('marketdata.synthetic.dashboard.tiles.stopped')}
                            value={figures.stopped.toLocaleString()}
                        />
                        <Tile
                            label={t('marketdata.synthetic.dashboard.tiles.demanded')}
                            value={figures.demanded.toLocaleString()}
                        />
                        <Tile
                            label={t('marketdata.synthetic.dashboard.tiles.ticks')}
                            value={figures.ticksPerMinute.toLocaleString()}
                            tone={figures.ticksPerMinute > 0 ? 'up' : 'plain'}
                        />
                    </div>
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
                                    const source = sources.get(entry.code);
                                    const held = counts.get(entry.code);
                                    const stopped = demandedNotServed(source);
                                    return (
                                        <tr
                                            key={entry.code}
                                            className="border-b border-line-subtle last:border-b-0 hover:bg-surface-hover"
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
                                                {t('marketdata.synthetic.dashboard.holds', {
                                                    fx: String(held?.fx ?? 0),
                                                    ir: String(held?.ir ?? 0),
                                                })}
                                            </td>
                                            <td className="px-4 py-2 text-right tabular-nums">
                                                {feedsHeld(held).toLocaleString()}
                                            </td>
                                            <td className="px-4 py-2">
                                                <ServingTag source={source} />
                                            </td>
                                            <td className="px-4 py-2 text-right">
                                                {stopped > 0 && (
                                                    <Tag tone="warn">
                                                        {t(
                                                            'marketdata.synthetic.dashboard.stoppedCount',
                                                            { count: stopped.toLocaleString() },
                                                        )}
                                                    </Tag>
                                                )}
                                            </td>
                                            <td className="px-4 py-2 text-right">
                                                <span className="flex items-center justify-end gap-2">
                                                    {runs.mayRun && (
                                                        <>
                                                            <Button
                                                                size="sm"
                                                                disabled={servesFeeds(source)}
                                                                onClick={() => ask('start', entry)}
                                                            >
                                                                {t(
                                                                    'marketdata.synthetic.runs.start',
                                                                )}
                                                            </Button>
                                                            <Button
                                                                size="sm"
                                                                disabled={!servesFeeds(source)}
                                                                onClick={() => ask('stop', entry)}
                                                            >
                                                                {t(
                                                                    'marketdata.synthetic.runs.stop',
                                                                )}
                                                            </Button>
                                                        </>
                                                    )}
                                                    <Link
                                                        className="text-sm text-accent hover:text-accent-bright"
                                                        to={`${PATHS.synthetic}?tab=provider&provider=${encodeURIComponent(entry.code)}`}
                                                    >
                                                        {t('marketdata.synthetic.dashboard.open')}
                                                    </Link>
                                                </span>
                                            </td>
                                        </tr>
                                    );
                                })}
                            </tbody>
                        </table>
                    </div>
                </>
            )}
            {runs.dialog}
        </div>
    );
}
