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
import { api, type FeedFolder } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { useRecords, show } from '../refdata/records.js';
import { RecordStoreProvider, syntheticStore } from '../refdata/store.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { PATHS } from '../shell/paths.js';
import { Button, Notice, PageHeader, Tag, cx } from '../ui/Primitives.js';
import { folderKey, providerKey, scopeKey } from './feedTree.js';
import { FOLDERS_KEY, useFolderRuns } from './folderRuns.js';
import { EVERY_MS, useCollections } from './useCollections.js';
import {
    providerName,
    providersOf,
    scopesOf,
    stateOf,
    totalOf,
    totalsOf,
    type OverviewScope,
    type ProviderEntry,
    type RunState,
    type Totals,
} from './feedOverview.js';
import { useSyntheticStep } from './syntheticTrail.js';

/** A node of the overview: a scope, a provider or a feed kind. */
interface Node {
    readonly key: string;
    /** The address of the same node in the feed tree. */
    readonly treeKey: string;
    readonly name: string;
    readonly note?: string;
    readonly totals: Totals;
    readonly folders: readonly string[];
    readonly tags?: readonly string[];
}

function StateTag({ totals }: { readonly totals: Totals }): ReactNode {
    const { t } = useTranslation();
    const state: RunState = stateOf(totals);
    switch (state) {
        case 'running':
            return <Tag tone="up">{t('marketdata.feedOverview.state.running')}</Tag>;
        case 'stopped':
            return <Tag tone="muted">{t('marketdata.feedOverview.state.stopped')}</Tag>;
        case 'partial':
            return (
                <Tag tone="warn">
                    {t('marketdata.feedOverview.state.partial', {
                        running: String(totals.running),
                        total: String(totalOf(totals)),
                    })}
                </Tag>
            );
        case 'empty':
            return <Tag tone="muted">{t('marketdata.feedOverview.state.empty')}</Tag>;
    }
}

/**
 * Feed overview: what runs, by scope, provider and feed kind. The columns drill
 * down, each choice narrows the next, and the page never lists a feed. The
 * server counts the feeds and the running ones under every folder, so the page
 * reads a few folders and not the feeds. A provider or a kind can be started or
 * stopped; one request names the folder and the server walks its subtree.
 */
export function FeedOverviewPage(): ReactNode {
    return (
        <RecordStoreProvider store={syntheticStore}>
            <FeedOverview />
        </RecordStoreProvider>
    );
}

function FeedOverview(): ReactNode {
    const { t } = useTranslation();
    const runs = useFolderRuns();
    const synthetic = useSyntheticStep();

    const { collections, loaded, error } = useCollections();
    const catalogue = useRecords('providers');
    const names = new Map<string, string>(
        (catalogue.data ?? []).map((row) => [show(row['code']), show(row['name'])]),
    );

    const [chosenScope, setScope] = useState<OverviewScope | undefined>();
    const [chosenProvider, setProvider] = useState<string | undefined>();
    const [chosenKind, setKind] = useState<string | undefined>();
    const all = collections;
    const scopes = scopesOf(all);
    const scope =
        scopes.find((entry) => entry.scope === chosenScope) ??
        scopes.find((entry) => totalOf(entry.totals) > 0) ??
        scopes[0];
    const providers = providersOf(scope?.collections ?? []);
    const provider: ProviderEntry | undefined = providers.find(
        (entry) => entry.code === chosenProvider,
    );
    const providerIds = (provider?.collections ?? []).map((collection) => collection.id);
    const kinds = useQuery({
        queryKey: [...FOLDERS_KEY, 'kinds', providerIds],
        queryFn: async () =>
            (await Promise.all(providerIds.map((id) => api.feedFolders(id)))).flat(),
        enabled: providerIds.length > 0,
        refetchInterval: EVERY_MS,
    });
    const kindFolders: readonly FeedFolder[] = kinds.data ?? [];
    const kind = kindFolders.find((folder) => folder.id === chosenKind);

    const scopeNodes: readonly Node[] = scopes.map((entry) => ({
        key: entry.scope,
        treeKey: scopeKey(entry.scope),
        name: t(`marketdata.feedOverview.scope.${entry.scope}`),
        note: t(`marketdata.feedOverview.scopeNote.${entry.scope}`),
        totals: entry.totals,
        folders: entry.collections.map((collection) => collection.id),
    }));
    const providerNodes: readonly Node[] = providers.map((entry) => ({
        key: entry.code,
        treeKey:
            scope?.scope === 'system'
                ? providerKey('system', entry.code)
                : entry.collections.length === 1
                  ? folderKey(entry.collections[0]?.id ?? '')
                  : scopeKey('test'),
        name: t('marketdata.feedOverview.named', { name: providerName(entry.code, names) }),
        totals: entry.totals,
        folders: entry.collections.map((collection) => collection.id),
        tags: [
            t(entry.bound ? 'marketdata.feedOverview.bound' : 'marketdata.feedOverview.sandboxed'),
        ],
    }));
    const kindNodes: readonly Node[] = kindFolders.map((folder) => ({
        key: folder.id,
        treeKey: folderKey(folder.id),
        name:
            (provider?.collections.length ?? 0) > 1
                ? `${folder.name} · ${
                      all.find((collection) => collection.id === folder.parent_id)?.name ?? ''
                  }`
                : folder.name,
        totals: totalsOf([folder]),
        folders: [folder.id],
    }));

    const selected: Node | undefined =
        kindNodes.find((node) => node.key === kind?.id) ??
        providerNodes.find((node) => node.key === provider?.code) ??
        scopeNodes.find((node) => node.key === scope?.scope);
    const children: readonly Node[] =
        selected === undefined
            ? []
            : kindNodes.some((node) => node.key === selected.key)
              ? []
              : providerNodes.some((node) => node.key === selected.key)
                ? kindNodes
                : providerNodes;

    const ask = (action: 'start' | 'stop', node: Node): void =>
        runs.ask(action, { name: node.name, folders: node.folders, totals: node.totals });

    const failed = error !== '';
    const nothing = loaded && all.length === 0;

    return (
        <div className="space-y-6">
            <div>
                <AreaTrail
                    area="marketdata"
                    screen={t('marketdata.feedOverview.title')}
                    steps={[synthetic]}
                />
                <PageHeader
                    title={t('marketdata.feedOverview.title')}
                    description={t('marketdata.feedOverview.lead')}
                />
            </div>
            {failed && <Notice tone="error">{error}</Notice>}
            {!loaded && !failed && <p className="text-sm text-ink-muted">{t('common.loading')}</p>}
            {nothing && <Notice tone="info">{t('marketdata.feedOverview.none')}</Notice>}
            {all.length > 0 && (
                <>
                    <div className="grid gap-4 md:grid-cols-3">
                        <Column
                            title={t('marketdata.feedOverview.columns.scope')}
                            nodes={scopeNodes}
                            chosen={scope?.scope}
                            onChoose={(key) => {
                                setScope(key as OverviewScope);
                                setProvider(undefined);
                                setKind(undefined);
                            }}
                        />
                        <Column
                            title={t('marketdata.feedOverview.columns.provider')}
                            nodes={providerNodes}
                            chosen={provider?.code}
                            empty={t('marketdata.feedOverview.noProviders')}
                            onChoose={(key) => {
                                setProvider(key);
                                setKind(undefined);
                            }}
                        />
                        <Column
                            title={t('marketdata.feedOverview.columns.kind')}
                            nodes={kindNodes}
                            chosen={kind?.id}
                            empty={t('marketdata.feedOverview.chooseProvider')}
                            onChoose={setKind}
                        />
                    </div>
                    {selected !== undefined && (
                        <Detail
                            node={selected}
                            below={children}
                            mayRun={runs.mayRun}
                            onAsk={ask}
                            atKind={kindNodes.some((node) => node.key === selected.key)}
                        />
                    )}
                </>
            )}
            {runs.dialog}
        </div>
    );
}

/** One column of the drill-down: a heading and the nodes of one level. */
function Column({
    title,
    nodes,
    chosen,
    empty,
    onChoose,
}: {
    readonly title: string;
    readonly nodes: readonly Node[];
    readonly chosen: string | undefined;
    readonly empty?: string;
    readonly onChoose: (key: string) => void;
}): ReactNode {
    return (
        <section className="rounded-md border border-line">
            <h2 className="border-b border-line px-3 py-2 text-xs font-medium text-ink-muted">
                {title}
            </h2>
            {nodes.length === 0 ? (
                <p className="px-3 py-3 text-sm text-ink-muted">{empty ?? ''}</p>
            ) : (
                <ul>
                    {nodes.map((node) => (
                        <li key={node.key}>
                            <button
                                type="button"
                                aria-pressed={chosen === node.key}
                                className={cx(
                                    'flex w-full items-center justify-between gap-3 border-b border-line-subtle px-3 py-2 text-left text-sm last:border-b-0 hover:bg-surface-hover',
                                    chosen === node.key && 'bg-accent/10',
                                )}
                                onClick={() => onChoose(node.key)}
                            >
                                <span className="min-w-0">
                                    <span className="block truncate font-medium text-ink">
                                        {node.name}
                                    </span>
                                    <span className="block text-xs text-ink-faint">
                                        <Holds totals={node.totals} />
                                    </span>
                                </span>
                                <StateTag totals={node.totals} />
                            </button>
                        </li>
                    ))}
                </ul>
            )}
        </section>
    );
}

function Holds({ totals }: { readonly totals: Totals }): ReactNode {
    const { t } = useTranslation();
    return (
        <>
            {t('marketdata.feedOverview.holds', {
                fx: String(totals.fx),
                ir: String(totals.ir),
            })}
        </>
    );
}

/** The chosen node: its figures, its start and stop, and the nodes under it. */
function Detail({
    node,
    below,
    mayRun,
    onAsk,
    atKind,
}: {
    readonly node: Node;
    readonly below: readonly Node[];
    readonly mayRun: boolean;
    readonly onAsk: (action: 'start' | 'stop', node: Node) => void;
    readonly atKind: boolean;
}): ReactNode {
    const { t } = useTranslation();
    const total = totalOf(node.totals);
    const actions = (target: Node, small: boolean): ReactNode =>
        mayRun && (
            <span className="flex gap-2">
                <Button
                    size={small ? 'sm' : 'md'}
                    disabled={
                        target.folders.length === 0 ||
                        target.totals.running >= totalOf(target.totals)
                    }
                    onClick={() => onAsk('start', target)}
                >
                    {t('marketdata.feedOverview.start')}
                </Button>
                <Button
                    size={small ? 'sm' : 'md'}
                    disabled={target.totals.running === 0}
                    onClick={() => onAsk('stop', target)}
                >
                    {t('marketdata.feedOverview.stop')}
                </Button>
            </span>
        );
    return (
        <section className="space-y-4 rounded-md border border-line p-4">
            <div className="flex flex-wrap items-center gap-3">
                <h2 className="text-lg font-semibold">{node.name}</h2>
                {node.tags?.map((tag) => (
                    <Tag key={tag} tone="muted">
                        {tag}
                    </Tag>
                ))}
                <StateTag totals={node.totals} />
            </div>
            {node.note !== undefined && <p className="text-sm text-ink-muted">{node.note}</p>}
            <dl className="grid grid-cols-2 gap-3 text-sm sm:grid-cols-4">
                {[
                    ['feeds', total],
                    ['running', node.totals.running],
                    ['fx', node.totals.fx],
                    ['ir', node.totals.ir],
                ].map(([key, value]) => (
                    <div key={String(key)}>
                        <dt className="text-xs text-ink-muted">
                            {t(`marketdata.feedOverview.figures.${String(key)}`)}
                        </dt>
                        <dd className="text-xl font-semibold tabular-nums">
                            {Number(value).toLocaleString()}
                        </dd>
                    </div>
                ))}
            </dl>
            <div className="flex flex-wrap items-center gap-3">
                {actions(node, false)}
                <Link
                    className="text-sm text-accent hover:text-accent-bright"
                    to={`${PATHS.feedTree}?node=${encodeURIComponent(node.treeKey)}`}
                >
                    {t('marketdata.feedOverview.openInTree')}
                </Link>
                <span className="text-xs text-ink-faint">
                    {t('marketdata.feedOverview.requestNote', { name: node.name })}
                </span>
            </div>
            {below.length > 0 && (
                <div className="overflow-x-auto rounded-md border border-line">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                <th className="px-4 py-2 font-medium">
                                    {t('marketdata.feedOverview.folder')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('marketdata.feedOverview.columns.holds')}
                                </th>
                                <th className="px-4 py-2 text-right font-medium">
                                    {t('marketdata.feedOverview.figures.feeds')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('marketdata.feedOverview.columns.state')}
                                </th>
                                <th className="px-4 py-2" />
                            </tr>
                        </thead>
                        <tbody>
                            {below.map((child) => (
                                <tr
                                    key={child.key}
                                    className="border-b border-line-subtle last:border-b-0"
                                >
                                    <td className="px-4 py-2 font-medium">{child.name}</td>
                                    <td className="px-4 py-2 text-ink-muted">
                                        <Holds totals={child.totals} />
                                    </td>
                                    <td className="px-4 py-2 text-right tabular-nums">
                                        {totalOf(child.totals).toLocaleString()}
                                    </td>
                                    <td className="px-4 py-2">
                                        <StateTag totals={child.totals} />
                                    </td>
                                    <td className="px-4 py-2 text-right">{actions(child, true)}</td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            )}
            {atKind && (
                <Notice tone="info">
                    {t('marketdata.feedOverview.kindNote', { count: total.toLocaleString() })}
                </Notice>
            )}
        </section>
    );
}
