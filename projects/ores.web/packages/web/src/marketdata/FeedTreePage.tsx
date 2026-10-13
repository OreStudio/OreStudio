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

import { keepPreviousData, useQuery, useQueryClient } from '@tanstack/react-query';
import { useEffect, useState, type ReactNode } from 'react';
import { Link, useSearchParams } from 'react-router';
import { api, type FeedFolder, type FolderFeed } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { show, useRecords } from '../refdata/records.js';
import { RecordStoreProvider, syntheticStore } from '../refdata/store.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { PATHS } from '../shell/paths.js';
import { Button, Input, Notice, PageHeader, Tag, cx } from '../ui/Primitives.js';
import { toast } from '../ui/Toasts.js';
import { providerName, stateOf, totalOf, type Totals } from './feedOverview.js';
import { FeedStatus, RUNNING_KEY, useRunningFeeds } from './feedRuns.js';
import {
    folderChain,
    folderKey,
    folderNode,
    holdsFeeds,
    loadsFrom,
    madeChildren,
    mayOpen,
    parseKey,
    providerKey,
    scopeNodes,
    type FolderRow,
    type TreeNode,
} from './feedTree.js';
import { FOLDERS_KEY, useFolderRuns } from './folderRuns.js';
import { useSyntheticStep } from './syntheticTrail.js';
import { EVERY_MS, useCollections } from './useCollections.js';

/** How many feeds a page of a folder lists. */
const PAGE = 60;

/** The address of a feed's own page. */
export function feedPageOf(feed: Pick<FolderFeed, 'kind' | 'id'>): string {
    return `${feed.kind === 'ir_curve' ? PATHS.irFeeds : PATHS.fxFeeds}/${encodeURIComponent(feed.id)}`;
}

/** What a node is called on the screen: the scope or the provider in words, a folder by its name. */
function useLabeller(): (node: TreeNode) => string {
    const { t } = useTranslation();
    const catalogue = useRecords('providers');
    const names = new Map<string, string>(
        (catalogue.data ?? []).map((row) => [show(row['code']), show(row['name'])]),
    );
    return (node) => {
        if (node.role === 'scope') {
            return t(`marketdata.feedOverview.scope.${node.scope ?? 'test'}`);
        }
        if (node.role === 'provider') {
            return t('marketdata.feedOverview.named', {
                name: providerName(node.providerCode ?? '', names),
            });
        }
        return node.name;
    };
}

/** The nodes under a node: made from the collections, or read from the service when opened. */
function useChildren(
    node: TreeNode,
    collections: readonly FeedFolder[],
    wanted: boolean,
): readonly TreeNode[] {
    const made = madeChildren(node, collections);
    const from = loadsFrom(node);
    const loaded = useQuery({
        queryKey: [...FOLDERS_KEY, 'children', from],
        queryFn: () => api.feedFolders(from ?? ''),
        enabled: wanted && made === undefined && from !== undefined,
        refetchInterval: EVERY_MS,
    });
    return made ?? (loaded.data ?? []).map(folderNode);
}

function StateChip({ totals }: { readonly totals: Totals }): ReactNode {
    const { t } = useTranslation();
    const all = totalOf(totals);
    return (
        <span
            className="flex items-center gap-2 text-xs text-ink-muted"
            title={t(
                `marketdata.feedOverview.state.${stateOf(totals) === 'partial' ? 'partial' : stateOf(totals)}`,
                {
                    running: String(totals.running),
                    total: String(all),
                },
            )}
        >
            <span className="tabular-nums">
                {totals.running}/{all}
            </span>
            <span className="h-1.5 w-10 overflow-hidden rounded-full bg-line">
                <span
                    className="block h-full bg-up"
                    style={{ width: `${all === 0 ? 0 : (100 * totals.running) / all}%` }}
                />
            </span>
        </span>
    );
}

/**
 * Feed tree: every feed by scope, provider and feed kind. The tree opens a level
 * at a time, each level counted by the service, so it never reads the feeds. The
 * chosen node has its figures, Start and Stop, and the nodes under it; the
 * folder that holds feeds lists them a page at a time, and a feed opens on its own
 * page. The search finds feeds by words in their name, source or URI.
 */
export function FeedTreePage(): ReactNode {
    return (
        <RecordStoreProvider store={syntheticStore}>
            <FeedTree />
        </RecordStoreProvider>
    );
}

function FeedTree(): ReactNode {
    const { t } = useTranslation();
    const synthetic = useSyntheticStep();
    const label = useLabeller();
    const runs = useFolderRuns();
    const [params, setParams] = useSearchParams();
    const { collections, loaded, error } = useCollections();
    const folderRows = useRecords('folders');
    const rows: readonly FolderRow[] = (folderRows.data ?? []).map((row) => ({
        id: show(row['id']),
        parent_id: show(row['parent_id']),
        name: show(row['name']),
        kind: show(row['kind']),
    }));

    const [expanded, setExpanded] = useState<ReadonlySet<string>>(new Set());
    const [query, setQuery] = useState('');
    const [runningOnly, setRunningOnly] = useState(false);

    const scopes = scopeNodes(collections);
    const key = params.get('node') ?? '';
    const parsed = parseKey(key);
    const folderId = parsed?.kind === 'folder' ? parsed.id : undefined;
    const known = collections.find((collection) => collection.id === folderId);
    const parentOf = rows.find((row) => row.id === folderId)?.parent_id;
    const siblings = useQuery({
        queryKey: [...FOLDERS_KEY, 'siblings', parentOf],
        queryFn: () => api.feedFolders(parentOf ?? ''),
        enabled: folderId !== undefined && known === undefined && parentOf !== undefined,
    });
    const folder = known ?? siblings.data?.find((candidate) => candidate.id === folderId);

    const selected: TreeNode | undefined = (() => {
        if (parsed?.kind === 'scope') {
            return scopes.find((scope) => scope.scope === parsed.scope);
        }
        if (parsed?.kind === 'provider') {
            const system = scopes.find((scope) => scope.scope === 'system');
            return system === undefined
                ? undefined
                : (madeChildren(system, collections) ?? []).find(
                      (node) => node.key === providerKey('system', parsed.code),
                  );
        }
        if (folder !== undefined) {
            return folderNode(folder);
        }
        return folderId === undefined
            ? (scopes.find((scope) => totalOf(scope.totals) > 0) ?? scopes[0])
            : undefined;
    })();

    // A node chosen by its address is shown opened: its scope, its provider and its folders.
    useEffect(() => {
        if (selected === undefined) {
            return;
        }
        const open = new Set<string>();
        if (selected.role === 'scope') {
            open.add(selected.key);
        } else {
            const home = collections.find(
                (collection) =>
                    collection.id === (selected.folder?.id ?? selected.folderIds[0]) ||
                    folderChain(rows, selected.folder?.id ?? '').some(
                        (row) => row.id === collection.id,
                    ),
            );
            const scope = home?.scope === 'system' ? 'system' : 'test';
            open.add(`scope:${scope}`);
            if (scope === 'system' && home !== undefined) {
                open.add(
                    providerKey(
                        'system',
                        home.provider_code === '' ? 'generic' : home.provider_code,
                    ),
                );
            }
            for (const row of folderChain(rows, selected.folder?.id ?? '')) {
                open.add(folderKey(row.id));
            }
        }
        setExpanded((current) => new Set([...current, ...open]));
        // Only a new address opens nodes, so a person's own closing is kept.
    }, [key, loaded, folderRows.data]);

    const choose = (node: TreeNode): void => {
        setQuery('');
        setParams({ node: node.key }, { replace: true });
    };
    const toggle = (node: TreeNode): void =>
        setExpanded((current) => {
            const next = new Set(current);
            if (next.has(node.key)) {
                next.delete(node.key);
            } else {
                next.add(node.key);
            }
            return next;
        });

    return (
        <div className="space-y-6">
            <div>
                <AreaTrail
                    area="marketdata"
                    screen={t('marketdata.feedTree.title')}
                    steps={[synthetic]}
                />
                <PageHeader
                    title={t('marketdata.feedTree.title')}
                    description={t('marketdata.feedTree.lead')}
                />
            </div>
            {error !== '' && <Notice tone="error">{error}</Notice>}
            {loaded && collections.length === 0 && (
                <Notice tone="info">{t('marketdata.feedOverview.none')}</Notice>
            )}
            {collections.length > 0 && (
                <div className="grid items-start gap-4 lg:grid-cols-[22rem_1fr]">
                    <section className="rounded-md border border-line">
                        <div className="space-y-2 border-b border-line p-3">
                            <Input
                                type="search"
                                value={query}
                                placeholder={t('marketdata.feedTree.search')}
                                aria-label={t('marketdata.feedTree.search')}
                                onChange={(event) => setQuery(event.target.value)}
                            />
                            <label className="flex items-center gap-2 text-sm text-ink-muted">
                                <input
                                    type="checkbox"
                                    checked={runningOnly}
                                    onChange={(event) => setRunningOnly(event.target.checked)}
                                />
                                {t('marketdata.feedTree.runningOnly')}
                            </label>
                        </div>
                        <ul
                            role="tree"
                            aria-label={t('marketdata.feedTree.title')}
                            className="py-1"
                        >
                            {scopes.map((node) => (
                                <Branch
                                    key={node.key}
                                    node={node}
                                    depth={0}
                                    collections={collections}
                                    expanded={expanded}
                                    chosen={selected?.key}
                                    runningOnly={runningOnly}
                                    label={label}
                                    onToggle={toggle}
                                    onChoose={choose}
                                />
                            ))}
                        </ul>
                    </section>
                    <div className="min-w-0">
                        {query.trim() !== '' ? (
                            <SearchResults
                                query={query.trim()}
                                rows={rows}
                                mayRun={runs.mayRun}
                                runningOnly={runningOnly}
                            />
                        ) : selected === undefined ? (
                            <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                        ) : (
                            <NodePanel
                                node={selected}
                                collections={collections}
                                label={label}
                                runningOnly={runningOnly}
                                mayRun={runs.mayRun}
                                onAsk={(action, node) =>
                                    runs.ask(action, {
                                        name: label(node),
                                        folders: node.folderIds,
                                        totals: node.totals,
                                    })
                                }
                                onChoose={choose}
                            />
                        )}
                    </div>
                </div>
            )}
            {runs.dialog}
        </div>
    );
}

/** One node of the tree and, when it is open, the nodes under it. */
function Branch({
    node,
    depth,
    collections,
    expanded,
    chosen,
    runningOnly,
    label,
    onToggle,
    onChoose,
}: {
    readonly node: TreeNode;
    readonly depth: number;
    readonly collections: readonly FeedFolder[];
    readonly expanded: ReadonlySet<string>;
    readonly chosen: string | undefined;
    readonly runningOnly: boolean;
    readonly label: (node: TreeNode) => string;
    readonly onToggle: (node: TreeNode) => void;
    readonly onChoose: (node: TreeNode) => void;
}): ReactNode {
    const open = expanded.has(node.key);
    const kids = useChildren(node, collections, open).filter(
        (kid) => !runningOnly || kid.totals.running > 0,
    );
    return (
        <li role="treeitem" aria-expanded={mayOpen(node) ? open : undefined}>
            <div
                className={cx(
                    'flex items-center gap-1 pr-3 text-sm hover:bg-surface-hover',
                    chosen === node.key && 'bg-accent/10',
                )}
                style={{ paddingLeft: `${8 + depth * 16}px` }}
            >
                <button
                    type="button"
                    aria-label={label(node)}
                    className="w-4 shrink-0 text-ink-faint"
                    disabled={!mayOpen(node)}
                    onClick={() => onToggle(node)}
                >
                    {mayOpen(node) ? (open ? '▾' : '▸') : ''}
                </button>
                <button
                    type="button"
                    className="flex min-w-0 flex-1 items-center justify-between gap-2 py-1.5 text-left"
                    onClick={() => onChoose(node)}
                >
                    <span className="truncate">{label(node)}</span>
                    <StateChip totals={node.totals} />
                </button>
            </div>
            {open && kids.length > 0 && (
                <ul role="group">
                    {kids.map((kid) => (
                        <Branch
                            key={kid.key}
                            node={kid}
                            depth={depth + 1}
                            collections={collections}
                            expanded={expanded}
                            chosen={chosen}
                            runningOnly={runningOnly}
                            label={label}
                            onToggle={onToggle}
                            onChoose={onChoose}
                        />
                    ))}
                </ul>
            )}
        </li>
    );
}

/** The chosen node: its figures, Start and Stop, and what is under it. */
function NodePanel({
    node,
    collections,
    label,
    runningOnly,
    mayRun,
    onAsk,
    onChoose,
}: {
    readonly node: TreeNode;
    readonly collections: readonly FeedFolder[];
    readonly label: (node: TreeNode) => string;
    readonly runningOnly: boolean;
    readonly mayRun: boolean;
    readonly onAsk: (action: 'start' | 'stop', node: TreeNode) => void;
    readonly onChoose: (node: TreeNode) => void;
}): ReactNode {
    const { t } = useTranslation();
    const kids = useChildren(node, collections, true).filter(
        (kid) => !runningOnly || kid.totals.running > 0,
    );
    const total = totalOf(node.totals);
    const state = stateOf(node.totals);
    const actions = (target: TreeNode): ReactNode =>
        mayRun && (
            <span className="flex gap-2">
                <Button
                    size="sm"
                    disabled={
                        target.folderIds.length === 0 ||
                        target.totals.running >= totalOf(target.totals)
                    }
                    onClick={() => onAsk('start', target)}
                >
                    {t('marketdata.feedOverview.start')}
                </Button>
                <Button
                    size="sm"
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
                <h2 className="text-lg font-semibold">{label(node)}</h2>
                {node.folder?.provider_code !== undefined && node.folder.provider_code !== '' && (
                    <Tag tone="muted">{node.folder.provider_code}</Tag>
                )}
                {node.folder?.binding_mode !== undefined && node.folder.binding_mode !== '' && (
                    <Tag tone={node.folder.binding_mode === 'bound' ? 'up' : 'muted'}>
                        {t(
                            node.folder.binding_mode === 'bound'
                                ? 'marketdata.feedOverview.bound'
                                : 'marketdata.feedOverview.sandboxed',
                        )}
                    </Tag>
                )}
                <Tag tone={state === 'running' ? 'up' : state === 'partial' ? 'warn' : 'muted'}>
                    {t(`marketdata.feedOverview.state.${state}`, {
                        running: String(node.totals.running),
                        total: String(total),
                    })}
                </Tag>
            </div>
            {node.role === 'scope' && (
                <p className="text-sm text-ink-muted">
                    {t(`marketdata.feedOverview.scopeNote.${node.scope ?? 'test'}`)}
                </p>
            )}
            <dl className="grid grid-cols-2 gap-3 text-sm sm:grid-cols-4">
                {(
                    [
                        ['feeds', total],
                        ['running', node.totals.running],
                        ['fx', node.totals.fx],
                        ['ir', node.totals.ir],
                    ] as const
                ).map(([name, value]) => (
                    <div key={name}>
                        <dt className="text-xs text-ink-muted">
                            {t(`marketdata.feedOverview.figures.${name}`)}
                        </dt>
                        <dd className="text-xl font-semibold tabular-nums">
                            {value.toLocaleString()}
                        </dd>
                    </div>
                ))}
            </dl>
            <div className="flex flex-wrap items-center gap-3">
                {actions(node)}
                <span className="text-xs text-ink-faint">
                    {t('marketdata.feedOverview.requestNote', { name: label(node) })}
                </span>
            </div>
            {holdsFeeds(node) && node.folder !== undefined ? (
                <FeedsTable folder={node.folder.id} runningOnly={runningOnly} mayRun={mayRun} />
            ) : (
                kids.length > 0 && (
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
                                {kids.map((kid) => (
                                    <tr
                                        key={kid.key}
                                        className="cursor-pointer border-b border-line-subtle last:border-b-0 hover:bg-surface-hover"
                                        onClick={() => onChoose(kid)}
                                    >
                                        <td className="px-4 py-2 font-medium">{label(kid)}</td>
                                        <td className="px-4 py-2 text-ink-muted">
                                            {t('marketdata.feedOverview.holds', {
                                                fx: String(kid.totals.fx),
                                                ir: String(kid.totals.ir),
                                            })}
                                        </td>
                                        <td className="px-4 py-2 text-right tabular-nums">
                                            {totalOf(kid.totals).toLocaleString()}
                                        </td>
                                        <td className="px-4 py-2">
                                            <StateChip totals={kid.totals} />
                                        </td>
                                        <td
                                            className="px-4 py-2 text-right"
                                            onClick={(event) => event.stopPropagation()}
                                        >
                                            {actions(kid)}
                                        </td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    </div>
                )
            )}
        </section>
    );
}

/** The feeds of one folder, a page at a time, each with its state, Start and Stop, and its page. */
function FeedsTable({
    folder,
    runningOnly,
    mayRun,
}: {
    readonly folder: string;
    readonly runningOnly: boolean;
    readonly mayRun: boolean;
}): ReactNode {
    return (
        <FeedRows
            folder={folder}
            search=""
            runningOnly={runningOnly}
            mayRun={mayRun}
            paths={null}
        />
    );
}

/** The feeds that match the words in the search box, wherever they sit. */
function SearchResults({
    query,
    rows,
    mayRun,
    runningOnly,
}: {
    readonly query: string;
    readonly rows: readonly FolderRow[];
    readonly mayRun: boolean;
    readonly runningOnly: boolean;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="space-y-3 rounded-md border border-line p-4">
            <h2 className="text-lg font-semibold">{t('marketdata.feedTree.results', { query })}</h2>
            <FeedRows
                folder=""
                search={query}
                runningOnly={runningOnly}
                mayRun={mayRun}
                paths={rows}
            />
        </section>
    );
}

function FeedRows({
    folder,
    search,
    runningOnly,
    mayRun,
    paths,
}: {
    readonly folder: string;
    readonly search: string;
    readonly runningOnly: boolean;
    readonly mayRun: boolean;
    /** The folders, to show where each feed sits; null when the folder is already named. */
    readonly paths: readonly FolderRow[] | null;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const running = useRunningFeeds();
    const [page, setPage] = useState(0);
    useEffect(() => setPage(0), [folder, search]);
    const feeds = useQuery({
        queryKey: [...FOLDERS_KEY, 'feeds', folder, search, page],
        queryFn: () => api.folderFeeds({ folder, search, offset: page * PAGE, limit: PAGE }),
        placeholderData: keepPreviousData,
        refetchInterval: EVERY_MS * 3,
    });
    const shown = (feeds.data?.feeds ?? []).filter(
        (feed) => !runningOnly || running?.has(feed.source_name) === true,
    );
    const total = feeds.data?.total ?? 0;
    const act = async (feed: FolderFeed, action: 'start' | 'stop'): Promise<void> => {
        try {
            await (action === 'start' ? api.startFeed(feed.id) : api.stopFeed(feed.id));
        } catch (error) {
            toast(error instanceof Error ? error.message : String(error), 'warn');
        }
        await queries.invalidateQueries({ queryKey: FOLDERS_KEY });
        await queries.invalidateQueries({ queryKey: RUNNING_KEY });
    };

    if (feeds.isError) {
        return <Notice tone="error">{feeds.error.message}</Notice>;
    }
    if (feeds.data === undefined) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (total === 0) {
        return <p className="text-sm text-ink-muted">{t('marketdata.feedTree.noFeeds')}</p>;
    }
    return (
        <div className="space-y-2">
            <div className="overflow-x-auto rounded-md border border-line">
                <table className="w-full text-left text-sm">
                    <thead>
                        <tr className="border-b border-line text-xs text-ink-muted">
                            <th className="px-4 py-2 font-medium">
                                {t('marketdata.feedTree.columns.feed')}
                            </th>
                            <th className="px-4 py-2 font-medium">
                                {t('marketdata.feedTree.columns.source')}
                            </th>
                            {paths !== null && (
                                <th className="px-4 py-2 font-medium">
                                    {t('marketdata.feedTree.columns.where')}
                                </th>
                            )}
                            <th className="px-4 py-2 font-medium">
                                {t('marketdata.feedOverview.columns.state')}
                            </th>
                            <th className="px-4 py-2" />
                        </tr>
                    </thead>
                    <tbody>
                        {shown.map((feed) => (
                            <tr
                                key={feed.id}
                                className="border-b border-line-subtle last:border-b-0"
                            >
                                <td className="px-4 py-2 font-medium">
                                    <Link
                                        className="text-accent hover:text-accent-bright"
                                        to={feedPageOf(feed)}
                                    >
                                        {feed.label}
                                    </Link>
                                </td>
                                <td className="px-4 py-2 font-mono text-xs text-ink-muted">
                                    {feed.source_name}
                                </td>
                                {paths !== null && (
                                    <td className="px-4 py-2 text-xs text-ink-muted">
                                        {folderChain(paths, feed.folder_id)
                                            .map((row) => row.name)
                                            .join(' › ')}
                                    </td>
                                )}
                                <td className="px-4 py-2">
                                    <FeedStatus running={running?.has(feed.source_name)} />
                                </td>
                                <td className="px-4 py-2 text-right whitespace-nowrap">
                                    {mayRun && (
                                        <span className="mr-2 inline-flex gap-2">
                                            <Button
                                                size="sm"
                                                disabled={running?.has(feed.source_name) === true}
                                                onClick={() => void act(feed, 'start')}
                                            >
                                                {t('marketdata.feedOverview.start')}
                                            </Button>
                                            <Button
                                                size="sm"
                                                disabled={running?.has(feed.source_name) !== true}
                                                onClick={() => void act(feed, 'stop')}
                                            >
                                                {t('marketdata.feedOverview.stop')}
                                            </Button>
                                        </span>
                                    )}
                                    <Link
                                        className="text-sm text-accent hover:text-accent-bright"
                                        to={feedPageOf(feed)}
                                    >
                                        {t('marketdata.feedTree.open')}
                                    </Link>
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </div>
            <div className="flex items-center justify-between text-sm text-ink-muted">
                <span>
                    {t('marketdata.feedTree.range', {
                        from: String(page * PAGE + 1),
                        to: String(Math.min(total, (page + 1) * PAGE)),
                        total: total.toLocaleString(),
                    })}
                </span>
                <span className="flex gap-2">
                    <Button size="sm" disabled={page === 0} onClick={() => setPage(page - 1)}>
                        {t('marketdata.feedTree.previous')}
                    </Button>
                    <Button
                        size="sm"
                        disabled={(page + 1) * PAGE >= total}
                        onClick={() => setPage(page + 1)}
                    >
                        {t('marketdata.feedTree.next')}
                    </Button>
                </span>
            </div>
        </div>
    );
}
