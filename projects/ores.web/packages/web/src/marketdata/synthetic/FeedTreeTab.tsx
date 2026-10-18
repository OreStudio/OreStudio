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
import { useEffect, useRef, useState, type ReactNode } from 'react';
import { Link, useNavigate, useSearchParams } from 'react-router';
import { api, type FeedFilter, type FeedFolder, type FolderFeed } from '../../api/client.js';
import { FlagOf } from '../../images/flags.js';
import { useTranslation } from '../../i18n/Provider.js';
import { OresmdUrlResolved } from '../../oresmd/OresmdUrl.js';
import { useSeriesTargets } from '../../oresmd/useSeriesTargets.js';
import { show, useRecords } from '../../refdata/records.js';
import { useLabelCatalogue } from '../../refdata/shared.js';
import { DEFAULT_PAGE_SIZE, Pager, pageBounds } from '../../ui/Pager.js';
import { Label } from '../../ui/Label.js';
import { Button, Input, Notice, Select, Tag, cx } from '../../ui/Primitives.js';
import { AddFeed, useMayAddFeed } from './AddFeed.js';
import { StateTag, useProviderFacts } from './DashboardTab.js';
import { frequencyOf } from './frequency.js';
import {
    ASSET_CLASSES,
    collectionOf,
    feedPageOf,
    filterOf,
    folderChain,
    folderKey,
    folderNode,
    loadsFrom,
    madeChildren,
    mayOpen,
    parseKey,
    providerKey,
    providerNodes,
    type FolderRow,
    type TreeNode,
} from './feedTree.js';
import { engineName } from './IrProcessPanel.js';
import { providerName } from './providers.js';
import {
    NOTHING,
    allPicked,
    everything,
    isPicked,
    pickedCount,
    selectionRequest,
    sendable,
    setPicked,
    type Selection,
} from './selection.js';
import { FEEDS_KEY, useSelectionRuns } from './selectionRuns.js';
import { totalOf, type Totals } from './totals.js';
import { EVERY_MS, FOLDERS_KEY, useCollections } from './useCollections.js';

type State = 'all' | 'started' | 'stopped';

const STATES: readonly State[] = ['all', 'started', 'stopped'];

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

function Roll({ totals }: { readonly totals: Totals }): ReactNode {
    const { t } = useTranslation();
    const all = totalOf(totals);
    return (
        <span
            className="flex items-center gap-2 text-xs text-ink-muted"
            title={t('marketdata.synthetic.tree.roll', {
                running: String(totals.running),
                total: String(all),
            })}
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

/** One node of the tree and, when it is open, the nodes under it. */
function Branch({
    node,
    depth,
    collections,
    expanded,
    chosen,
    label,
    onToggle,
    onChoose,
}: {
    readonly node: TreeNode;
    readonly depth: number;
    readonly collections: readonly FeedFolder[];
    readonly expanded: ReadonlySet<string>;
    readonly chosen: string;
    readonly label: (node: TreeNode) => string;
    readonly onToggle: (node: TreeNode) => void;
    readonly onChoose: (node: TreeNode) => void;
}): ReactNode {
    const open = expanded.has(node.key);
    const kids = useChildren(node, collections, open);
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
                    <Roll totals={node.totals} />
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

/** What the person typed, sent on after a pause so a search does not ask for every key. */
function useDebounced(value: string, ms: number): string {
    const [held, setHeld] = useState(value);
    useEffect(() => {
        const id = setTimeout(() => setHeld(value), ms);
        return () => clearTimeout(id);
    }, [value, ms]);
    return held;
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

/** The words of a column header. The series is named as the field it is, on every screen. */
function columnWords(column: string): string {
    return column === 'series'
        ? 'refdata.fields.oresmd_uri'
        : `marketdata.synthetic.tree.columns.${column}`;
}

/**
 * The feed tree: the providers and their folders on the left, and on the right a
 * table of the feeds under the chosen node, a page at a time. The table is
 * filtered by state, asset class and words, all on the server. A person picks
 * rows, or every row the filter matches, and starts or stops the pick with one
 * button; the server finds the feeds. A row opens the feed's own page.
 */
export function FeedTreeTab(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const [params, setParams] = useSearchParams();
    const { collections, loaded, error } = useCollections();
    const facts = useProviderFacts();
    const labels = useLabelCatalogue();
    const folderRows = useRecords('folders');
    const rows: readonly FolderRow[] = (folderRows.data ?? []).map((row) => ({
        id: show(row['id']),
        parent_id: show(row['parent_id']),
        name: show(row['name']),
        kind: show(row['kind']),
    }));

    const nodeKey = params.get('node') ?? '';
    const parsed = parseKey(nodeKey);
    const stateParam = params.get('state');
    const state: State = STATES.find((one) => one === stateParam) ?? 'all';
    const assetClass = params.get('class') ?? '';
    const shouldRun = params.get('shouldRun') === 'true';
    const pageSize = Number(params.get('size')) || DEFAULT_PAGE_SIZE;
    const offset = Math.max(0, Number(params.get('offset')) || 0);
    const search = params.get('q') ?? '';

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

    const [typed, setTyped] = useState(search);
    const sent = useDebounced(typed, 300);
    useEffect(() => {
        if (sent.trim() !== search) {
            narrow({ q: sent.trim() });
        }
        // The address is the truth; only a new pause in typing writes it.
    }, [sent]);

    const filter: FeedFilter = {
        ...(parsed?.kind === 'provider' ? { provider: parsed.code } : {}),
        ...(parsed?.kind === 'folder' ? { folder: parsed.id } : {}),
        ...(assetClass === '' ? {} : { assetClass }),
        ...(state === 'all' ? {} : { state }),
        ...(shouldRun ? { shouldRun: true } : {}),
        ...(search === '' ? {} : { search }),
    };
    const filterKey = JSON.stringify(filter);

    const page = useQuery({
        queryKey: [...FEEDS_KEY, filterKey, offset, pageSize],
        queryFn: () => api.folderFeeds(filter, { offset, limit: Math.min(pageSize, 200) }),
        placeholderData: keepPreviousData,
        refetchInterval: EVERY_MS,
    });
    const feeds = page.data?.feeds ?? [];
    const total = page.data?.total ?? 0;
    // The series page of every row on the page, decided in one read about the page,
    // so a grid of many feeds does not ask the codec about a URI a row.
    const targetOf = useSeriesTargets(feeds.map((feed) => feed.oresmd_uri));

    const [picked, setPicked_] = useState<Selection>(NOTHING);
    useEffect(() => setPicked_(NOTHING), [filterKey]);
    const runs = useSelectionRuns(() => setPicked_(NOTHING));
    const mayAdd = useMayAddFeed();
    const [adding, setAdding] = useState(false);

    const [expanded, setExpanded] = useState<ReadonlySet<string>>(new Set());
    const roots = providerNodes(collections);
    const label = (node: TreeNode): string =>
        node.role === 'provider' ? providerName(node.providerCode ?? '', facts) : node.name;
    // The tree starts fully open, once, when the providers have loaded and the folders have
    // been asked for, so a tenant with no folders still sees its providers open.
    const opened = useRef(false);
    useEffect(() => {
        if (opened.current || collections.length === 0 || folderRows.isLoading) {
            return;
        }
        opened.current = true;
        setExpanded(
            (current) =>
                new Set([
                    ...current,
                    ...roots.map((node) => node.key),
                    ...rows
                        .filter((row) => row.kind !== 'root' && row.kind !== 'instrument_type')
                        .map((row) => folderKey(row.id)),
                ]),
        );
    }, [collections, folderRows.data]);
    useEffect(() => {
        if (parsed?.kind === 'folder') {
            const chain = folderChain(rows, parsed.id);
            const home = collections.find((one) => one.id === chain[0]?.id);
            setExpanded(
                (current) =>
                    new Set([
                        ...current,
                        ...(home === undefined ? [] : [providerKey(home.provider_code)]),
                        ...chain.map((row) => folderKey(row.id)),
                    ]),
            );
        } else if (parsed?.kind === 'provider') {
            setExpanded((current) => new Set([...current, providerKey(parsed.code)]));
        }
        // Only a new address opens nodes, so a person's own closing is kept.
    }, [nodeKey, loaded, folderRows.data]);

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
    const heading =
        parsed?.kind === 'provider'
            ? providerName(parsed.code, facts)
            : parsed?.kind === 'folder'
              ? (rows.find((row) => row.id === parsed.id)?.name ?? '')
              : t('marketdata.synthetic.tree.all');

    const chosenCollection = collectionOf(parsed, collections, rows);

    const ids = feeds.map((feed) => feed.id);
    const count = pickedCount(picked, total);
    const names = (): string =>
        t('marketdata.synthetic.tree.pickName', { count: count.toLocaleString() });
    const ask = (action: 'start' | 'stop'): void =>
        runs.ask(action, { name: names(), selection: selectionRequest(picked, filter) });
    const { first, last } = pageBounds(offset, feeds.length);
    const showProvider = parsed?.kind !== 'provider' && parsed?.kind !== 'folder';

    return (
        <div className="space-y-4">
            {error !== '' && <Notice tone="error">{error}</Notice>}
            {loaded && collections.length === 0 && (
                <Notice tone="info">{t('marketdata.synthetic.dashboard.none')}</Notice>
            )}
            {collections.length > 0 && (
                <div className="grid items-start gap-4 lg:grid-cols-[20rem_1fr]">
                    <section className="rounded-md border border-line">
                        <ul
                            role="tree"
                            aria-label={t('marketdata.synthetic.tabs.tree')}
                            className="py-1"
                        >
                            <li role="treeitem">
                                <button
                                    type="button"
                                    className={cx(
                                        'w-full px-3 py-1.5 text-left text-sm hover:bg-surface-hover',
                                        nodeKey === '' && 'bg-accent/10',
                                    )}
                                    onClick={() => narrow({ node: undefined })}
                                >
                                    {t('marketdata.synthetic.tree.all')}
                                </button>
                            </li>
                            {roots.map((node) => (
                                <Branch
                                    key={node.key}
                                    node={node}
                                    depth={0}
                                    collections={collections}
                                    expanded={expanded}
                                    chosen={nodeKey}
                                    label={label}
                                    onToggle={toggle}
                                    onChoose={(chosen) => narrow({ node: chosen.key })}
                                />
                            ))}
                        </ul>
                    </section>
                    <div className="min-w-0 space-y-3">
                        <div className="flex flex-wrap items-center gap-2">
                            <Input
                                type="search"
                                className="min-w-60 flex-1"
                                value={typed}
                                placeholder={t('marketdata.synthetic.tree.search')}
                                aria-label={t('marketdata.synthetic.tree.search')}
                                onChange={(event) => setTyped(event.target.value)}
                            />
                            <div className="w-36 shrink-0">
                                <Select
                                    value={state}
                                    aria-label={t('marketdata.synthetic.tree.stateFilter')}
                                    onChange={(event) =>
                                        narrow({
                                            state:
                                                event.target.value === 'all'
                                                    ? ''
                                                    : event.target.value,
                                        })
                                    }
                                >
                                    {STATES.map((one) => (
                                        <option key={one} value={one}>
                                            {t(`marketdata.synthetic.tree.states.${one}`)}
                                        </option>
                                    ))}
                                </Select>
                            </div>
                            <div className="w-48 shrink-0">
                                <Select
                                    value={assetClass}
                                    aria-label={t('marketdata.synthetic.tree.classFilter')}
                                    onChange={(event) => narrow({ class: event.target.value })}
                                >
                                    <option value="">
                                        {t('marketdata.synthetic.tree.allClasses')}
                                    </option>
                                    {ASSET_CLASSES.map((code) => (
                                        <option key={code} value={code}>
                                            {t(`marketdata.synthetic.assetClass.${code}`)}
                                        </option>
                                    ))}
                                </Select>
                            </div>
                            {mayAdd && (
                                <Button variant="primary" onClick={() => setAdding(true)}>
                                    {t('marketdata.synthetic.tree.add')}
                                </Button>
                            )}
                        </div>
                        <p className="flex flex-wrap items-center gap-2 text-sm text-ink-muted">
                            <strong className="text-ink">
                                {search === ''
                                    ? heading
                                    : t('marketdata.synthetic.tree.matches', {
                                          count: total.toLocaleString(),
                                      })}
                            </strong>
                            {search === '' && (
                                <span>
                                    {t('marketdata.synthetic.tree.feedsCount', {
                                        count: total.toLocaleString(),
                                    })}
                                </span>
                            )}
                            {shouldRun && (
                                <Tag tone="warn">
                                    {t('marketdata.synthetic.tree.shouldRun')}{' '}
                                    <button
                                        type="button"
                                        aria-label={t('marketdata.synthetic.tree.clearShouldRun')}
                                        onClick={() => narrow({ shouldRun: undefined })}
                                    >
                                        ×
                                    </button>
                                </Tag>
                            )}
                        </p>
                        {count > 0 && (
                            <div className="flex flex-wrap items-center gap-3 rounded-md border border-accent/40 bg-accent/10 px-3 py-2 text-sm">
                                <strong>
                                    {t('marketdata.synthetic.tree.selected', {
                                        count: count.toLocaleString(),
                                    })}
                                </strong>
                                {picked.mode === 'ids' &&
                                    allPicked(picked, ids) &&
                                    total > ids.length && (
                                        <button
                                            type="button"
                                            className="text-accent hover:text-accent-bright"
                                            onClick={() => setPicked_(everything)}
                                        >
                                            {t('marketdata.synthetic.tree.selectAll', {
                                                count: total.toLocaleString(),
                                            })}
                                        </button>
                                    )}
                                {picked.mode === 'all' && (
                                    <span className="text-ink-muted">
                                        {t('marketdata.synthetic.tree.allMatching')}
                                    </span>
                                )}
                                <span className="ml-auto flex items-center gap-2">
                                    {runs.mayRun && (
                                        <>
                                            <Button
                                                size="sm"
                                                disabled={!sendable(picked)}
                                                onClick={() => ask('start')}
                                            >
                                                {t('marketdata.synthetic.runs.start')}
                                            </Button>
                                            <Button
                                                size="sm"
                                                disabled={!sendable(picked)}
                                                onClick={() => ask('stop')}
                                            >
                                                {t('marketdata.synthetic.runs.stop')}
                                            </Button>
                                        </>
                                    )}
                                    <Button
                                        size="sm"
                                        variant="ghost"
                                        onClick={() => setPicked_(NOTHING)}
                                    >
                                        {t('marketdata.synthetic.tree.clear')}
                                    </Button>
                                </span>
                            </div>
                        )}
                        {picked.mode === 'ids' && !sendable(picked) && (
                            <Notice tone="warn">{t('marketdata.synthetic.tree.tooMany')}</Notice>
                        )}
                        <div className="overflow-x-auto rounded-md border border-line">
                            <table className="w-full text-left text-sm">
                                <thead>
                                    <tr className="border-b border-line text-xs text-ink-muted">
                                        <th className="w-8 px-3 py-2">
                                            {runs.mayRun && (
                                                <input
                                                    type="checkbox"
                                                    aria-label={t(
                                                        'marketdata.synthetic.tree.selectPage',
                                                    )}
                                                    checked={allPicked(picked, ids)}
                                                    onChange={(event) =>
                                                        setPicked_(
                                                            setPicked(
                                                                picked,
                                                                ids,
                                                                event.target.checked,
                                                            ),
                                                        )
                                                    }
                                                />
                                            )}
                                        </th>
                                        {(
                                            [
                                                'state',
                                                'enabled',
                                                'name',
                                                'assetClass',
                                                'underlying',
                                                'engine',
                                                'frequency',
                                                'series',
                                            ] as const
                                        ).map((column) => (
                                            <th key={column} className="px-3 py-2 font-medium">
                                                {t(columnWords(column))}
                                            </th>
                                        ))}
                                    </tr>
                                </thead>
                                <tbody>
                                    {feeds.map((feed) => {
                                        const every = frequencyOf(feed.ticks_per_hour);
                                        return (
                                            <tr
                                                key={feed.id}
                                                className={cx(
                                                    'cursor-pointer border-b border-line-subtle last:border-b-0 hover:bg-surface-hover',
                                                    isPicked(picked, feed.id) && 'bg-accent/10',
                                                )}
                                                onClick={() => void navigate(feedPageOf(feed))}
                                            >
                                                <td
                                                    className="px-3 py-2"
                                                    onClick={(event) => event.stopPropagation()}
                                                >
                                                    {runs.mayRun && (
                                                        <input
                                                            type="checkbox"
                                                            aria-label={t(
                                                                'marketdata.synthetic.tree.selectFeed',
                                                                {
                                                                    feed: feed.label,
                                                                },
                                                            )}
                                                            checked={isPicked(picked, feed.id)}
                                                            onChange={(event) =>
                                                                setPicked_(
                                                                    setPicked(
                                                                        picked,
                                                                        [feed.id],
                                                                        event.target.checked,
                                                                    ),
                                                                )
                                                            }
                                                        />
                                                    )}
                                                </td>
                                                <td className="px-3 py-2">
                                                    <Tag tone={feed.running ? 'up' : 'muted'}>
                                                        {t(
                                                            feed.running
                                                                ? 'marketdata.synthetic.state.running'
                                                                : 'marketdata.synthetic.state.stopped',
                                                        )}
                                                    </Tag>
                                                </td>
                                                <td className="px-3 py-2">
                                                    <Tag tone={feed.enabled ? 'up' : 'down'}>
                                                        {t(
                                                            feed.enabled
                                                                ? 'marketdata.synthetic.tree.enabled'
                                                                : 'marketdata.synthetic.tree.disabled',
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
                                                    {showProvider && (
                                                        <span className="ml-2 text-xs font-normal text-ink-faint">
                                                            {providerName(
                                                                feed.provider_code,
                                                                facts,
                                                            )}
                                                        </span>
                                                    )}
                                                </td>
                                                <td className="px-3 py-2">
                                                    <Label
                                                        text={t(
                                                            `marketdata.synthetic.assetClass.${feed.asset_class_code}`,
                                                        )}
                                                        badge={labels.byCode.get(
                                                            `asset_class_${feed.asset_class_code}`,
                                                        )}
                                                    />
                                                </td>
                                                <td className="px-3 py-2">
                                                    <Underlying feed={feed} />
                                                </td>
                                                <td className="px-3 py-2">
                                                    {feed.kind === 'ir_curve'
                                                        ? engineName(t, feed.engine)
                                                        : t(
                                                              `refdata.choices.process_type.${feed.engine}`,
                                                          )}
                                                </td>
                                                <td className="px-3 py-2 tabular-nums">
                                                    {t(
                                                        `marketdata.synthetic.tree.every.${every.unit}`,
                                                        {
                                                            n: every.n,
                                                        },
                                                    )}
                                                </td>
                                                <td
                                                    className="max-w-xs px-3 py-2"
                                                    onClick={(event) => event.stopPropagation()}
                                                >
                                                    <OresmdUrlResolved
                                                        uri={feed.oresmd_uri}
                                                        to={targetOf(feed.oresmd_uri)}
                                                    />
                                                </td>
                                            </tr>
                                        );
                                    })}
                                    {feeds.length === 0 && page.isSuccess && (
                                        <tr>
                                            <td colSpan={9} className="px-3 py-4 text-ink-muted">
                                                {t('marketdata.synthetic.tree.none')}
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
                            showing={t('marketdata.synthetic.tree.showing', {
                                first: String(first),
                                last: String(last),
                                total: total.toLocaleString(),
                            })}
                            onMove={(next) =>
                                change({ offset: next === 0 ? undefined : String(next) })
                            }
                            onPageSize={(size) =>
                                change({
                                    size: size === DEFAULT_PAGE_SIZE ? undefined : String(size),
                                    offset: undefined,
                                })
                            }
                        />
                    </div>
                </div>
            )}
            {adding && <AddFeed collectionId={chosenCollection} onClose={() => setAdding(false)} />}
            {runs.dialog}
        </div>
    );
}
