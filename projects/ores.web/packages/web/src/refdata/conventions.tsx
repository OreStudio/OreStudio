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

import { useQuery, useQueryClient } from '@tanstack/react-query';
import type { KeyboardEvent, ReactNode } from 'react';
import { Link, useNavigate, useParams, useSearchParams } from 'react-router';
import { conventions, type ConventionFamilyCard, type ConventionRow } from '../api/conventions.js';
import type { WatchedEntity } from '../events/EntityEvents.js';
import { useEntityChanges } from '../events/useEntityChanges.js';
import { useLoadState } from '../events/useLoadState.js';
import { useTranslation } from '../i18n/Provider.js';
import { TERM_SPECS } from '../journeys/conventionsState.js';
import { conventionDocumentPath, conventionPath, PATHS } from '../shell/paths.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { WorkflowMenu } from '../shell/WorkflowMenu.js';
import { AccessMark } from '../ui/AccessMark.js';
import { Icon } from '../ui/Icon.js';
import { RefreshButton } from '../ui/RefreshButton.js';
import { useTabs } from '../ui/Tabs.js';
import { Button, Input, Notice, PageHeader, Select, Tag } from '../ui/Primitives.js';
import { RecordList, type ListColumn, type ListSource } from './RecordList.js';
import { Crumbs } from './shared.js';

/** The cache key of the families read, which the list of kinds shows. */
export const FAMILIES_KEY = ['conventions', 'families'] as const;

/** The events collection a family's changes are published on. */
function watchOf(card: ConventionFamilyCard): WatchedEntity {
    return { component: 'refdata', entity: `${card.entity}s` };
}

const TABS = ['party', 'shared'] as const;

type Tab = (typeof TABS)[number];

const SORT_COLUMNS = ['kind', 'documents'] as const;

type KindColumn = (typeof SORT_COLUMNS)[number];

/** How many terms a document list draws; the record page will show the rest. */
const LISTED_TERMS = 4;

/**
 * The asset classes a list of kinds is drawn under, in the order the screen
 * draws them. Which class a kind serves is data on the kind, and reaches the
 * screen on its card.
 */
const ASSET_CLASSES = ['interest-rates', 'fx', 'inflation', 'credit', 'commodity', 'bond'] as const;

/** The groups a list of kinds is drawn in, and the one that holds a kind whose class is not known. */
type Group = (typeof ASSET_CLASSES)[number] | 'other';

function groupOf(card: ConventionFamilyCard): Group {
    return ASSET_CLASSES.find((group) => group === card.assetClass) ?? 'other';
}

/**
 * The conventions, one row for each kind, under the asset class it serves.
 *
 * The party's kinds and the kinds every party reads stand in separate tabs, so
 * a person never mistakes a change to a shared kind for a change to their own.
 * The search, the asset class and the order live in the address.
 */
export function ConventionKindsPage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const queries = useQueryClient();
    const families = useQuery({ queryKey: FAMILIES_KEY, queryFn: conventions.families });
    const load = useLoadState([FAMILIES_KEY]);
    const news = useEntityChanges((families.data ?? []).map(watchOf), load);
    const [params, setParams] = useSearchParams();
    const cards = families.data ?? [];
    const areaRoot = useAreaRoot('refdata');
    const countIn = (tab: Tab): number =>
        cards.filter((card) => card.shared === (tab === 'shared')).length;
    const { tab, bar } = useTabs({
        label: t('refdata.conventions.tabs'),
        tabs: TABS,
        titleOf: (candidate) =>
            `${t(`refdata.conventions.tab.${candidate}`)} (${String(countIn(candidate as Tab))})`,
    });
    const search = params.get('q') ?? '';
    const assetClass = params.get('class') ?? '';
    const sortParam = params.get('sort');
    const sort: KindColumn = SORT_COLUMNS.find((column) => column === sortParam) ?? 'kind';
    const descending = params.get('dir') === 'desc';
    const nameOf = (card: ConventionFamilyCard): string =>
        t(`journey.conventions.families.${card.key}`);

    const change = (updates: Readonly<Record<string, string>>): void => {
        const next = new URLSearchParams(params);
        for (const [name, value] of Object.entries(updates)) {
            if (value === '') {
                next.delete(name);
            } else {
                next.set(name, value);
            }
        }
        setParams(next, { replace: true });
    };

    const inTab = cards.filter((card) => card.shared === (tab === 'shared'));
    const needle = search.trim().toLocaleLowerCase();
    const shown = inTab.filter(
        (card) =>
            (assetClass === '' || groupOf(card) === assetClass) &&
            (needle === '' ||
                nameOf(card).toLocaleLowerCase().includes(needle) ||
                card.key.includes(needle)),
    );
    const valueOf = (card: ConventionFamilyCard): string | number =>
        sort === 'documents' ? (card.count ?? -1) : nameOf(card).toLocaleLowerCase();
    const ordered = [...shown].sort((left, right) => {
        const a = valueOf(left);
        const b = valueOf(right);
        return (a < b ? -1 : a > b ? 1 : 0) * (descending ? -1 : 1);
    });
    const groups = [...ASSET_CLASSES, 'other' as const]
        .map((group) => ({ group, rows: ordered.filter((card) => groupOf(card) === group) }))
        .filter((entry) => entry.rows.length > 0);
    const present = [...ASSET_CLASSES, 'other' as const].filter((group) =>
        inTab.some((card) => groupOf(card) === group),
    );
    const total = inTab.reduce((sum, card) => sum + (card.count ?? 0), 0);

    const sortBy = (column: KindColumn): void =>
        change({
            sort: column === 'kind' && (sort !== 'kind' || descending) ? '' : column,
            dir: sort === column && !descending ? 'desc' : '',
        });
    const header = (column: KindColumn, text: string, numeric = false): ReactNode => (
        <th
            aria-sort={sort === column ? (descending ? 'descending' : 'ascending') : undefined}
            className={`px-4 py-2 font-medium ${numeric ? 'text-right' : ''}`}
        >
            <button type="button" className="hover:text-ink" onClick={() => sortBy(column)}>
                {text}
                {sort === column ? (descending ? ' ↓' : ' ↑') : ''}
            </button>
        </th>
    );
    const open = (card: ConventionFamilyCard): void => void navigate(conventionPath(card.key));
    const onKey = (event: KeyboardEvent, card: ConventionFamilyCard): void => {
        if (event.key === 'Enter') {
            open(card);
        }
    };

    return (
        <div className="space-y-4">
            <div>
                <Crumbs parts={[...areaRoot, { label: t('refdata.conventions.title') }]} />
                <PageHeader
                    title={t('refdata.conventions.title')}
                    description={t('refdata.conventions.lead')}
                    actions={
                        <>
                            <RefreshButton
                                onClick={() =>
                                    void queries.invalidateQueries({ queryKey: FAMILIES_KEY })
                                }
                                load={load}
                                news={news}
                            />
                            <WorkflowMenu on="conventions" />
                        </>
                    }
                />
            </div>
            {bar}
            {tab === 'shared' && (
                <Notice tone="warn">
                    <b>{t('refdata.conventions.sharedWarning')}</b>{' '}
                    {t('refdata.conventions.sharedWarningBody')}
                </Notice>
            )}
            <section className="overflow-hidden rounded-md border border-line">
                <div className="flex flex-wrap items-center gap-3 border-b border-line p-3">
                    <label className="relative min-w-60 flex-1">
                        <span className="pointer-events-none absolute inset-y-0 left-3 flex items-center text-ink-faint">
                            <Icon name="search" size={16} />
                        </span>
                        <Input
                            type="search"
                            className="pl-9"
                            value={search}
                            placeholder={t('refdata.conventions.search')}
                            aria-label={t('refdata.conventions.search')}
                            onChange={(event) => change({ q: event.target.value })}
                        />
                    </label>
                    <div className="w-56 shrink-0">
                        <Select
                            aria-label={t('refdata.conventions.assetClass')}
                            value={assetClass}
                            onChange={(event) => change({ class: event.target.value })}
                        >
                            <option value="">{t('refdata.conventions.allClasses')}</option>
                            {present.map((group) => (
                                <option key={group} value={group}>
                                    {t(`refdata.conventions.classes.${group}`)}
                                </option>
                            ))}
                        </Select>
                    </div>
                </div>
                {families.isError && (
                    <div className="p-3">
                        <Notice tone="error">
                            <span className="mr-3">
                                {t('journey.conventions.instrument.readFailed', {
                                    message: families.error.message,
                                })}
                            </span>
                            <Button size="sm" onClick={() => void families.refetch()}>
                                {t('refdata.records.retry')}
                            </Button>
                        </Notice>
                    </div>
                )}
                {families.data !== undefined && (
                    <p className="border-b border-line px-4 py-2 text-xs text-ink-faint">
                        {t('refdata.conventions.summary', {
                            documents: String(total),
                            kinds: String(inTab.length),
                        })}
                    </p>
                )}
                <div className="overflow-x-auto">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                {header('kind', t('refdata.conventions.columns.kind'))}
                                {header(
                                    'documents',
                                    t('refdata.conventions.columns.documents'),
                                    true,
                                )}
                                <th className="px-4 py-2 font-medium">
                                    {t('refdata.conventions.columns.terms')}
                                </th>
                            </tr>
                        </thead>
                        <tbody
                            className={
                                families.isFetching && families.data !== undefined
                                    ? 'opacity-60'
                                    : ''
                            }
                        >
                            {families.data === undefined && !families.isError && (
                                <tr>
                                    <td colSpan={3} className="px-4 py-3 text-ink-muted">
                                        {t('common.loading')}
                                    </td>
                                </tr>
                            )}
                            {families.data !== undefined && shown.length === 0 && (
                                <tr>
                                    <td colSpan={3} className="px-4 py-3 text-ink-muted">
                                        <span className="flex items-center gap-3">
                                            {t('refdata.conventions.noMatch')}
                                            <Button
                                                size="sm"
                                                variant="ghost"
                                                onClick={() => change({ q: '', class: '' })}
                                            >
                                                {t('refdata.records.clearSearch')}
                                            </Button>
                                        </span>
                                    </td>
                                </tr>
                            )}
                            {groups.map(({ group, rows }) => (
                                <GroupRows
                                    key={group}
                                    group={group}
                                    rows={rows}
                                    nameOf={nameOf}
                                    onOpen={open}
                                    onKey={onKey}
                                />
                            ))}
                        </tbody>
                    </table>
                </div>
            </section>
        </div>
    );
}

function GroupRows({
    group,
    rows,
    nameOf,
    onOpen,
    onKey,
}: {
    readonly group: Group;
    readonly rows: readonly ConventionFamilyCard[];
    readonly nameOf: (card: ConventionFamilyCard) => string;
    readonly onOpen: (card: ConventionFamilyCard) => void;
    readonly onKey: (event: KeyboardEvent, card: ConventionFamilyCard) => void;
}): ReactNode {
    const { t } = useTranslation();
    const documents = rows.reduce((sum, card) => sum + (card.count ?? 0), 0);
    return (
        <>
            <tr className="border-b border-line">
                <th
                    colSpan={3}
                    scope="rowgroup"
                    className="px-4 pb-1.5 pt-4 text-left text-sm font-semibold"
                >
                    {t(`refdata.conventions.classes.${group}`)}
                    <span className="ml-2 text-xs font-normal text-ink-faint">
                        {t('refdata.conventions.groupSummary', {
                            kinds: String(rows.length),
                            documents: String(documents),
                        })}
                    </span>
                </th>
            </tr>
            {rows.map((card) => (
                <tr
                    key={card.key}
                    tabIndex={0}
                    className="cursor-pointer border-b border-line-subtle hover:bg-surface-hover focus:bg-surface-hover focus:outline-none"
                    onClick={() => onOpen(card)}
                    onKeyDown={(event) => onKey(event, card)}
                >
                    <td className="px-4 py-2">
                        <Link
                            to={conventionPath(card.key)}
                            className="hover:text-accent"
                            onClick={(event) => event.stopPropagation()}
                        >
                            {nameOf(card)}
                        </Link>
                    </td>
                    <td className="px-4 py-2 text-right tabular-nums">
                        {card.count === null ? (
                            <span
                                className="text-ink-faint"
                                title={t('journey.conventions.instrument.countUnknown')}
                            >
                                —
                            </span>
                        ) : (
                            String(card.count)
                        )}
                    </td>
                    <td className="px-4 py-2">
                        <Tag tone={card.writable ? 'accent' : 'muted'}>
                            {card.writable
                                ? t('journey.conventions.instrument.drawn')
                                : t('journey.conventions.instrument.notDrawn')}
                        </Tag>
                    </td>
                </tr>
            ))}
        </>
    );
}

/** The columns of a kind's documents: the key, and the terms the screen draws for the kind. */
function documentColumns(
    family: string,
    t: (key: string) => string,
): readonly ListColumn<ConventionRow>[] {
    const terms = (TERM_SPECS[family] ?? []).slice(0, LISTED_TERMS);
    return [
        {
            id: 'id',
            header: t('journey.conventions.convention.columns.id'),
            cell: (row) => row.id,
            mono: true,
        },
        ...terms.map((spec): ListColumn<ConventionRow> => ({
            id: spec.column,
            header: t(`journey.conventions.terms.columns.${spec.column}`),
            cell: (row) => {
                const value = row[spec.column];
                return value === null || value === undefined || value === '' ? (
                    <span className="text-ink-faint">—</span>
                ) : (
                    String(value)
                );
            },
        })),
    ];
}

/**
 * The documents of one kind, a page at a time from the server.
 *
 * The server filters a convention list by id only and sends it in no order, so
 * the list offers no search and no sort until it does. The page is the server's.
 */
export function ConventionDocumentsPage(): ReactNode {
    const { family = '' } = useParams();
    const { t } = useTranslation();
    const families = useQuery({ queryKey: FAMILIES_KEY, queryFn: conventions.families });
    const card = families.data?.find((candidate) => candidate.key === family);
    const crumbs = [
        ...useAreaRoot('refdata'),
        { label: t('refdata.conventions.title'), to: PATHS.conventions },
        { label: t(`journey.conventions.families.${family}`) },
    ];
    if (families.data !== undefined && card === undefined) {
        return (
            <div className="space-y-4">
                <Crumbs parts={crumbs} />
                <Notice tone="warn">
                    {t('refdata.conventions.unknownKind', { kind: family })}{' '}
                    <Link to={PATHS.conventions} className="text-accent hover:text-accent-bright">
                        {t('refdata.conventions.backToKinds')}
                    </Link>
                </Notice>
            </div>
        );
    }
    if (card === undefined) {
        return (
            <div className="space-y-4">
                <Crumbs parts={crumbs} />
                {families.isError ? (
                    <Notice tone="error">
                        <span className="mr-3">
                            {t('journey.conventions.instrument.readFailed', {
                                message: families.error.message,
                            })}
                        </span>
                        <Button size="sm" onClick={() => void families.refetch()}>
                            {t('refdata.records.retry')}
                        </Button>
                    </Notice>
                ) : (
                    <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                )}
            </div>
        );
    }
    const source: ListSource<ConventionRow> = {
        key: `conventions:${family}`,
        read: async (page) => {
            const read = await conventions.page(family, {
                offset: page.offset,
                limit: page.limit,
            });
            return { rows: read.rows, total: read.total };
        },
        search: false,
        sortable: [],
        mayAdd: false,
        watches: [watchOf(card)],
        audit: (row, field) => row[field],
    };
    return (
        <RecordList
            source={source}
            title={t(`journey.conventions.families.${family}`)}
            lead={t('refdata.conventions.documentsLead', {
                class: t(`refdata.conventions.classes.${groupOf(card)}`),
            })}
            crumbs={crumbs}
            keyOf={(row) => row.id}
            pathOf={(row) => conventionDocumentPath(family, row.id)}
            notes={
                card.shared
                    ? [
                          {
                              tone: 'warn',
                              text: `${t('refdata.conventions.sharedDocuments')} ${t('refdata.conventions.sharedWarningBody')}`,
                          },
                      ]
                    : []
            }
            actions={
                <>
                    <AccessMark canWrite={false} />
                    <WorkflowMenu on="conventions" />
                </>
            }
            columns={documentColumns(family, t)}
        />
    );
}
