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

import { useEffect, useMemo, useRef, useState, type ReactNode } from 'react';
import type { UseQueryResult } from '@tanstack/react-query';
import { DataTable, type RowAction } from './DataTable.js';
import { MaskIcon } from '../ui/icons/MaskIcon.js';
import { useEntityChanges } from '../events/ChangeEvents.js';
import { Button, Notice, cx } from '../ui/Primitives.js';
import { useTranslation } from '../i18n/Provider.js';
import type { EntityMeta } from '../ui-contract.js';

/**
 * The list screen for every entity.
 *
 * Generated from the entity's declaration plus the collection name, so an entity
 * adds no screen code. Search, filtering and paging are here once, which is the
 * point: a hundred entities with a hundred list screens is a hundred places for
 * the search box to behave differently.
 */
export interface EntityListPageProps<Row> {
  readonly meta: EntityMeta;
  /**
   * The key's members, which identify a row in the list.
   *
   * The metadata carries the natural key, which a junction does not have: its
   * identity is the pair it links, so a row is named by both members. The
   * declaration is where that list lives, and the container passes it down.
   */
  readonly keyFields: readonly string[];
  readonly title: string;
  readonly description?: string;
  readonly rows: readonly Row[];
  readonly totalCount: number;
  readonly query: UseQueryResult<{ rows: readonly Row[]; totalCount: number }>;
  readonly page: number;
  readonly pageSize: number;
  readonly onPageChange: (page: number) => void;
  readonly onPageSizeChange: (size: number) => void;
  readonly onReload: () => void;
  readonly onOpen?: (row: Row) => void;
  /** Creating a new record. Omitted where the entity has none. */
  readonly onCreate?: () => void;
  /** Reached from the row menu. Empty when the entity offers none. */
  readonly onEdit?: (row: Row) => void;
  readonly onHistory?: (row: Row) => void;
  readonly onDelete?: (row: Row) => void;
  /** Fields the search box matches. */
  readonly searchFields?: readonly string[];
  /**
   * Catalogue key for what the search box searches, so it names the entity's
   * own fields rather than saying "Search" and leaving nobody to guess.
   */
  readonly searchPlaceholderKey?: string;
  /**
   * A message from a failed action, shown above the table.
   *
   * Actions are wired by the entity, so an entity that deletes needs somewhere
   * to report a refusal. The screen owns the reporting because it owns the
   * layout, and a modal for a refused delete is heavier than the news deserves.
   */
  readonly failureMessage?: string;
  /**
   * The component and entity this list is of, so it can be told when the
   * collection has changed underneath it.
   */
  readonly watchedAs?: { readonly component: string; readonly entity: string };
  /** What this collection is called, plural, for copy the screen writes itself. */
  readonly collectionName: string;
  /** The field the type filter groups on, and the values present. */
  readonly filterField?: string;
  /**
   * The instant the page is read as of, when the entity's list read takes one.
   *
   * Absent for an entity whose request carries no window, so the control is
   * not rendered rather than rendered and refused.
   */
  readonly asOf?: string;
  /** Present exactly when `asOf` is: an absent pair renders no control. */
  readonly onAsOfChange?: (asOf: string) => void;
}

const PAGE_SIZES = [25, 50, 100, 200, 500] as const;
const LOAD_ALL_CEILING = 1000;

/**
 * A row's identity in the list: the values of every key member, joined.
 *
 * Joined with a separator no generated value holds, so two different rows
 * cannot fold to one key and collide in the table.
 */
function recordKeyOf(keyFields: readonly string[], row: Record<string, unknown>): string {
  return keyFields.map((field) => String(row[field] ?? '')).join('\u0000');
}

export function EntityListPage<Row extends Record<string, unknown>>({
  meta,
  keyFields,
  title,
  description,
  rows,
  totalCount,
  query,
  page,
  pageSize,
  onPageChange,
  onPageSizeChange,
  onReload,
  onOpen,
  onCreate,
  onEdit,
  onHistory,
  onDelete,
  searchFields = [],
  searchPlaceholderKey = 'entity.search',
  failureMessage,
  watchedAs,
  collectionName,
  filterField,
  asOf,
  onAsOfChange,
}: EntityListPageProps<Row>): ReactNode {
  const { t, plural } = useTranslation();
  const [search, setSearch] = useState('');
  const [filter, setFilter] = useState('');

  // Filtering runs in the browser over the loaded page, because the server's
  // list call takes paging and not a predicate. When the server grows one, the
  // same controls drive the query and this layout does not change.
  const visible = useMemo(() => {
    const needle = search.trim().toLowerCase();
    return rows.filter((row) => {
      if (filter.length > 0 && String(row[filterField ?? '']) !== filter) return false;
      if (needle.length === 0) return true;
      return searchFields.some((field) => String(row[field] ?? '').toLowerCase().includes(needle));
    });
  }, [rows, search, filter, filterField, searchFields]);

  const filterValues = useMemo(() => {
    if (filterField === undefined) return [];
    return [...new Set(rows.map((row) => String(row[filterField] ?? '')))].filter((v) => v.length > 0).sort();
  }, [rows, filterField]);

  // The actions a row offers, in the order a person looks for them. History
  // before Delete, because the destructive one should not be the first thing
  // under the cursor.
  const rowActions: readonly RowAction<Row>[] = [
    ...(onEdit === undefined
      ? []
      : [{ id: 'edit', label: t('entity.edit'), icon: 'edit' as const, onSelect: onEdit }]),
    ...(onHistory === undefined
      ? []
      : [{ id: 'history', label: t('entity.history'), icon: 'history' as const, onSelect: onHistory }]),
    ...(onDelete === undefined
      ? []
      : [
          {
            id: 'delete',
            label: t('entity.delete'),
            icon: 'delete' as const,
            danger: true,
            onSelect: onDelete,
          },
        ]),
  ];

  /*
   * Search has to reach the whole collection, not the page on screen.
   *
   * The service's list takes paging and no predicate, so filtering happens here,
   * over whatever has been loaded. That is fine while the whole collection fits
   * in one page and badly wrong as soon as it does not: searching for a record on
   * page four returns "no results" while the record exists, which is worse than
   * having no search at all.
   *
   * So a search asks for everything, once, when the collection is small enough to
   * hold. Above the ceiling the count says what was searched, because a person
   * can act on a stated limit but not on a silent one.
   */
  const searching = search.trim().length > 0;
  const wholeCollectionLoaded = rows.length >= totalCount;
  useEffect(() => {
    if (!searching || wholeCollectionLoaded) return;
    if (totalCount > LOAD_ALL_CEILING || totalCount === 0) return;
    onPageSizeChange(totalCount);
  }, [searching, wholeCollectionLoaded, totalCount, onPageSizeChange]);

  /*
   * Stale, and it says so on the reload action.
   *
   * Never a reload by itself: a list that jumps while somebody is reading it is
   * worse than a list that is briefly out of date, and a person is the one who
   * knows whether they are finished with what is on screen.
   */
  const changes = useEntityChanges(
    watchedAs?.component ?? '',
    watchedAs?.entity ?? '',
    query.dataUpdatedAt,
  );
  const stale = watchedAs !== undefined && changes.stale;
  /*
   * The rows that moved, by comparison with the last load.
   *
   * Not against the time the change was announced, which does not work: a row is
   * stamped when it is written and the announcement follows it, so the row is
   * always a little older than the news and would never mark. What the mark
   * means is "newer than what was here last time", and that is what it compares.
   *
   * Both sides are the service's clock, because both come from the rows
   * themselves. This also means a reload a person asks for shows what changed
   * since they last looked, whether or not anything was announced.
   *
   * Derived rather than remembered, so the marks appear when the reload brings
   * the new rows in rather than when the news arrives and the old rows are still
   * on screen.
   */
  const previousNewest = useRef<string | undefined>(undefined);
  const marked = useMemo(() => {
    const since = previousNewest.current;
    if (since === undefined || since.length === 0) return new Set<string>();
    const fromData = new Set(
      rows
        .filter((row) => String(row['recorded_at'] ?? '') > since)
        .map((row) => recordKeyOf(keyFields, row)),
    );
    return fromData;
  }, [rows, keyFields]);

  /*
   * The badge fades on its own, as a mail client's does.
   *
   * It is there to catch the eye of somebody who was looking when the change
   * arrived. Somebody who was not, and comes back later, wants the list as it
   * stands rather than a week of badges — and the reload control is still saying
   * there is something to bring in, which is the part that persists.
   */
  const badgeKey = Array.from(marked).sort().join(',');
  const [badgesVisible, setBadgesVisible] = useState(true);
  useEffect(() => {
    setBadgesVisible(true);
    if (badgeKey.length === 0) return undefined;
    const timer = setTimeout(() => setBadgesVisible(false), 6_000);
    return () => clearTimeout(timer);
  }, [badgeKey]);

  // Advanced after the marks have been taken, so the comparison is always
  // against the load before this one.
  useEffect(() => {
    if (query.isFetching || rows.length === 0) return;
    const newest = rows.reduce((newest, row) => {
      const value = String(row['recorded_at'] ?? '');
      return value > newest ? value : newest;
    }, '');
    if (newest.length > 0) previousNewest.current = newest;
  }, [rows, query.isFetching]);

  const pages = Math.max(1, Math.ceil(totalCount / pageSize));
  const loading = query.isPending;
  const isFiltered = search.trim().length > 0 || filter.length > 0;

  return (
    <div className="mx-auto max-w-[1100px] px-5 py-7">
      <header className="mb-5 flex items-start gap-4">
        <div className="min-w-0">
          <h1 className="text-xl font-semibold tracking-tight">{title}</h1>
          {description !== undefined && <p className="mt-1 text-sm text-ink-muted">{description}</p>}
        </div>
        <div className="ml-auto flex shrink-0 items-center gap-2">
          <Button
            variant="secondary"
            size="sm"
            onClick={() => {
              // Reloading is what the news was about, so the news is answered.
              changes.clear();
              onReload();
            }}
            pending={query.isFetching && !loading}
            pendingLabel={t('accounts.refreshing')}
            title={stale ? t('entity.changed') : undefined}
            /*
             * The control itself says that something changed, which is where a
             * person already looks when they want newer data. A bar elsewhere on
             * the screen is an alert, and news is not an alarm.
             */
            className={stale ? 'stale-pulse border-accent text-ink' : undefined}
          >
            <MaskIcon name="arrowSync" className={cx('size-3.5', stale ? 'text-accent' : 'opacity-70')} />
            {t('entity.refresh')}

          </Button>
          {/*
            Not rendered rather than disabled: a disabled button invites a
            question a missing one does not. An entity that can be created gets a
            button that works; one that cannot gets no button at all.
          */}
          {onCreate !== undefined && (
            <Button variant="primary" size="sm" onClick={onCreate}>
              <MaskIcon name="add" className="size-3.5" />
              {t('entity.add')}
            </Button>
          )}
        </div>
      </header>

      {failureMessage !== undefined && (
        <div className="mb-4">
          <Notice tone="error">{failureMessage}</Notice>
        </div>
      )}

      {query.isError && (
        <div className="mb-4">
          <Notice tone="error">
            {t('entity.loadFailed', { collection: collectionName })}{' '}
            <button type="button" onClick={onReload} className="underline hover:text-ink">
              {t('feedback.retry')}
            </button>
          </Notice>
        </div>
      )}

      <div className="mb-3 flex flex-wrap items-center gap-2">
        {searchFields.length > 0 && (
          <div className="relative min-w-56 flex-1">
            <MaskIcon name="search" className="pointer-events-none absolute left-2.5 top-1/2 size-3.5 -translate-y-1/2 opacity-50" />
            <input
              type="search"
              value={search}
              onChange={(event) => setSearch(event.target.value)}
              placeholder={t(searchPlaceholderKey)}
              aria-label={t('accounts.search')}
              className="h-9 w-full rounded-md border border-line bg-bg-secondary pl-8 pr-3 text-sm text-ink placeholder:text-ink-faint focus:border-line-strong focus:outline-none"
            />
          </div>
        )}

        {filterValues.length > 0 && (
          <select
            value={filter}
            onChange={(event) => setFilter(event.target.value)}
            aria-label={t('accounts.filterByType')}
            className="h-9 rounded-md border border-line bg-bg-secondary px-2.5 text-sm text-ink focus:border-line-strong focus:outline-none"
          >
            <option value="">{t('accounts.allTypes')}</option>
            {filterValues.map((value) => (
              <option key={value} value={value}>
                {value}
              </option>
            ))}
          </select>
        )}

        {asOf !== undefined && onAsOfChange !== undefined && (
          /*
           * The window the page is read as of. An absent date means the
           * present, which is the request's own absent value, so clearing the
           * control is what returns to now.
           */
          <label className="flex h-9 items-center gap-2 rounded-md border border-line bg-bg-secondary px-2.5 text-sm text-ink focus-within:border-line-strong">
            <MaskIcon name="clock" className="size-3.5 opacity-60" />
            <span className="text-xs text-ink-muted">{t('entity.asOf')}</span>
            <input
              type="date"
              value={asOf}
              onChange={(event) => onAsOfChange(event.target.value)}
              aria-label={t('entity.asOf')}
              className="bg-transparent text-sm text-ink focus:outline-none"
            />
            {asOf.length > 0 && (
              <button
                type="button"
                onClick={() => onAsOfChange('')}
                className="text-xs underline hover:text-ink"
              >
                {t('entity.asOfNow')}
              </button>
            )}
          </label>
        )}

        <span className="ml-auto text-xs tabular-nums text-ink-faint">
          {searching && !wholeCollectionLoaded && totalCount > LOAD_ALL_CEILING
            ? t('table.searchedSoFar', { shown: rows.length, total: totalCount })
            : isFiltered
              ? t('accounts.count', { shown: visible.length, total: rows.length })
              : plural('home.entities', totalCount)}
        </span>
      </div>

      {/* Loading keeps the rows and dims them rather than blanking the table,
          because a table that empties on every refresh is one nobody can read. */}
      <div className={cx(query.isFetching && !loading && 'opacity-60 transition-opacity')}>
        <DataTable
          columns={meta.columns}
          rows={visible}
          rowKey={(row) => recordKeyOf(keyFields, row)}
          {...(onOpen === undefined ? {} : { onOpen })}
          {...(rowActions.length === 0 ? {} : { rowActions })}
          changed={badgesVisible ? marked : new Set()}
          loading={loading}
          emptyMessage={
            isFiltered ? t('entity.emptyFiltered', { collection: collectionName }) : t('entity.noRecords')
          }
        />
      </div>

      <div className="mt-4 flex flex-wrap items-center gap-3 text-xs text-ink-muted">
        <span className="tabular-nums">
          {isFiltered
            ? t('accounts.count', { shown: visible.length, total: rows.length })
            : totalCount === 0
              ? t('entity.noRecords')
              : t('entity.page', { page, pages })}
        </span>

        <div className="ml-auto flex items-center gap-1">
          <PagerButton disabled={page <= 1} onClick={() => onPageChange(1)} label={t('entity.first')} />
          <PagerButton disabled={page <= 1} onClick={() => onPageChange(page - 1)} label={t('entity.previous')} />
          <PagerButton disabled={page >= pages} onClick={() => onPageChange(page + 1)} label={t('entity.next')} />
          <PagerButton disabled={page >= pages} onClick={() => onPageChange(pages)} label={t('entity.last')} />
        </div>

        <label className="flex items-center gap-2">
          {t('entity.pageSize')}
          <select
            value={pageSize}
            onChange={(event) => onPageSizeChange(Number(event.target.value))}
            className="h-8 rounded-md border border-line bg-bg-secondary px-2 text-xs text-ink focus:outline-none"
          >
            {PAGE_SIZES.map((size) => (
              <option key={size} value={size}>
                {size}
              </option>
            ))}
          </select>
        </label>

        {/* Offered only when it is honest: loading a hundred thousand records
            into a browser is a trap, not a feature. */}
        {totalCount > 0 && totalCount <= LOAD_ALL_CEILING && pageSize < totalCount && (
          <button
            type="button"
            onClick={() => onPageSizeChange(totalCount)}
            className="rounded-md border border-line px-2 py-1 text-xs text-ink-muted hover:text-ink"
          >
            {t('entity.loadAll')}
          </button>
        )}
      </div>
    </div>
  );
}

function PagerButton({
  disabled,
  onClick,
  label,
}: {
  readonly disabled: boolean;
  readonly onClick: () => void;
  readonly label: string;
}): ReactNode {
  return (
    <button
      type="button"
      disabled={disabled}
      onClick={onClick}
      className="rounded-md border border-line px-2 py-1 text-xs text-ink-muted transition-colors hover:text-ink disabled:opacity-40 disabled:hover:text-ink-muted"
    >
      {label}
    </button>
  );
}
