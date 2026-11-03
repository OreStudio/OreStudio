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

import { useMemo, type ReactNode } from 'react';
import { Link, useSearchParams } from 'react-router';
import { useQuery } from '@tanstack/react-query';
import {
    api,
    type AggregateRow,
    type Money,
    type ResultLevel,
    type ResultReason,
    type RunResultRow,
} from '../api/client.js';
import type { ReportRunErrorView } from '@ores/wire-protocol/browser';
import { ReadFailure } from '../operations/GridJobs.js';
import { localSource, RecordTable, type ListColumn } from '../refdata/RecordList.js';
import { Tag } from '../ui/Primitives.js';
import { useRunErrors } from './RunErrorsTable.js';
import { useTranslation } from '../i18n/Provider.js';
import { Donut, bookSharesOf } from './Donut.js';
import { bookPath, counterpartyPath, tradePath } from '../shell/paths.js';
import { asFigure, toAggregateRows, toTradeRows } from './resultsModel.js';

/**
 * What a run produced, read at the level the reader asks for.
 *
 * One select in the header chooses the level, and the address carries it, so a
 * reader can send the book view to a colleague and a refresh keeps it. The rows
 * are always the server's: it joins the trades to their books and names their
 * counterparties, and rolls the figures up before they arrive.
 */

const LEVELS: readonly ResultLevel[] = ['trade', 'book', 'counterparty'];

const REASON_TEXT: Readonly<Record<Exclude<ResultReason, ''>, string>> = {
    'no-run': 'reporting.results.noRun',
    'no-rows': 'reporting.results.noRows',
    'no-figures': 'reporting.results.noFigures',
};

export function RunResultsPanel({
    runId,
    definitionId,
    pricedAt,
}: {
    readonly runId: string;
    readonly definitionId: string;
    readonly pricedAt: string;
}): ReactNode {
    const { t } = useTranslation();
    const field = 'reporting.results';
    const [search, setSearch] = useSearchParams();
    const level = LEVELS.find((candidate) => candidate === search.get('by')) ?? 'trade';
    const bookFilter = search.get('book') ?? '';
    const counterpartyFilter = search.get('counterparty') ?? '';
    const noBook = search.get('nobook') === '1';
    const baseline = search.get('baseline') ?? '';

    const results = useQuery({
        queryKey: ['reporting-run-results', runId, level, baseline],
        queryFn: () => api.reportRunResults(runId, level, baseline),
        meta: { quiet: true },
    });
    const otherRuns = useQuery({
        queryKey: ['report-runs', definitionId],
        queryFn: () => api.reportRuns(definitionId),
        meta: { quiet: true },
    });
    const errors = useRunErrors(runId);
    const errorsOf = useMemo(() => {
        const byTrade = new Map<string, ReportRunErrorView[]>();
        for (const error of errors.data ?? []) {
            const trade = error.subject_id ?? '';
            if (error.scope !== 'Trade' || trade === '') {
                continue;
            }
            byTrade.set(trade, [...(byTrade.get(trade) ?? []), error]);
        }
        return byTrade;
    }, [errors.data]);
    const selected = search.get('trade') ?? '';

    const candidates = (otherRuns.data ?? [])
        .filter((entry) => entry.state_name === 'completed' && entry.id !== runId)
        .map((entry) => ({ id: entry.id, name: entry.name }));
    const chooseBaseline = (next: string): void => {
        const params = new URLSearchParams(search);
        if (next === '') {
            params.delete('baseline');
        } else {
            params.set('baseline', next);
        }
        setSearch(params, { replace: true });
    };
    const chooseLevel = (next: ResultLevel): void => {
        const params = new URLSearchParams(search);
        params.set('by', next);
        params.delete('book');
        params.delete('counterparty');
        setSearch(params, { replace: true });
    };
    /** The address with some parameters set, and the ones named null removed. */
    const addressWith = (changes: Readonly<Record<string, string | null>>): string => {
        const params = new URLSearchParams(search);
        for (const [name, value] of Object.entries(changes)) {
            if (value === null) {
                params.delete(name);
            } else {
                params.set(name, value);
            }
        }
        return `?${params.toString()}`;
    };
    const tradesOf = (key: string): string =>
        addressWith({
            by: 'trade',
            trade: null,
            book: level === 'book' && key !== '' ? key : null,
            nobook: level === 'book' && key === '' ? '1' : null,
            counterparty: level === 'counterparty' ? key : null,
        });
    const clearFilter = (): void => {
        const params = new URLSearchParams(search);
        params.delete('book');
        params.delete('counterparty');
        setSearch(params, { replace: true });
    };

    const rows = results.data?.rows ?? [];
    const tradeParse = level === 'trade' ? toTradeRows(rows) : { rows: [], unread: 0 };
    const aggregateParse = level === 'trade' ? { rows: [], unread: 0 } : toAggregateRows(rows);
    const trades = tradeParse.rows;
    const aggregates = aggregateParse.rows;
    const unread = tradeParse.unread + aggregateParse.unread;
    const filtered =
        level !== 'trade'
            ? []
            : trades.filter((row) => {
                  if (noBook) {
                      return row.book_id === '';
                  }
                  if (bookFilter !== '') {
                      return row.book_id === bookFilter;
                  }
                  return counterpartyFilter === '' || row.counterparty === counterpartyFilter;
              });
    const totals = results.data?.totals ?? [];
    const reason = results.data?.reason ?? '';
    const filtering = bookFilter !== '' || counterpartyFilter !== '' || noBook;
    const tradeCount = results.data?.tradeCount ?? 0;
    const bookShares = bookSharesOf(
        aggregates.map((row) => ({
            key: row.key,
            name: row.name,
            is_group: row.is_group,
            is_removed: row.is_removed,
            totals: row.totals,
        })),
    );
    const filterName = noBook
        ? t(`${field}.unbooked`)
        : bookFilter !== ''
          ? (trades.find((row) => row.book_id === bookFilter)?.book_name ?? '')
          : (trades.find((row) => row.counterparty === counterpartyFilter)?.counterparty_name ??
            '');
    const degraded = results.data?.recordsComplete === false;

    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t(`${field}.oneRowPer.${level}`)}</h2>
                <span className="rounded border border-line px-2 py-0.5 text-xs text-ink-muted">
                    {t(`${field}.${tradeCount === 1 ? 'runTagOne' : 'runTagMany'}`, {
                        count: tradeCount,
                        date: pricedAt,
                    })}
                </span>
            </header>
            {level !== 'trade' && (
                <label className="flex items-center gap-2 text-sm">
                    <span className="text-ink-faint">{t(`${field}.compareTo`)}</span>
                    <select
                        aria-label={t(`${field}.compareTo`)}
                        value={baseline}
                        onChange={(event) => chooseBaseline(event.target.value)}
                        className="rounded border border-line bg-surface px-2 py-1"
                    >
                        <option value="">{t(`${field}.noComparison`)}</option>
                        {candidates.map((candidate) => (
                            <option key={candidate.id} value={candidate.id}>
                                {candidate.name === '' ? candidate.id.slice(0, 8) : candidate.name}
                            </option>
                        ))}
                    </select>
                </label>
            )}
            <label className="flex items-center gap-2 text-sm">
                <span className="text-ink-faint">{t(`${field}.aggregateBy`)}</span>
                <select
                    aria-label={t(`${field}.aggregateBy`)}
                    value={level}
                    onChange={(event) => chooseLevel(event.target.value as ResultLevel)}
                    className="rounded border border-line bg-surface px-2 py-1"
                >
                    {LEVELS.map((candidate) => (
                        <option key={candidate} value={candidate}>
                            {t(`${field}.level.${candidate}`)}
                        </option>
                    ))}
                </select>
            </label>
            {results.isPending && <p className="text-sm text-ink-muted">{t('common.loading')}</p>}
            {results.isError && (
                <ReadFailure error={results.error} onRetry={() => void results.refetch()} />
            )}
            {results.isSuccess && (
                <>
                    {trades.length === 0 && aggregates.length === 0 ? (
                        <p className="text-sm text-ink-muted">
                            {reason === ''
                                ? t(`${field}.noRun`)
                                : t(REASON_TEXT[reason] ?? `${field}.noRun`)}
                        </p>
                    ) : (
                        <>
                            <p className="rounded-md border border-line-subtle bg-surface-raised px-3 py-2 text-sm text-ink-muted">
                                {level === 'trade'
                                    ? t(`${field}.sourceNote`)
                                    : t(`${field}.aggregateNote`, {
                                          level: t(`${field}.level.${level}`),
                                          date: pricedAt,
                                      })}
                            </p>
                            {degraded && (
                                <p className="text-sm text-down">{t(`${field}.degraded`)}</p>
                            )}
                            {unread > 0 && (
                                <p className="text-sm text-down">
                                    {t(`${field}.unreadRows`, { count: unread })}
                                </p>
                            )}
                            <dl className="flex flex-wrap gap-8">
                                <div>
                                    <dt className="text-xs text-ink-faint">
                                        {t(`${field}.trades`)}
                                    </dt>
                                    <dd className="font-mono text-sm">{tradeCount}</dd>
                                </div>
                                {totals.map((total) => (
                                    <div key={total.currency}>
                                        <dt className="text-xs text-ink-faint">
                                            {total.currency === ''
                                                ? t(`${field}.total`)
                                                : t(`${field}.totalIn`, {
                                                      currency: total.currency,
                                                  })}
                                        </dt>
                                        <dd className="font-mono text-sm">
                                            {asFigure(total.total)}
                                        </dd>
                                    </div>
                                ))}
                                {results.data?.baseline?.change.map((money) => {
                                    const was = results.data?.baseline?.totals.find(
                                        (entry) => entry.currency === money.currency,
                                    );
                                    const share = results.data?.baseline?.change_pct.find(
                                        (entry) => entry.currency === money.currency,
                                    );
                                    return (
                                        <div key={`baseline-${money.currency}`}>
                                            <dt className="text-xs text-ink-faint">
                                                {t(`${field}.was`)}
                                            </dt>
                                            <dd className="font-mono text-sm">
                                                {was === undefined ? '—' : asFigure(was.total)}
                                            </dd>
                                            <dd
                                                className={`font-mono text-xs ${
                                                    money.total.startsWith('-')
                                                        ? 'text-down'
                                                        : 'text-ink-muted'
                                                }`}
                                            >
                                                {asFigure(money.total)}
                                                {' · '}
                                                {share === undefined || share.total === ''
                                                    ? '—'
                                                    : `${asFigure(share.total)}%`}
                                            </dd>
                                        </div>
                                    );
                                })}
                            </dl>
                            {filtering && level === 'trade' && (
                                <p className="flex items-center gap-2 text-sm">
                                    <span className="rounded border border-line px-2 py-0.5 text-xs">
                                        {t(`${field}.filteredTo`, {
                                            name:
                                                filterName === ''
                                                    ? t(`${field}.unassigned`)
                                                    : filterName,
                                            count: filtered.length,
                                        })}
                                    </span>
                                    <button
                                        type="button"
                                        onClick={clearFilter}
                                        className="text-accent hover:text-accent-bright"
                                    >
                                        {t(`${field}.clearFilter`)}
                                    </button>
                                </p>
                            )}
                            {level === 'book' && bookShares !== null && (
                                <Donut
                                    title={t(`${field}.shareTitle`)}
                                    caption={t(`${field}.shareCaption`)}
                                    parts={bookShares}
                                    format={(value) => asFigure(value.toFixed(2))}
                                />
                            )}
                            {level === 'trade' ? (
                                <TradeTable
                                    runId={runId}
                                    rows={filtered}
                                    errorsOf={errorsOf}
                                    openOf={(id) =>
                                        addressWith({ trade: selected === id ? null : id })
                                    }
                                />
                            ) : (
                                <AggregateTable
                                    runId={runId}
                                    level={level}
                                    rows={aggregates}
                                    tradesOf={tradesOf}
                                    compared={baseline !== ''}
                                />
                            )}
                            {level === 'trade' ? (
                                <p className="text-sm text-ink-muted">{t(`${field}.selectHint`)}</p>
                            ) : (
                                <p className="text-sm text-ink-muted">
                                    {t(`${field}.aggregateHint`)}
                                </p>
                            )}
                            {level === 'trade' && selected !== '' && (
                                <TradeResult
                                    row={filtered.find((row) => row.trade_id === selected)}
                                    errors={errorsOf.get(selected) ?? []}
                                />
                            )}
                        </>
                    )}
                </>
            )}
        </section>
    );
}

/** Amounts in several currencies, one after another. */
function Amounts({
    amounts,
    signed = false,
}: {
    readonly amounts: readonly Money[];
    readonly signed?: boolean;
}): ReactNode {
    return amounts.map((money) => (
        <span
            key={money.currency}
            className={`mr-2 ${signed && money.total.startsWith('-') ? 'text-down' : ''}`}
        >
            {asFigure(money.total)}
            {money.currency === '' ? '' : ` ${money.currency}`}
        </span>
    ));
}

/** The trades of a run, one row each; a row opens the trade's figures and errors below. */
function TradeTable({
    runId,
    rows,
    errorsOf,
    openOf,
}: {
    readonly runId: string;
    readonly rows: readonly RunResultRow[];
    readonly errorsOf: ReadonlyMap<string, readonly ReportRunErrorView[]>;
    readonly openOf: (tradeId: string) => string;
}): ReactNode {
    const { t } = useTranslation();
    const field = 'reporting.results';
    return (
        <RecordTable<RunResultRow>
            source={localSource<RunResultRow>({
                key: `run-trades-${runId}`,
                rows,
                revision: String(rows.length),
                searchOf: (row) => `${row.trade_id} ${row.book_name} ${row.counterparty_name}`,
            })}
            plural={t(`${field}.trades`)}
            keyOf={(row) => row.trade_id}
            pathOf={(row) => openOf(row.trade_id)}
            columns={[
                {
                    id: 'trade',
                    header: t(`${field}.tradeId`),
                    mono: true,
                    cell: (row) => (
                        <span className="flex items-center gap-2">
                            <Link
                                to={tradePath(row.trade_id)}
                                className="text-accent hover:text-accent-bright"
                            >
                                {row.trade_id}
                            </Link>
                            {(errorsOf.get(row.trade_id)?.length ?? 0) > 0 && (
                                <Tag tone="down">{t(`${field}.error`)}</Tag>
                            )}
                        </span>
                    ),
                },
                {
                    id: 'type',
                    header: t(`${field}.tradeType`),
                    cell: (row) => row.trade_type || '—',
                },
                {
                    id: 'npv',
                    header: t(`${field}.npv`),
                    numeric: true,
                    cell: (row) => asFigure(row.npv),
                },
                {
                    id: 'npvBase',
                    header: t(`${field}.npvBase`),
                    numeric: true,
                    cell: (row) => asFigure(row.npv_base),
                },
                {
                    id: 'notional',
                    header: t(`${field}.notional`),
                    numeric: true,
                    cell: (row) => asFigure(row.notional),
                },
                {
                    id: 'book',
                    header: t(`${field}.book`),
                    cell: (row) =>
                        row.book_id === '' ? (
                            <span className="text-ink-muted">{t(`${field}.unbooked`)}</span>
                        ) : (
                            <Link
                                to={bookPath(row.book_id)}
                                className="text-accent hover:text-accent-bright"
                            >
                                {row.book_name === '' ? row.book_id : row.book_name}
                            </Link>
                        ),
                },
                {
                    id: 'counterparty',
                    header: t(`${field}.counterparty`),
                    cell: (row) =>
                        row.counterparty === '' ? (
                            '—'
                        ) : (
                            <Link
                                to={counterpartyPath(row.counterparty)}
                                className="text-accent hover:text-accent-bright"
                            >
                                {row.counterparty_name || row.counterparty}
                            </Link>
                        ),
                },
            ]}
        />
    );
}

/**
 * The run rolled up by book or counterparty, with the groups above their
 * members. A member's row opens its trades; a group has no row of its own.
 */
function AggregateTable({
    runId,
    level,
    rows,
    tradesOf,
    compared,
}: {
    readonly runId: string;
    readonly level: Exclude<ResultLevel, 'trade'>;
    readonly rows: readonly AggregateRow[];
    readonly tradesOf: (key: string) => string;
    readonly compared: boolean;
}): ReactNode {
    const { t } = useTranslation();
    const field = 'reporting.results';
    const nameOf = (row: AggregateRow): ReactNode => {
        if (row.key === '') {
            return t(`${field}.${level === 'book' ? 'unbooked' : 'unassigned'}`);
        }
        const name = row.name === '' ? row.key : row.name;
        if (row.is_group) {
            return name;
        }
        return (
            <Link
                to={level === 'book' ? bookPath(row.key) : counterpartyPath(row.key)}
                className="text-accent hover:text-accent-bright"
            >
                {name}
            </Link>
        );
    };
    return (
        <RecordTable<AggregateRow>
            source={localSource<AggregateRow>({
                key: `run-${level}s-${runId}`,
                rows,
                revision: `${compared ? 'compared' : 'alone'}:${rows.length}`,
                searchOf: (row) => row.name,
            })}
            plural={t(`${field}.${level}`)}
            keyOf={(row) => `${row.is_group ? 'group' : 'row'}-${row.key}`}
            pathOf={(row) => (row.is_group ? undefined : tradesOf(row.key))}
            columns={[
                {
                    id: 'name',
                    header: t(`${field}.${level}`),
                    cell: (row) => (
                        <span
                            className={`flex items-center gap-2 ${row.is_group ? 'font-medium' : ''}`}
                            style={{ paddingLeft: `${row.depth * 16}px` }}
                        >
                            {row.is_removed && <Tag tone="muted">{t(`${field}.gone`)}</Tag>}
                            {nameOf(row)}
                        </span>
                    ),
                },
                {
                    id: 'trades',
                    header: t(`${field}.trades`),
                    numeric: true,
                    cell: (row) => row.trades,
                },
                {
                    id: 'npv',
                    header: t(`${field}.npv`),
                    numeric: true,
                    cell: (row) => <Amounts amounts={row.totals} />,
                },
                {
                    id: 'notional',
                    header: t(`${field}.notional`),
                    numeric: true,
                    cell: (row) => <Amounts amounts={row.notional} />,
                },
                ...(compared
                    ? ([
                          {
                              id: 'change',
                              header: t(`${field}.change`),
                              numeric: true,
                              cell: (row) => <Amounts amounts={row.change} signed />,
                          },
                          {
                              id: 'changePct',
                              header: t(`${field}.changePct`),
                              numeric: true,
                              cell: (row) =>
                                  row.change_pct.map((money) => (
                                      <span key={money.currency} className="mr-2 text-ink-muted">
                                          {money.total === '' ? '—' : `${asFigure(money.total)}%`}
                                      </span>
                                  )),
                          },
                      ] satisfies ListColumn<AggregateRow>[])
                    : []),
            ]}
        />
    );
}

/**
 * The trade a row was opened onto: its figures, and the errors the run recorded
 * against it.
 */
function TradeResult({
    row,
    errors,
}: {
    readonly row: RunResultRow | undefined;
    readonly errors: readonly ReportRunErrorView[];
}): ReactNode {
    const { t } = useTranslation();
    const field = 'reporting.results';
    if (row === undefined) {
        return null;
    }
    return (
        <div className="mt-3 space-y-3 rounded-md border border-line-subtle bg-surface-raised p-4">
            <h3 className="text-sm font-medium">{t(`${field}.selectedTrade`)}</h3>
            <dl className="grid gap-x-8 gap-y-2 text-sm sm:grid-cols-3">
                <div>
                    <dt className="text-xs text-ink-faint">{t(`${field}.trade`)}</dt>
                    <dd className="font-mono text-xs">
                        <Link
                            to={tradePath(row.trade_id)}
                            className="text-accent hover:text-accent-bright"
                        >
                            {row.trade_id}
                        </Link>
                    </dd>
                </div>
                <div>
                    <dt className="text-xs text-ink-faint">{t(`${field}.type`)}</dt>
                    <dd>{row.trade_type}</dd>
                </div>
                <div>
                    <dt className="text-xs text-ink-faint">{t(`${field}.npv`)}</dt>
                    <dd className="font-mono">{asFigure(row.npv)}</dd>
                </div>
                <div>
                    <dt className="text-xs text-ink-faint">{t(`${field}.npvBase`)}</dt>
                    <dd className="font-mono">{asFigure(row.npv_base)}</dd>
                </div>
                <div>
                    <dt className="text-xs text-ink-faint">{t(`${field}.notional`)}</dt>
                    <dd className="font-mono">{asFigure(row.notional)}</dd>
                </div>
                <div>
                    <dt className="text-xs text-ink-faint">{t(`${field}.counterparty`)}</dt>
                    <dd>
                        {row.counterparty === '' ? (
                            '—'
                        ) : (
                            <Link
                                to={`/refdata/counterparties/${encodeURIComponent(row.counterparty)}`}
                                className="text-accent hover:text-accent-bright"
                            >
                                {row.counterparty_name === ''
                                    ? row.counterparty
                                    : row.counterparty_name}
                            </Link>
                        )}
                    </dd>
                </div>
            </dl>
            <div className="space-y-1">
                <h4 className="text-xs uppercase tracking-wide text-ink-faint">
                    {t(`${field}.tradeErrors`)}
                </h4>
                {errors.length === 0 ? (
                    <p className="text-sm text-ink-muted">{t(`${field}.noTradeError`)}</p>
                ) : (
                    <ul className="space-y-1">
                        {errors.map((error) => (
                            <li key={error.id} className="text-sm">
                                <span className="mr-2 rounded border border-line px-1 text-xs text-down">
                                    {error.severity}
                                </span>
                                {error.message}
                                {error.cause === null || error.cause === ''
                                    ? ''
                                    : ` (${error.cause})`}
                            </li>
                        ))}
                    </ul>
                )}
            </div>
        </div>
    );
}
