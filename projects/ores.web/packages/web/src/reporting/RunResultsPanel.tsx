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

import { useMemo, useState, type ReactNode } from 'react';
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
import { useTranslation } from '../i18n/Provider.js';
import { bookPath, counterpartyPath, tradePath } from '../shell/paths.js';

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
    'no-output': 'reporting.results.noOutput',
    'no-result-file': 'reporting.results.noResultFile',
    'no-figures': 'reporting.results.noFigures',
};

/** A figure with its thousands grouped and two decimals, or the text as written. */
const asFigure = (text: string): string => {
    const figure = Number.parseFloat(text);
    if (!Number.isFinite(figure)) {
        return text;
    }
    return new Intl.NumberFormat('en-GB', {
        minimumFractionDigits: 2,
        maximumFractionDigits: 2,
    }).format(figure);
};

const asText = (value: unknown): string => (typeof value === 'string' ? value : '');

const toTrade = (raw: unknown): RunResultRow => {
    const row = raw as Record<string, unknown>;
    return {
        trade_id: asText(row['trade_id']),
        trade_type: asText(row['trade_type']),
        npv: asText(row['npv']),
        npv_currency: asText(row['npv_currency']),
        npv_base: asText(row['npv_base']),
        notional: asText(row['notional']),
        notional_currency: asText(row['notional_currency']),
        counterparty: asText(row['counterparty']),
        counterparty_name: asText(row['counterparty_name']),
        book_id: asText(row['book_id']),
        book_name: asText(row['book_name']),
    };
};

const toAggregate = (raw: unknown): AggregateRow => {
    const row = raw as Record<string, unknown>;
    const money = (value: unknown): readonly Money[] =>
        Array.isArray(value)
            ? value.map((entry) => {
                  const item = entry as Record<string, unknown>;
                  return { currency: asText(item['currency']), total: asText(item['total']) };
              })
            : [];
    return {
        key: asText(row['key']),
        name: asText(row['name']),
        trades: typeof row['trades'] === 'number' ? row['trades'] : 0,
        totals: money(row['totals']),
        notional: money(row['notional']),
        group: asText(row['group']),
        is_group: row['is_group'] === true,
        previous: money(row['previous']),
        change: money(row['change']),
        change_pct: money(row['change_pct']),
        is_removed: row['is_removed'] === true,
    };
};

type RunError = {
    readonly scope: string;
    readonly severity: string;
    readonly message: string;
    readonly cause: string;
    readonly subjectId: string;
};

const toError = (raw: unknown): RunError => {
    const row = raw as Record<string, unknown>;
    return {
        scope: asText(row['scope']),
        severity: asText(row['severity']),
        message: asText(row['message']),
        cause: asText(row['cause']),
        subjectId: asText(row['subject_id']),
    };
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
    const errors = useQuery({
        queryKey: ['reporting-run-errors', runId],
        queryFn: () => api.gridReportRunErrors(runId),
        meta: { quiet: true },
    });
    const errorsOf = useMemo(() => {
        const byTrade = new Map<string, RunError[]>();
        for (const error of (errors.data ?? []).map(toError)) {
            if (error.scope !== 'Trade' || error.subjectId === '') {
                continue;
            }
            byTrade.set(error.subjectId, [...(byTrade.get(error.subjectId) ?? []), error]);
        }
        return byTrade;
    }, [errors.data]);
    const [selected, setSelected] = useState('');

    const candidates = (otherRuns.data ?? [])
        .map((entry) => entry as Record<string, unknown>)
        .filter((entry) => asText(entry['state_name']) === 'completed')
        .filter((entry) => asText(entry['id']) !== runId)
        .map((entry) => ({ id: asText(entry['id']), name: asText(entry['name']) }));
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
    const chooseFilter = (key: string): void => {
        const params = new URLSearchParams(search);
        params.set('by', 'trade');
        params.delete('book');
        params.delete('counterparty');
        params.delete('nobook');
        if (level === 'book' && key === '') {
            params.set('nobook', '1');
        } else if (level === 'book') {
            params.set('book', key);
        } else if (level === 'counterparty') {
            params.set('counterparty', key);
        }
        setSearch(params, { replace: true });
    };
    const clearFilter = (): void => {
        const params = new URLSearchParams(search);
        params.delete('book');
        params.delete('counterparty');
        setSearch(params, { replace: true });
    };

    const rows = results.data?.rows ?? [];
    const trades = level === 'trade' ? rows.map(toTrade) : [];
    const aggregates = level === 'trade' ? [] : rows.map(toAggregate);
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
    const tradeCount =
        level === 'trade' ? trades.length : aggregates.reduce((sum, row) => sum + row.trades, 0);
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
            {results.isError && <p className="text-sm text-down">{t(`${field}.failed`)}</p>}
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
                            {level === 'trade' ? (
                                <TradeTable
                                    rows={filtered}
                                    errorsOf={errorsOf}
                                    selected={selected}
                                    onSelect={(id) => setSelected(selected === id ? '' : id)}
                                />
                            ) : (
                                <AggregateTable
                                    level={level}
                                    rows={aggregates}
                                    onTrades={chooseFilter}
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

function TradeTable({
    rows,
    errorsOf,
    selected,
    onSelect,
}: {
    readonly rows: readonly RunResultRow[];
    readonly errorsOf: ReadonlyMap<string, readonly RunError[]>;
    readonly selected: string;
    readonly onSelect: (id: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const field = 'reporting.results';
    return (
        <div className="overflow-x-auto">
            <table className="w-full text-left text-sm">
                <thead>
                    <tr className="text-xs text-ink-faint">
                        <th className="px-2 py-1 font-normal">{t(`${field}.tradeId`)}</th>
                        <th className="px-2 py-1 font-normal">{t(`${field}.tradeType`)}</th>
                        <th className="px-2 py-1 text-right font-normal">{t(`${field}.npv`)}</th>
                        <th className="px-2 py-1 text-right font-normal">
                            {t(`${field}.npvBase`)}
                        </th>
                        <th className="px-2 py-1 text-right font-normal">
                            {t(`${field}.notional`)}
                        </th>
                        <th className="px-2 py-1 font-normal">{t(`${field}.book`)}</th>
                        <th className="px-2 py-1 font-normal">{t(`${field}.counterparty`)}</th>
                    </tr>
                </thead>
                <tbody>
                    {rows.map((row) => {
                        const tradeErrors = errorsOf.get(row.trade_id) ?? [];
                        const chosen = selected === row.trade_id;
                        return (
                            <tr
                                key={row.trade_id}
                                onClick={() => onSelect(row.trade_id)}
                                className={`cursor-pointer border-t border-line-subtle hover:bg-surface-raised ${
                                    chosen ? 'bg-surface-raised' : ''
                                }`}
                            >
                                <td className="px-2 py-1 font-mono text-xs">
                                    <Link
                                        to={tradePath(row.trade_id)}
                                        className="text-accent hover:text-accent-bright"
                                    >
                                        {row.trade_id}
                                    </Link>
                                    {tradeErrors.length > 0 && (
                                        <span className="ml-2 rounded border border-line px-1 text-xs text-down">
                                            {t(`${field}.error`)}
                                        </span>
                                    )}
                                </td>
                                <td className="px-2 py-1 uppercase">{row.trade_type}</td>
                                <td className="px-2 py-1 text-right font-mono">
                                    {asFigure(row.npv)}
                                </td>
                                <td className="px-2 py-1 text-right font-mono">
                                    {asFigure(row.npv_base)}
                                </td>
                                <td className="px-2 py-1 text-right font-mono">
                                    {asFigure(row.notional)}
                                </td>
                                <td className="px-2 py-1">
                                    {row.book_id === '' ? (
                                        <span className="text-ink-muted">
                                            {t(`${field}.unbooked`)}
                                        </span>
                                    ) : (
                                        <Link
                                            to={bookPath(row.book_id)}
                                            className="text-accent hover:text-accent-bright"
                                        >
                                            {row.book_name === '' ? row.book_id : row.book_name}
                                        </Link>
                                    )}
                                </td>
                                <td className="px-2 py-1 uppercase">
                                    {row.counterparty === '' ? (
                                        '—'
                                    ) : (
                                        <Link
                                            to={counterpartyPath(row.counterparty)}
                                            className="text-accent hover:text-accent-bright"
                                        >
                                            {row.counterparty_name}
                                        </Link>
                                    )}
                                </td>
                            </tr>
                        );
                    })}
                </tbody>
            </table>
        </div>
    );
}

function AggregateTable({
    level,
    rows,
    onTrades,
    compared,
}: {
    readonly level: Exclude<ResultLevel, 'trade'>;
    readonly rows: readonly AggregateRow[];
    readonly onTrades: (key: string) => void;
    readonly compared: boolean;
}): ReactNode {
    const { t } = useTranslation();
    const field = 'reporting.results';
    return (
        <div className="overflow-x-auto">
            <table className="w-full text-left text-sm">
                <thead>
                    <tr className="text-xs text-ink-faint">
                        <th className="px-2 py-1 font-normal">{t(`${field}.${level}`)}</th>
                        <th className="px-2 py-1 text-right font-normal">{t(`${field}.trades`)}</th>
                        <th className="px-2 py-1 text-right font-normal">{t(`${field}.npv`)}</th>
                        <th className="px-2 py-1 text-right font-normal">
                            {t(`${field}.notional`)}
                        </th>
                        {compared && (
                            <>
                                <th className="px-2 py-1 text-right font-normal">
                                    {t(`${field}.change`)}
                                </th>
                                <th className="px-2 py-1 text-right font-normal">
                                    {t(`${field}.changePct`)}
                                </th>
                            </>
                        )}
                        <th className="px-2 py-1 font-normal">{t(`${field}.actions`)}</th>
                    </tr>
                </thead>
                <tbody>
                    {rows.map((row) => (
                        <tr
                            key={`${row.is_group ? 'group' : 'row'}-${row.key}`}
                            className={`border-t border-line-subtle ${
                                row.is_group ? 'font-medium' : ''
                            }`}
                        >
                            <td className={`px-2 py-1 uppercase ${row.group === '' ? '' : 'pl-8'}`}>
                                {row.is_removed && (
                                    <span className="mr-2 rounded border border-line px-1 text-xs text-ink-muted">
                                        {t(`${field}.gone`)}
                                    </span>
                                )}
                                {row.key === '' ? (
                                    level === 'book' ? (
                                        t(`${field}.unbooked`)
                                    ) : (
                                        t(`${field}.unassigned`)
                                    )
                                ) : level === 'book' ? (
                                    <Link
                                        to={bookPath(row.key)}
                                        className="text-accent hover:text-accent-bright"
                                    >
                                        {row.name === '' ? row.key : row.name}
                                    </Link>
                                ) : (
                                    <Link
                                        to={counterpartyPath(row.key)}
                                        className="text-accent hover:text-accent-bright"
                                    >
                                        {row.name === '' ? row.key : row.name}
                                    </Link>
                                )}
                            </td>
                            <td className="px-2 py-1 text-right font-mono">{row.trades}</td>
                            <td className="px-2 py-1 text-right font-mono">
                                {row.totals.map((total) => (
                                    <span key={total.currency} className="mr-2">
                                        {asFigure(total.total)}
                                        {total.currency === '' ? '' : ` ${total.currency}`}
                                    </span>
                                ))}
                            </td>
                            <td className="px-2 py-1 text-right font-mono">
                                {row.notional.map((total) => (
                                    <span key={total.currency} className="mr-2">
                                        {asFigure(total.total)}
                                        {total.currency === '' ? '' : ` ${total.currency}`}
                                    </span>
                                ))}
                            </td>
                            {compared && (
                                <>
                                    <td className="px-2 py-1 text-right font-mono">
                                        {row.change.map((money) => (
                                            <span
                                                key={money.currency}
                                                className={`mr-2 ${
                                                    money.total.startsWith('-') ? 'text-down' : ''
                                                }`}
                                            >
                                                {asFigure(money.total)}
                                                {money.currency === '' ? '' : ` ${money.currency}`}
                                            </span>
                                        ))}
                                    </td>
                                    <td className="px-2 py-1 text-right font-mono text-ink-muted">
                                        {row.change_pct.map((money) => (
                                            <span key={money.currency} className="mr-2">
                                                {money.total === ''
                                                    ? '—'
                                                    : `${asFigure(money.total)}%`}
                                            </span>
                                        ))}
                                    </td>
                                </>
                            )}
                            <td className="px-2 py-1">
                                {row.is_group ? (
                                    ''
                                ) : (
                                    <button
                                        type="button"
                                        onClick={() => onTrades(row.key)}
                                        className="text-accent hover:text-accent-bright"
                                    >
                                        {t(`${field}.showTrades`)}
                                    </button>
                                )}
                            </td>
                        </tr>
                    ))}
                </tbody>
            </table>
        </div>
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
    readonly errors: readonly RunError[];
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
                            <li key={error.message} className="text-sm">
                                <span className="mr-2 rounded border border-line px-1 text-xs text-down">
                                    {error.severity}
                                </span>
                                {error.message}
                                {error.cause === '' ? '' : ` (${error.cause})`}
                            </li>
                        ))}
                    </ul>
                )}
            </div>
        </div>
    );
}
