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
import { useMemo, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import type { ResultReason } from '../api/client.js';

/** One trade's result, as the engine wrote it. The figures stay text. */
type RunResultRow = {
    readonly trade_id: string;
    readonly trade_type: string;
    readonly npv: string;
    readonly npv_currency: string;
    readonly npv_base: string;
    readonly notional: string;
    readonly counterparty: string;
    readonly counterparty_name: string;
};

const REASON_TEXT: Readonly<Record<Exclude<ResultReason, ''>, string>> = {
    'no-run': 'reporting.results.noRun',
    'no-output': 'reporting.results.noOutput',
    'no-result-file': 'reporting.results.noResultFile',
    'no-figures': 'reporting.results.noFigures',
};

const asText = (value: unknown): string => (typeof value === 'string' ? value : '');

/** One error the run recorded, as the run states it. */
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

const toRow = (raw: unknown): RunResultRow => {
    const row = raw as Record<string, unknown>;
    return {
        trade_id: asText(row['trade_id']),
        trade_type: asText(row['trade_type']),
        npv: asText(row['npv']),
        npv_currency: asText(row['npv_currency']),
        npv_base: asText(row['npv_base']),
        notional: asText(row['notional']),
        counterparty: asText(row['counterparty']),
        counterparty_name: asText(row['counterparty_name']),
    };
};

/** The total NPV of each currency the rows are stated in. */
function totalsOf(rows: readonly RunResultRow[]): readonly { currency: string; total: number }[] {
    const totals = new Map<string, number>();
    for (const row of rows) {
        const figure = Number.parseFloat(row.npv);
        if (!Number.isFinite(figure)) {
            continue;
        }
        const currency = row.npv_currency === '' ? '' : row.npv_currency;
        totals.set(currency, (totals.get(currency) ?? 0) + figure);
    }
    return [...totals.entries()].map(([currency, total]) => ({ currency, total }));
}

/**
 * What the latest run produced, figures first: the count and the totals, then
 * the per-trade rows.
 */
export function RunResultsPanel({
    runId,
    queryKey,
    load,
}: {
    readonly runId: string;
    readonly queryKey: readonly unknown[];
    readonly load: () => Promise<{ rows: readonly unknown[]; reason: ResultReason }>;
}): ReactNode {
    const { t } = useTranslation();
    const field = 'reporting.results';
    const results = useQuery({ queryKey: [...queryKey], queryFn: load, meta: { quiet: true } });
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
    if (results.isPending) {
        return null;
    }
    if (results.isError) {
        return (
            <section className="card space-y-3 p-6">
                <h2 className="text-lg font-medium">{t(`${field}.title`)}</h2>
                <p className="text-sm text-danger">{t(`${field}.failed`)}</p>
            </section>
        );
    }
    const rows = (results.data?.rows ?? []).map(toRow);
    const reason = results.data?.reason ?? '';
    return (
        <section className="card space-y-3 p-6">
            <h2 className="text-lg font-medium">{t(`${field}.title`)}</h2>
            {rows.length === 0 ? (
                <p className="text-sm text-ink-muted">
                    {reason === ''
                        ? t(`${field}.noRun`)
                        : t(REASON_TEXT[reason as Exclude<ResultReason, ''>])}
                </p>
            ) : (
                <>
                    <dl className="flex flex-wrap gap-8">
                        <div>
                            <dt className="text-xs text-ink-faint">{t(`${field}.trades`)}</dt>
                            <dd className="font-mono text-sm">{rows.length}</dd>
                        </div>
                        {totalsOf(rows).map((total) => (
                            <div key={total.currency}>
                                <dt className="text-xs text-ink-faint">
                                    {total.currency === ''
                                        ? t(`${field}.total`)
                                        : t(`${field}.totalIn`, { currency: total.currency })}
                                </dt>
                                <dd className="font-mono text-sm">{total.total.toFixed(2)}</dd>
                            </div>
                        ))}
                    </dl>
                    <div className="overflow-x-auto">
                        <table className="w-full text-left text-sm">
                            <thead>
                                <tr className="text-xs uppercase tracking-wide text-ink-faint">
                                    <th className="px-2 py-1">{t(`${field}.trade`)}</th>
                                    <th className="px-2 py-1">{t(`${field}.type`)}</th>
                                    <th className="px-2 py-1">{t(`${field}.npv`)}</th>
                                    <th className="px-2 py-1">{t(`${field}.npvBase`)}</th>
                                    <th className="px-2 py-1">{t(`${field}.notional`)}</th>
                                    <th className="px-2 py-1">{t(`${field}.counterparty`)}</th>
                                </tr>
                            </thead>
                            <tbody>
                                {rows.map((row) => {
                                    const tradeErrors = errorsOf.get(row.trade_id) ?? [];
                                    const chosen = selected === row.trade_id;
                                    return (
                                        <tr
                                            key={row.trade_id}
                                            onClick={() => setSelected(chosen ? '' : row.trade_id)}
                                            className={`cursor-pointer border-t border-line-subtle ${
                                                chosen ? 'bg-surface-raised' : ''
                                            }`}
                                        >
                                            <td className="px-2 py-1 font-mono text-xs">
                                                <Link
                                                    to={`/trading/trades/${encodeURIComponent(row.trade_id)}`}
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
                                            <td className="px-2 py-1">{row.trade_type}</td>
                                            <td className="px-2 py-1 font-mono">{row.npv}</td>
                                            <td className="px-2 py-1 font-mono">{row.npv_base}</td>
                                            <td className="px-2 py-1 font-mono">{row.notional}</td>
                                            <td className="px-2 py-1">
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
                                            </td>
                                        </tr>
                                    );
                                })}
                            </tbody>
                        </table>
                        {selected !== '' && (
                            <TradeResult
                                row={rows.find((row) => row.trade_id === selected)}
                                errors={errorsOf.get(selected) ?? []}
                            />
                        )}
                    </div>
                </>
            )}
        </section>
    );
}

/**
 * The trade a row was opened onto: its figures, and the errors the run recorded
 * against it.
 *
 * A trade the engine could not build writes no row into npv.csv, so a failed
 * trade's price cells are empty by nature. Every error shown here names the
 * trade it is about.
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
                            to={`/trading/trades/${encodeURIComponent(row.trade_id)}`}
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
                    <dd className="font-mono">{row.npv}</dd>
                </div>
                <div>
                    <dt className="text-xs text-ink-faint">{t(`${field}.npvBase`)}</dt>
                    <dd className="font-mono">{row.npv_base}</dd>
                </div>
                <div>
                    <dt className="text-xs text-ink-faint">{t(`${field}.notional`)}</dt>
                    <dd className="font-mono">{row.notional}</dd>
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
