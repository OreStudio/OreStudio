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
import { useQuery } from '@tanstack/react-query';
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
};

const REASON_TEXT: Readonly<Record<Exclude<ResultReason, ''>, string>> = {
    'no-run': 'reporting.results.noRun',
    'no-output': 'reporting.results.noOutput',
    'no-result-file': 'reporting.results.noResultFile',
    'no-figures': 'reporting.results.noFigures',
};

const asText = (value: unknown): string => (typeof value === 'string' ? value : '');

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
    queryKey,
    load,
}: {
    readonly queryKey: readonly unknown[];
    readonly load: () => Promise<{ rows: readonly unknown[]; reason: ResultReason }>;
}): ReactNode {
    const { t } = useTranslation();
    const field = 'reporting.results';
    const results = useQuery({ queryKey: [...queryKey], queryFn: load, meta: { quiet: true } });
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
                                {rows.map((row) => (
                                    <tr key={row.trade_id} className="border-t border-line-subtle">
                                        <td className="px-2 py-1 font-mono text-xs">
                                            {row.trade_id.slice(0, 8)}…
                                        </td>
                                        <td className="px-2 py-1">{row.trade_type}</td>
                                        <td className="px-2 py-1 font-mono">{row.npv}</td>
                                        <td className="px-2 py-1 font-mono">{row.npv_base}</td>
                                        <td className="px-2 py-1 font-mono">{row.notional}</td>
                                        <td className="px-2 py-1 font-mono text-xs">
                                            {row.counterparty.slice(0, 8)}…
                                        </td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    </div>
                </>
            )}
        </section>
    );
}
