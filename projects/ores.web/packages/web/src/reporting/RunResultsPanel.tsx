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

/** One trade's result, as the engine wrote it. The figures stay text. */
type RunResultRow = {
    readonly tradeId: string;
    readonly tradeType: string;
    readonly npv: string;
    readonly npvBase: string;
    readonly notional: string;
    readonly counterparty: string;
};

const asText = (value: unknown): string => (typeof value === 'string' ? value : '');

const toResultRow = (raw: unknown): RunResultRow => {
    const row = raw as Record<string, unknown>;
    return {
        tradeId: asText(row['trade_id']),
        tradeType: asText(row['trade_type']),
        npv: asText(row['npv']),
        npvBase: asText(row['npv_base']),
        notional: asText(row['notional']),
        counterparty: asText(row['counterparty']),
    };
};

/**
 * What a run produced, figures first: the count and the sum, then the rows.
 *
 * A first draft of variant A of the "read what a run produced" prototype. The
 * comparison, the book tree and the plots are not here; the figures and the rows
 * are. The loader decides which run is read, so the run page asks for one run
 * and the report page for the latest run of the report.
 */
export function RunResultsPanel({
    queryKey,
    load,
}: {
    readonly queryKey: readonly unknown[];
    readonly load: () => Promise<{ rows: readonly unknown[]; message: string }>;
}): ReactNode {
    const { t } = useTranslation();
    const results = useQuery({ queryKey: [...queryKey], queryFn: load, meta: { quiet: true } });
    if (results.isPending) {
        return null;
    }
    const field = 'operations.grid.reportRun';
    const rows = (results.data?.rows ?? []).map(toResultRow);
    const total = rows.reduce((sum, row) => sum + (Number.parseFloat(row.npv) || 0), 0);
    return (
        <section className="card space-y-3 p-6">
            <h2 className="text-lg font-medium">{t(`${field}.results`)}</h2>
            {rows.length === 0 ? (
                <p className="text-sm text-ink-muted">
                    {results.data?.message === ''
                        ? t(`${field}.resultsEmpty`)
                        : (results.data?.message ?? t(`${field}.resultsEmpty`))}
                </p>
            ) : (
                <>
                    <dl className="flex flex-wrap gap-8">
                        <div>
                            <dt className="text-xs text-ink-faint">
                                {t(`${field}.resultsTrades`)}
                            </dt>
                            <dd className="font-mono text-sm">{rows.length}</dd>
                        </div>
                        <div>
                            <dt className="text-xs text-ink-faint">
                                {t(`${field}.resultsTotalNpv`)}
                            </dt>
                            <dd className="font-mono text-sm">{total.toFixed(2)}</dd>
                        </div>
                    </dl>
                    <div className="overflow-x-auto">
                        <table className="w-full text-left text-sm">
                            <thead>
                                <tr className="text-xs uppercase tracking-wide text-ink-faint">
                                    <th className="px-2 py-1">{t(`${field}.resultsTrade`)}</th>
                                    <th className="px-2 py-1">{t(`${field}.resultsType`)}</th>
                                    <th className="px-2 py-1">{t(`${field}.resultsNpv`)}</th>
                                    <th className="px-2 py-1">{t(`${field}.resultsNpvBase`)}</th>
                                    <th className="px-2 py-1">{t(`${field}.resultsNotional`)}</th>
                                    <th className="px-2 py-1">
                                        {t(`${field}.resultsCounterparty`)}
                                    </th>
                                </tr>
                            </thead>
                            <tbody>
                                {rows.map((row) => (
                                    <tr key={row.tradeId} className="border-t border-line-subtle">
                                        <td className="px-2 py-1 font-mono text-xs">
                                            {row.tradeId.slice(0, 8)}…
                                        </td>
                                        <td className="px-2 py-1">{row.tradeType}</td>
                                        <td className="px-2 py-1 font-mono">{row.npv}</td>
                                        <td className="px-2 py-1 font-mono">{row.npvBase}</td>
                                        <td className="px-2 py-1 font-mono">{row.notional}</td>
                                        <td className="px-2 py-1">{row.counterparty}</td>
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
