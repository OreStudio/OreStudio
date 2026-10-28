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

import { useMutation, useQuery } from '@tanstack/react-query';
import { useMemo, useState, type ReactNode } from 'react';
import { Link, useNavigate } from 'react-router';
import { structures } from '../api/structures.js';
import { useTranslation } from '../i18n/Provider.js';
import { Crumbs } from '../refdata/shared.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { PATHS } from '../shell/paths.js';
import { Button, Field, Notice, Select, Tag } from '../ui/Primitives.js';

/**
 * Grouping trades into one structure.
 *
 * The kind is chosen here and not afterwards, because it decides what the
 * customer receives: a package's legs each confirm on their own, and every other
 * kind confirms as one document. The legs keep the order they were picked in, so
 * the sequence a person reads on the structure matches the order they chose.
 *
 * The rules the database enforces are stated before the write: every leg shares
 * one counterparty, and a trade belongs to one structure at a time, which is why
 * only ungrouped trades are offered.
 */
export function GroupStructuresPage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const [kind, setKind] = useState('');
    const [chosen, setChosen] = useState<readonly string[]>([]);
    const [counterparty, setCounterparty] = useState('');

    const kinds = useQuery({ queryKey: ['structure-kinds'], queryFn: () => structures.kinds() });
    const candidates = useQuery({
        queryKey: ['ungrouped-trades'],
        queryFn: () => structures.ungroupedTrades(),
    });
    const group = useMutation({
        mutationFn: () => structures.group(kind, chosen),
        onSuccess: (made) => {
            void navigate(
                `${PATHS.structures}/${encodeURIComponent(
                    made.reference === '' ? made.id : made.reference,
                )}`,
            );
        },
    });

    /*
     * A structure's legs share one counterparty, and the screen refuses a mixed
     * pair, so the trades are ordered by counterparty and the picker filters to
     * one: a pair that may be grouped sits together instead of somewhere in a
     * list of eighty.
     */
    const all = candidates.data ?? [];
    const counterpartyNames = [...new Set(all.map((row) => row.counterparty_name))].sort(
        (left, right) => left.localeCompare(right),
    );
    const rows = all
        .filter((row) => counterparty === '' || row.counterparty_name === counterparty)
        .slice()
        .sort(
            (left, right) =>
                left.counterparty_name.localeCompare(right.counterparty_name) ||
                left.reference.localeCompare(right.reference),
        );
    const counterparties = useMemo(
        () =>
            new Set(
                rows.filter((row) => chosen.includes(row.id)).map((row) => row.counterparty_id),
            ),
        [rows, chosen],
    );
    const mixed = counterparties.size > 1;

    const toggle = (id: string): void => {
        setChosen((current) =>
            current.includes(id) ? current.filter((entry) => entry !== id) : [...current, id],
        );
    };

    const trail = [
        { label: t('shell.menu.home'), to: '/' },
        { label: t('shell.menu.trading'), to: PATHS.trading },
        { label: t('trading.trades.title'), to: PATHS.trades },
    ];

    return (
        <ScreenFrame
            trail={<Crumbs parts={[...trail, { label: t('trading.group.title') }]} />}
            title={t('trading.group.title')}
            description={t('trading.group.lead')}
        >
            <div className="space-y-4">
                {kinds.isError && <Notice tone="error">{kinds.error.message}</Notice>}
                {candidates.isError && <Notice tone="error">{candidates.error.message}</Notice>}
                {group.isError && <Notice tone="error">{group.error.message}</Notice>}

                <div className="grid gap-3 md:grid-cols-2">
                    <Field label={t('trading.group.kind')} hint={t('trading.group.kindHint')}>
                        <Select value={kind} onChange={(event) => setKind(event.target.value)}>
                            <option value="">{t('trading.booking.choose')}</option>
                            {(kinds.data ?? []).map((row) => (
                                <option key={row.code} value={row.code}>
                                    {row.confirms_as_whole
                                        ? t('trading.group.kindAsOne', { kind: row.code })
                                        : t('trading.group.kindLegByLeg', { kind: row.code })}
                                </option>
                            ))}
                        </Select>
                    </Field>
                    <Field label={t('trading.group.counterparty')}>
                        <Select
                            value={counterparty}
                            onChange={(event) => setCounterparty(event.target.value)}
                        >
                            <option value="">{t('trading.group.allCounterparties')}</option>
                            {counterpartyNames.map((name) => (
                                <option key={name} value={name}>
                                    {name}
                                </option>
                            ))}
                        </Select>
                    </Field>
                    <Field label={t('trading.group.chosen')}>
                        <p className="pt-2 text-sm text-ink">
                            {chosen.length === 0
                                ? t('trading.group.noneChosen')
                                : t('trading.group.chosenCount', { count: chosen.length })}
                        </p>
                    </Field>
                </div>

                {mixed && <Notice tone="info">{t('trading.group.mixedCounterparties')}</Notice>}

                <section className="overflow-hidden rounded-md border border-line">
                    <h2 className="border-b border-line bg-surface-2 px-3 py-2 text-sm font-medium text-ink">
                        {t('trading.group.pick')}
                    </h2>
                    {rows.length === 0 ? (
                        <p className="px-3 py-3 text-sm text-ink-muted">
                            {t('trading.group.none')}
                        </p>
                    ) : (
                        <table className="w-full text-left text-sm">
                            <thead>
                                <tr className="border-b border-line text-xs uppercase tracking-wide text-ink-faint">
                                    <th className="px-3 py-2 font-medium">
                                        {t('trading.group.pickColumn')}
                                    </th>
                                    <th className="px-3 py-2 font-medium">
                                        {t('trading.structure.trade')}
                                    </th>
                                    <th className="px-3 py-2 font-medium">
                                        {t('trading.structure.product')}
                                    </th>
                                    <th className="px-3 py-2 font-medium">
                                        {t('trading.structure.counterparty')}
                                    </th>
                                </tr>
                            </thead>
                            <tbody>
                                {rows.map((row) => (
                                    <tr key={row.id} className="border-b border-line last:border-0">
                                        <td className="px-3 py-2">
                                            <input
                                                type="checkbox"
                                                aria-label={
                                                    row.reference === '' ? row.id : row.reference
                                                }
                                                checked={chosen.includes(row.id)}
                                                onChange={() => toggle(row.id)}
                                            />
                                        </td>
                                        <td className="px-3 py-2 font-mono text-ink">
                                            <Link
                                                className="text-accent hover:text-accent-bright"
                                                to={`${PATHS.trades}/${row.id}`}
                                            >
                                                {row.reference === '' ? row.id : row.reference}
                                            </Link>
                                        </td>
                                        <td className="px-3 py-2 text-ink">
                                            <Tag tone="muted">{row.instrument_type}</Tag>
                                        </td>
                                        <td className="px-3 py-2 text-ink">
                                            {row.counterparty_name}
                                        </td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    )}
                </section>

                <Button
                    type="button"
                    variant="primary"
                    pending={group.isPending}
                    disabled={kind === '' || chosen.length < 2 || mixed}
                    onClick={() => group.mutate()}
                >
                    {t('trading.group.submit')}
                </Button>
            </div>
        </ScreenFrame>
    );
}
