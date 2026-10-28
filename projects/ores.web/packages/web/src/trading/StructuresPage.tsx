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

import { useQuery } from '@tanstack/react-query';
import { useEffect, useState, type ReactNode } from 'react';
import { Link, useNavigate, useSearchParams } from 'react-router';
import { structures, type StructureListRow } from '../api/structures.js';
import { useTranslation } from '../i18n/Provider.js';
import { Crumbs } from '../refdata/shared.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { PATHS } from '../shell/paths.js';
import { Button, Input, Select, Tag } from '../ui/Primitives.js';

/** How long the search waits after the last keystroke before it asks the server. */
const SEARCH_DELAY_MS = 250;

/** How many structures one page holds. */
const PAGE_SIZE = 25;

/** The path of a structure, by the reference the desk knows it by. */
export function structurePathFor(row: {
    readonly id: string;
    readonly reference: string;
}): string {
    return `${PATHS.structures}/${encodeURIComponent(row.reference === '' ? row.id : row.reference)}`;
}

/** What a row holds, in the words the prototype uses. */
function insideLabel(row: StructureListRow, t: (key: string, values?: Record<string, string>) => string): string {
    const parts: string[] = [];
    if (row.inner_structures > 0) {
        parts.push(
            t('trading.structures.innerCount', { count: String(row.inner_structures) }),
        );
    }
    if (row.legs > 0) {
        parts.push(t('trading.structures.legCount', { count: String(row.legs) }));
    }
    return parts.join(' · ');
}

/**
 * The structures a tenant holds.
 *
 * A structure is a tree: the deal the customer sees at the root, with strategies,
 * packages and trades inside. The list draws the trees rather than the rows, so a
 * structure that is inside another is indented under it and a person reads the
 * composition before they open anything.
 */
export function StructuresPage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const [search, setSearch] = useSearchParams();
    const [typed, setTyped] = useState(search.get('search') ?? '');
    const [kind, setKind] = useState(search.get('kind') ?? '');

    useEffect(() => {
        const timer = setTimeout(() => {
            const next = new URLSearchParams(search);
            if (typed === '') {
                next.delete('search');
            } else {
                next.set('search', typed);
            }
            setSearch(next, { replace: true });
        }, SEARCH_DELAY_MS);
        return () => clearTimeout(timer);
        // The address is the state the page reads, so the timer is reset by it.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [typed]);

    const page = useQuery({
        queryKey: ['structures', 'list', typed, kind],
        queryFn: () =>
            structures.list({ offset: 0, limit: PAGE_SIZE, search: typed, kind }),
    });

    const rows = page.data?.rows ?? [];
    // The page's own name closes the trail, as every list screen's does.
    const trail = [
        { label: t('shell.menu.home'), to: '/' },
        { label: t('shell.menu.trading'), to: PATHS.trading },
        { label: t('trading.structures.title') },
    ];

    return (
        <ScreenFrame
            trail={<Crumbs parts={trail} />}
            title={t('trading.structures.title')}
            description={t('trading.structures.lead')}
        >
            <div className="space-y-4">
                <div className="flex flex-wrap items-center gap-3">
                    <div className="relative min-w-72 flex-1">
                        <Input
                            type="search"
                            value={typed}
                            placeholder={t('trading.structures.search')}
                            aria-label={t('trading.structures.search')}
                            onChange={(event) => setTyped(event.target.value)}
                        />
                    </div>
                    <Select
                        aria-label={t('trading.structures.kindFilter')}
                        value={kind}
                        onChange={(event) => {
                            setKind(event.target.value);
                            const next = new URLSearchParams(search);
                            if (event.target.value === '') {
                                next.delete('kind');
                            } else {
                                next.set('kind', event.target.value);
                            }
                            setSearch(next, { replace: true });
                        }}
                    >
                        <option value="">{t('trading.structures.allKinds')}</option>
                        {(page.data?.kinds ?? []).map((code) => (
                            <option key={code} value={code}>
                                {code}
                            </option>
                        ))}
                    </Select>
                    <Button
                        type="button"
                        variant="primary"
                        onClick={() => {
                            void navigate(PATHS.groupStructures);
                        }}
                    >
                        {t('trading.group.open')}
                    </Button>
                </div>

                <section className="overflow-hidden rounded-md border border-line">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line bg-surface-2 text-xs uppercase tracking-wide text-ink-faint">
                                <th className="px-3 py-2 font-medium">
                                    {t('trading.structures.identifier')}
                                </th>
                                <th className="px-3 py-2 font-medium">
                                    {t('trading.structures.kind')}
                                </th>
                                <th className="px-3 py-2 font-medium">
                                    {t('trading.structures.template')}
                                </th>
                                <th className="px-3 py-2 font-medium">
                                    {t('trading.structures.counterparty')}
                                </th>
                                <th className="px-3 py-2 font-medium">
                                    {t('trading.structures.inside')}
                                </th>
                                <th className="px-3 py-2 font-medium">
                                    {t('trading.structures.confirms')}
                                </th>
                                <th className="px-3 py-2 font-medium">
                                    {t('trading.structures.priced')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {rows.length === 0 ? (
                                <tr>
                                    <td
                                        className="px-3 py-3 text-sm text-ink-muted"
                                        colSpan={7}
                                    >
                                        {page.isPending
                                            ? t('common.loading')
                                            : t('trading.structures.none')}
                                    </td>
                                </tr>
                            ) : (
                                rows.map((row) => (
                                    <tr
                                        key={row.id}
                                        className="border-b border-line last:border-0 hover:bg-surface-2"
                                    >
                                        <td className="px-3 py-2 font-mono">
                                            <span
                                                style={{ paddingLeft: `${row.depth * 18}px` }}
                                                className="inline-block"
                                            >
                                                {row.depth > 0 ? '└ ' : ''}
                                                <Link
                                                    className="text-accent hover:text-accent-bright"
                                                    to={structurePathFor(row)}
                                                >
                                                    {row.reference === ''
                                                        ? row.id
                                                        : row.reference}
                                                </Link>
                                            </span>
                                        </td>
                                        <td className="px-3 py-2">
                                            <Tag tone={row.kind === 'Package' ? 'muted' : 'neutral'}>
                                                {row.kind}
                                            </Tag>
                                        </td>
                                        <td className="px-3 py-2 text-ink">
                                            {row.template_code === '' ? '—' : row.template_code}
                                        </td>
                                        <td className="px-3 py-2 text-ink">
                                            {row.counterparty_id === '' ? (
                                                row.counterparty_name
                                            ) : (
                                                <Link
                                                    className="text-accent hover:text-accent-bright"
                                                    to={`${PATHS.counterparties}/${row.counterparty_id}`}
                                                >
                                                    {row.counterparty_name}
                                                </Link>
                                            )}
                                        </td>
                                        <td className="px-3 py-2 text-ink-muted">
                                            {insideLabel(row, t)}
                                        </td>
                                        <td className="px-3 py-2 text-ink-muted">
                                            {row.confirms}
                                        </td>
                                        <td className="px-3 py-2 text-ink-muted">{row.prices}</td>
                                    </tr>
                                ))
                            )}
                        </tbody>
                    </table>
                </section>

                <p className="text-xs text-ink-faint">
                    {t('trading.structures.showing', {
                        shown: String(Math.min(rows.length, page.data?.total ?? 0)),
                        total: String(page.data?.total ?? 0),
                    })}
                </p>
            </div>
        </ScreenFrame>
    );
}
