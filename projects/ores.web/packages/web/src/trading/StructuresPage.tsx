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
import { useNavigate } from 'react-router';
import { structures, type StructureListRow } from '../api/structures.js';
import { useTranslation } from '../i18n/Provider.js';
import { RecordList, type ListSource } from '../refdata/RecordList.js';
import { Link } from 'react-router';
import { PATHS } from '../shell/paths.js';
import { Button, Tag } from '../ui/Primitives.js';

/** The path of a structure, by the reference the desk knows it by. */
export function structurePathFor(row: { readonly id: string; readonly reference: string }): string {
    return `${PATHS.structures}/${encodeURIComponent(row.reference === '' ? row.id : row.reference)}`;
}

/**
 * The structures a tenant holds.
 *
 * A structure is a tree: the deal the customer sees at the root, with strategies,
 * packages and trades inside. The list is the application's own, so its search,
 * its filter, its paging and its footer are the ones every list screen has; what
 * this screen adds is the indent that draws a tree, and the columns that say what
 * a row holds and what it means.
 */
export function StructuresPage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();

    const source: ListSource<StructureListRow> = {
        key: 'structures',
        read: (page) =>
            structures.list({
                offset: page.offset,
                limit: page.limit,
                search: page.search,
                kind: page.filters['kind'] ?? '',
            }),
        search: true,
        sortable: [],
        filters: [
            {
                kind: 'choice',
                id: 'kind',
                label: t('trading.structures.kind'),
                all: t('trading.structures.allKinds'),
                choices: KINDS.map((kind) => ({ value: kind, label: kind })),
            },
        ],
        mayAdd: false,
    };

    return (
        <RecordList
            source={source}
            title={t('trading.structures.title')}
            lead={t('trading.structures.lead')}
            crumbs={[
                { label: t('shell.menu.home'), to: '/' },
                { label: t('shell.menu.trading'), to: PATHS.trading },
                { label: t('trading.structures.title') },
            ]}
            keyOf={(row) => row.id}
            pathOf={(row) => structurePathFor(row)}
            actions={
                <Button
                    type="button"
                    variant="secondary"
                    onClick={() => {
                        void navigate(PATHS.groupStructures);
                    }}
                >
                    {t('trading.group.open')}
                </Button>
            }
            columns={[
                {
                    id: 'reference',
                    header: t('trading.structures.identifier'),
                    mono: true,
                    cell: (row) => (
                        <span
                            style={{ paddingLeft: `${row.depth * 18}px` }}
                            className="inline-block"
                        >
                            {row.depth > 0 ? '└ ' : ''}
                            <Link
                                className="text-accent hover:text-accent-bright"
                                to={structurePathFor(row)}
                            >
                                {row.reference === '' ? row.id : row.reference}
                            </Link>
                        </span>
                    ),
                },
                {
                    id: 'kind',
                    header: t('trading.structures.kind'),
                    cell: (row) => (
                        <Tag tone={row.kind === 'Package' ? 'muted' : 'neutral'}>{row.kind}</Tag>
                    ),
                },
                {
                    id: 'template',
                    header: t('trading.structures.template'),
                    cell: (row) => (row.template_code === '' ? '—' : row.template_code),
                },
                {
                    id: 'counterparty',
                    header: t('trading.structures.counterparty'),
                    cell: (row) =>
                        row.counterparty_id === '' ? (
                            row.counterparty_name
                        ) : (
                            <Link
                                className="text-accent hover:text-accent-bright"
                                to={`${PATHS.counterparties}/${row.counterparty_id}`}
                            >
                                {row.counterparty_name}
                            </Link>
                        ),
                },
                {
                    id: 'inside',
                    header: t('trading.structures.inside'),
                    cell: (row) => {
                        const parts: string[] = [];
                        if (row.inner_structures > 0) {
                            parts.push(
                                t('trading.structures.innerCount', {
                                    count: String(row.inner_structures),
                                }),
                            );
                        }
                        if (row.legs > 0) {
                            parts.push(
                                t('trading.structures.legCount', { count: String(row.legs) }),
                            );
                        }
                        return parts.join(' · ');
                    },
                },
                {
                    id: 'confirms',
                    header: t('trading.structures.confirms'),
                    cell: (row) => row.confirms,
                },
                {
                    id: 'priced',
                    header: t('trading.structures.priced'),
                    cell: (row) => row.prices,
                },
            ]}
        />
    );
}

/** The kinds a structure can be, as the grouping screen offers them. */
const KINDS = ['Package', 'Strategy', 'Typed', 'Dynamic'];
