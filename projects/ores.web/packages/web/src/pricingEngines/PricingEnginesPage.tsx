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
 */

/**
 * The pricing engine configurations a party holds, from
 * doc/knowledge/journeys/reporting/journey_see_the_pricing_engines_a_party_holds.org.
 *
 * One row per configuration: its name, variant, how many products and settings
 * it holds and who changed it last. The server answers the party's
 * configurations whole, so the list searches and orders them here and says so
 * rather than pretending to a server search it does not have.
 */

import type { ReactNode } from 'react';
import { Link } from 'react-router';
import type { PricingSummary } from '../api/pricingEngines.js';
import { pricingEngines } from '../api/pricingEngines.js';
import { useTranslation } from '../i18n/Provider.js';
import {
    RecordList,
    type ListColumn,
    type ListSource,
    type PageRequest,
} from '../refdata/RecordList.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { pricingEnginePath } from '../shell/paths.js';
import { WorkflowMenu } from '../shell/WorkflowMenu.js';
import { Tag } from '../ui/Primitives.js';
import { RelativeTime } from '../ui/Time.js';

/** Where the services announce a change to a configuration, so the list can hear one. */
export const PRICING_ENGINES_EVENTS = {
    component: 'analytics',
    entity: 'pricing_model_configs',
} as const;

/** The fields a column orders by, which are the ones the rows carry. */
const SORTABLE = [
    'name',
    'partyName',
    'variant',
    'products',
    'parameters',
    'modifiedBy',
    'recordedAt',
] as const;

type SortField = (typeof SORTABLE)[number];

function isSortField(field: string): field is SortField {
    return (SORTABLE as readonly string[]).includes(field);
}

/** One page of the rows, searched and ordered as the list asked. */
export function pageOf(
    rows: readonly PricingSummary[],
    page: Pick<PageRequest, 'offset' | 'limit' | 'sort' | 'descending' | 'search'>,
): { readonly rows: readonly PricingSummary[]; readonly total: number } {
    const needle = page.search.trim().toLowerCase();
    const found =
        needle === ''
            ? [...rows]
            : rows.filter((row) =>
                  [row.name, row.description, row.variant, row.partyName, row.partyCode].some(
                      (text) => text.toLowerCase().includes(needle),
                  ),
              );
    const field = isSortField(page.sort) ? page.sort : 'name';
    found.sort((a, b) => {
        const left = a[field];
        const right = b[field];
        const order =
            typeof left === 'number' && typeof right === 'number'
                ? left - right
                : String(left).localeCompare(String(right));
        return page.descending ? -order : order;
    });
    return { rows: found.slice(page.offset, page.offset + page.limit), total: found.length };
}

const SOURCE: ListSource<PricingSummary> = {
    key: 'pricing-engines',
    read: async (page) => pageOf(await pricingEngines.list(), page),
    search: true,
    sortable: SORTABLE,
    mayAdd: false,
    watches: [PRICING_ENGINES_EVENTS],
    recordKey: (row) => ({ name: row.name }),
};

function useColumns(): readonly ListColumn<PricingSummary>[] {
    const { t } = useTranslation();
    return [
        {
            id: 'name',
            sort: 'name',
            header: t('pricingEngines.columns.name'),
            cell: (row) => (
                <Link to={pricingEnginePath(row.id)} className="text-accent">
                    {row.name}
                </Link>
            ),
        },
        {
            id: 'party',
            sort: 'partyName',
            header: t('pricingEngines.columns.party'),
            cell: (row) => (row.partyName === '' ? row.partyId : row.partyName),
        },
        {
            id: 'variant',
            sort: 'variant',
            header: t('pricingEngines.columns.variant'),
            cell: (row) =>
                row.variant === '' ? (
                    <span className="text-ink-faint">{t('pricingEngines.cell.noVariant')}</span>
                ) : (
                    <Tag>{row.variant}</Tag>
                ),
        },
        {
            id: 'products',
            sort: 'products',
            header: t('pricingEngines.columns.products'),
            cell: (row) => String(row.products),
            mono: true,
            numeric: true,
        },
        {
            id: 'parameters',
            sort: 'parameters',
            header: t('pricingEngines.columns.parameters'),
            cell: (row) => String(row.parameters),
            mono: true,
            numeric: true,
        },
        {
            id: 'modifiedBy',
            sort: 'modifiedBy',
            header: t('pricingEngines.columns.modifiedBy'),
            cell: (row) => row.modifiedBy,
        },
        {
            id: 'recordedAt',
            sort: 'recordedAt',
            header: t('pricingEngines.columns.changed'),
            cell: (row) => <RelativeTime at={row.recordedAt} />,
        },
    ];
}

export function PricingEnginesPage(): ReactNode {
    const { t } = useTranslation();
    const columns = useColumns();
    const trail = [...useAreaRoot('reporting'), { label: t('pricingEngines.title') }];
    return (
        <RecordList
            source={SOURCE}
            title={t('pricingEngines.title')}
            lead={t('pricingEngines.lead')}
            crumbs={trail}
            columns={columns}
            pathOf={(row) => pricingEnginePath(row.id)}
            actions={<WorkflowMenu on="pricingEngines" />}
        />
    );
}
