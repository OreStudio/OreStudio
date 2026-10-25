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
    type ListFilter,
} from '../refdata/RecordList.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { pricingEnginePath } from '../shell/paths.js';
import { WorkflowMenu } from '../shell/WorkflowMenu.js';
import { PARTY_COLUMN_ID, PARTY_FILTER_ID, usePartyScope } from '../refdata/partyScope.js';
import { Crumbs } from '../refdata/shared.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { Notice, Tag } from '../ui/Primitives.js';
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
    page: Pick<PageRequest, 'offset' | 'limit' | 'sort' | 'descending' | 'search'> & {
        readonly filters?: Readonly<Record<string, string>>;
    },
): { readonly rows: readonly PricingSummary[]; readonly total: number } {
    const needle = page.search.trim().toLowerCase();
    const party = page.filters?.[PARTY_FILTER_ID] ?? '';
    const owned = party === '' ? rows : rows.filter((row) => row.partyId === party);
    const found =
        needle === ''
            ? [...owned]
            : owned.filter((row) =>
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

function sourceOf(filter: ListFilter | undefined): ListSource<PricingSummary> {
    return {
        key: 'pricing-engines',
        // An address can carry a party from a time the account worked in several. A
        // list with no party filter has no way to clear it, so it ignores it.
        read: async (page) =>
            pageOf(
                await pricingEngines.list(),
                filter === undefined ? { ...page, filters: {} } : page,
            ),
        search: true,
        sortable: SORTABLE,
        ...(filter === undefined ? {} : { filters: [filter] }),
        mayAdd: false,
        watches: [PRICING_ENGINES_EVENTS],
        recordKey: (row) => ({ name: row.name }),
    };
}

function useColumns(showParty: boolean): readonly ListColumn<PricingSummary>[] {
    const { t } = useTranslation();
    const party = {
        id: PARTY_COLUMN_ID,
        sort: 'partyName' as const,
        header: t('pricingEngines.columns.party'),
        cell: (row: PricingSummary) => (row.partyName === '' ? row.partyId : row.partyName),
    };
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
        ...(showParty ? [party] : []),
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
    const scope = usePartyScope({
        label: t('pricingEngines.columns.party'),
        all: t('pricingEngines.allParties'),
    });
    const columns = useColumns(scope.several);
    const source = sourceOf(scope.filter);
    const trail = [...useAreaRoot('reporting'), { label: t('pricingEngines.title') }];
    if (scope.pending || scope.error !== null) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={trail} />}
                title={t('pricingEngines.title')}
                description={t('pricingEngines.lead')}
            >
                {scope.error === null ? (
                    <p className="text-sm text-ink-muted" role="status">
                        {t('common.loading')}
                    </p>
                ) : (
                    <Notice tone="error">{scope.error}</Notice>
                )}
            </ScreenFrame>
        );
    }
    return (
        <RecordList
            source={source}
            title={t('pricingEngines.title')}
            lead={t('pricingEngines.lead')}
            crumbs={trail}
            columns={columns}
            pathOf={(row) => pricingEnginePath(row.id)}
            actions={<WorkflowMenu on="pricingEngines" />}
        />
    );
}
