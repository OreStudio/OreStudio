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
import type { ReactNode } from 'react';
import { useNavigate } from 'react-router';
import type { TenantPage, TenantSummary } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import {
    RecordList,
    type ListNote,
    type ListPage,
    type ListSource,
    type PageRequest,
} from '../refdata/RecordList.js';
import { PaintedValue, SetupCell } from './TenantParts.js';

/** The address of one tenant's page. */
export function tenantPath(code?: string): string {
    return code === undefined ? '/tenants' : `/tenants/${encodeURIComponent(code)}`;
}

/** The roster query for one page of the list: its search, order, filters and bounds. */
export function tenantQuery(page: PageRequest): Parameters<typeof api.tenants>[0] {
    return {
        search: page.search,
        type: page.filters['type'] ?? '',
        status: page.filters['status'] ?? '',
        includeTest: page.filters['test'] === '1',
        sort: page.sort,
        descending: page.descending,
        offset: page.offset,
        limit: page.limit,
    };
}

/** The roster's answer as a page of the list, with what it says about the page as notes. */
export function tenantListPage(
    read: TenantPage,
    t: (key: string) => string,
    plural: (key: string, count: number) => string,
): ListPage<TenantSummary> {
    const notes: ListNote[] = [];
    if (read.setupUnavailable) {
        notes.push({ tone: 'warn', text: t('tenants.setupUnavailable') });
    }
    if (read.hiddenTestCount > 0) {
        notes.push({ tone: 'info', text: plural('tenants.hiddenTest', read.hiddenTestCount) });
    }
    return { rows: read.tenants, total: read.totalCount, notes };
}

/**
 * The tenants a deployment holds: the shared record list the system
 * administration area opens on. It writes nothing; a row opens the tenant,
 * where its setup is resumed and the tenant is deleted.
 *
 * The deployment's own record, the system tenant, is on this roster like any
 * other, and opening it is how the system administrator reaches the
 * deployment's own data. Test tenants are hidden unless the person asks for
 * them, and the server says how many it hid. A deployment with no tenant of
 * its own is the state right after the system administrator is created, and
 * the empty list offers Add.
 */
export function TenantsPage(): ReactNode {
    const { t, plural } = useTranslation();
    const navigate = useNavigate();
    const statuses = useQuery({ queryKey: ['tenant-statuses'], queryFn: api.tenantStatuses });
    const types = useQuery({ queryKey: ['tenant-types'], queryFn: api.tenantTypes });
    const statusByCode = new Map((statuses.data ?? []).map((status) => [status.code, status]));
    const typeByCode = new Map((types.data ?? []).map((type) => [type.code, type]));

    const source: ListSource<TenantSummary> = {
        key: 'tenants',
        read: async (page) => tenantListPage(await api.tenants(tenantQuery(page)), t, plural),
        search: true,
        sortable: ['code', 'name'],
        filters: [
            {
                kind: 'choice',
                id: 'type',
                label: t('tenants.filterType'),
                all: t('tenants.allTypes'),
                choices: (types.data ?? []).map((type) => ({ value: type.code, label: type.name })),
            },
            {
                kind: 'choice',
                id: 'status',
                label: t('tenants.filterStatus'),
                all: t('tenants.allStatuses'),
                choices: (statuses.data ?? []).map((status) => ({
                    value: status.code,
                    label: status.name,
                })),
            },
            { kind: 'toggle', id: 'test', label: t('tenants.showTest') },
        ],
        mayAdd: true,
    };

    return (
        <RecordList
            source={source}
            title={t('tenants.title')}
            lead={t('tenants.description')}
            crumbs={[{ label: t('shell.menu.home'), to: '/' }, { label: t('tenants.title') }]}
            pathOf={(tenant) => tenantPath(tenant.code)}
            addLabel={t('tenants.add')}
            onAdd={() => void navigate('/tenants/new')}
            columns={[
                {
                    id: 'code',
                    header: t('tenants.code'),
                    cell: (tenant) => tenant.code,
                    mono: true,
                    sort: 'code',
                },
                {
                    id: 'name',
                    header: t('tenants.name'),
                    cell: (tenant) => tenant.name,
                    sort: 'name',
                },
                {
                    id: 'hostname',
                    header: t('tenants.hostname'),
                    cell: (tenant) => tenant.hostname,
                    mono: true,
                },
                {
                    id: 'type',
                    header: t('tenants.type'),
                    cell: (tenant) => (
                        <PaintedValue value={tenant.type} known={typeByCode.get(tenant.type)} />
                    ),
                },
                {
                    id: 'status',
                    header: t('tenants.status'),
                    cell: (tenant) => (
                        <PaintedValue
                            value={tenant.status}
                            known={statusByCode.get(tenant.status)}
                        />
                    ),
                },
                {
                    id: 'setup',
                    header: t('tenants.setup'),
                    cell: (tenant) => <SetupCell setup={tenant.setup} />,
                },
            ]}
        />
    );
}
