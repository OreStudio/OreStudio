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
import { useState, type ReactNode } from 'react';
import { Link, Navigate, useNavigate, useParams } from 'react-router';
import {
    SYSTEM_TENANT_ID,
    type Account,
    type TenantDetailResponse,
    type TenantParty,
} from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { ApiFailure } from '../api/transport.js';
import { useTranslation } from '../i18n/Provider.js';
import { HistoryPanel } from '../refdata/HistoryPanel.js';
import { RecordTable, type ListSource } from '../refdata/RecordList.js';
import { RecordDetails, RecordHeader } from '../refdata/records.js';
import { Avatar, imageUrl } from '../ui/Images.js';
import { Notice, Tag } from '../ui/Primitives.js';
import { useTabs } from '../ui/Tabs.js';
import { partyColumns } from './PartiesPage.js';
import { displayName } from '../access/names.js';
import { RemoveTenantDialog } from './RemoveTenantDialog.js';
import { PaintedValue, SetupCell } from './TenantParts.js';
import { tenantPath } from './TenantsPage.js';

/**
 * One tenant, opened from the list: what it is and how its setup went, then
 * its parties and its people as tabs. The tenant's own fields are read only
 * here; its parties and people are changed by its own administrators, and are
 * read inside the tenant for each read. The one write is deleting the tenant,
 * which asks first and is not offered for the system tenant.
 */
export function TenantPage(): ReactNode {
    const { t } = useTranslation();
    const { code = '' } = useParams();
    const read = useQuery({
        queryKey: ['tenant', code],
        queryFn: () => api.tenant(code),
        retry: false,
    });

    if (read.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (read.isError) {
        return read.error instanceof ApiFailure && read.error.status === 404 ? (
            <Navigate to={tenantPath()} replace />
        ) : (
            <Notice tone="error">{read.error.message}</Notice>
        );
    }
    return <TenantBody detail={read.data} />;
}

function TenantBody({ detail }: { readonly detail: TenantDetailResponse }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const { tenant } = detail;
    const statuses = useQuery({ queryKey: ['tenant-statuses'], queryFn: api.tenantStatuses });
    const types = useQuery({ queryKey: ['tenant-types'], queryFn: api.tenantTypes });
    const [removing, setRemoving] = useState(false);
    const { tab, bar } = useTabs({
        label: tenant.name,
        tabs: ['details', 'parties', 'people', 'history'],
        titleOf: (candidate) => t(`tenants.detail.tab.${candidate}`),
    });
    const text = (value: string): ReactNode =>
        value === '' ? <span className="text-ink-faint">—</span> : value;
    const mono = (value: string): ReactNode => (
        <span className="font-mono text-xs">{text(value)}</span>
    );

    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[
                    { label: t('shell.menu.home'), to: '/' },
                    { label: t('tenants.title'), to: tenantPath() },
                    { label: tenant.code },
                ]}
                title={tenant.name}
                recordKey={tenant.code}
                version={tenant.version}
                access={false}
                onDelete={tenant.id === SYSTEM_TENANT_ID ? undefined : () => setRemoving(true)}
            />
            {bar}
            {tab === 'details' && (
                <div className="space-y-4">
                    {tenant.id !== SYSTEM_TENANT_ID && (
                        <Notice tone="info">{t('tenants.detail.viewOnlyHint')}</Notice>
                    )}
                    <RecordDetails
                        specs={[]}
                        row={{
                            version: tenant.version,
                            modified_by: tenant.modifiedBy,
                            performed_by: tenant.performedBy,
                            recorded_at: tenant.recordedAt,
                            change_reason_code: tenant.changeReasonCode,
                            change_commentary: tenant.changeCommentary,
                        }}
                        extra={[
                            [t('tenants.code'), mono(tenant.code)],
                            [t('tenants.name'), text(tenant.name)],
                            [t('tenants.hostname'), mono(tenant.hostname)],
                            [
                                t('tenants.type'),
                                <PaintedValue
                                    key="type"
                                    value={tenant.type}
                                    known={(types.data ?? []).find(
                                        (row) => row.code === tenant.type,
                                    )}
                                />,
                            ],
                            [
                                t('tenants.status'),
                                <PaintedValue
                                    key="status"
                                    value={tenant.status}
                                    known={(statuses.data ?? []).find(
                                        (row) => row.code === tenant.status,
                                    )}
                                />,
                            ],
                            [
                                t('tenants.detail.registrationDefault'),
                                tenant.registrationDefault
                                    ? t('tenants.detail.yes')
                                    : t('tenants.detail.no'),
                            ],
                            [t('tenants.detail.description'), text(tenant.description)],
                        ]}
                    />
                    <section className="rounded-md border border-line bg-surface-raised p-4">
                        <h2 className="mb-3 text-sm font-semibold">{t('tenants.setup')}</h2>
                        <SetupPanel detail={detail} />
                    </section>
                </div>
            )}
            {tab === 'parties' && <TenantParties code={tenant.code} />}
            {tab === 'people' && <TenantPeople code={tenant.code} />}
            {tab === 'history' && (
                <HistoryPanel entityType="ores.iam.tenant" entityId={tenant.code} />
            )}
            {removing && (
                <RemoveTenantDialog
                    tenant={tenant}
                    onClose={() => setRemoving(false)}
                    onRemoved={() => void navigate(tenantPath())}
                />
            )}
        </div>
    );
}

/** The tenant's parties, read inside the tenant, each named with the party it belongs to. */
function TenantParties({ code }: { readonly code: string }): ReactNode {
    const { t } = useTranslation();
    const source: ListSource<TenantParty> = {
        key: 'tenant-parties',
        scope: code,
        read: async (page) => {
            const read = await api.tenantParties(code, {
                offset: page.offset,
                limit: page.limit,
                search: page.search,
                sort: page.sort,
                descending: page.descending,
            });
            return { rows: read.parties, total: read.totalCount };
        },
        search: true,
        sortable: [
            'short_code',
            'full_name',
            'party_category',
            'party_type',
            'status',
            'business_center_code',
        ],
        mayAdd: false,
        watches: [{ component: 'refdata', entity: 'parties' }],
    };
    return (
        <RecordTable
            source={source}
            plural={t('parties.title')}
            keyOf={(party) => party.id}
            columns={partyColumns(t, code)}
        />
    );
}

/** The people who can sign in to the tenant; each opens on their own page. */
function TenantPeople({ code }: { readonly code: string }): ReactNode {
    const { t } = useTranslation();
    const source: ListSource<Account> = {
        key: 'tenant-people',
        scope: code,
        read: async (page) => {
            const read = await api.tenantPeople(code, { offset: page.offset, limit: page.limit });
            return { rows: read.accounts, total: read.totalCount };
        },
        search: false,
        sortable: [],
        mayAdd: false,
        watches: [{ component: 'iam', entity: 'accounts' }],
    };
    return (
        <RecordTable
            source={source}
            plural={t('tenants.detail.tab.people')}
            pathOf={(account) =>
                `${tenantPath(code)}/people/${encodeURIComponent(account.username)}`
            }
            columns={[
                {
                    id: 'person',
                    header: t('tenants.detail.person'),
                    cell: (account) => (
                        <span className="flex items-center gap-3">
                            <Avatar
                                name={displayName(account, account.username)}
                                src={
                                    account.imageId === null
                                        ? null
                                        : imageUrl(account.imageId, code)
                                }
                            />
                            {displayName(account, account.username)}
                        </span>
                    ),
                },
                {
                    id: 'username',
                    header: t('tenants.detail.username'),
                    cell: (account) => account.username,
                    mono: true,
                },
                {
                    id: 'email',
                    header: t('tenants.detail.email'),
                    cell: (account) => account.email,
                },
                {
                    id: 'kind',
                    header: t('signIns.kind'),
                    cell: (account) =>
                        account.accountType === 'user' ? (
                            <span className="text-ink-muted">{t('signIns.person')}</span>
                        ) : (
                            <Tag tone="accent">{t('signIns.service')}</Tag>
                        ),
                },
            ]}
        />
    );
}

/**
 * How the tenant's setup went. An unfinished run links to its page, which is
 * where it is watched or resumed, and a failed one shows the error the engine
 * stopped on.
 */
function SetupPanel({ detail }: { readonly detail: TenantDetailResponse }): ReactNode {
    const { t } = useTranslation();
    const setup = detail.tenant.setup;
    if (detail.setupUnavailable) {
        return <Notice tone="warn">{t('tenants.detail.setupUnavailable')}</Notice>;
    }
    if (setup === null) {
        return <p className="text-sm text-ink-muted">{t('tenants.detail.noRun')}</p>;
    }
    if (setup.status === 'completed') {
        return <p className="text-sm text-ink-muted">{t('tenants.detail.runCompleted')}</p>;
    }
    return (
        <div className="flex flex-wrap items-center gap-4 text-sm">
            <SetupCell setup={setup} />
            {setup.error !== '' && <span className="text-ink-muted">{setup.error}</span>}
            <Link
                to={`/tenants/runs/${encodeURIComponent(setup.instanceId)}`}
                className="text-accent-bright underline-offset-2 hover:underline"
            >
                {t('tenants.resumeSetup')}
            </Link>
        </div>
    );
}
