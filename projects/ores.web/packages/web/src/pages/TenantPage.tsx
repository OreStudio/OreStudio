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

import { keepPreviousData, useQuery } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { Link, useNavigate, useParams, useSearchParams } from 'react-router';
import { SYSTEM_TENANT_ID, type TenantDetailResponse } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { ApiFailure } from '../api/transport.js';
import { PaintedValue, SetupCell } from './TenantParts.js';
import { FlaggedCode } from '../images/flags.js';
import { Avatar, imageUrl } from '../ui/Images.js';
import { DEFAULT_PAGE_SIZE, Pager, pageBounds } from '../ui/Pager.js';
import { Button, Detail, LinkButton, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { RemoveTenantDialog } from './RemoveTenantDialog.js';

/** The tabs of a tenant's screen, in the order they are drawn. */
const TABS = ['overview', 'parties', 'people'] as const;
type Tab = (typeof TABS)[number];

/**
 * One tenant, opened from the roster: what it is, how its setup went, and its
 * parties and people.
 *
 * The address names the tenant's code, its stable name in a request, and the
 * tab, so a link opens the same tenant at the same place again. The one write
 * is removing the tenant, which asks first. The parties and people are the tenant's own data, which the
 * server reads inside the tenant for each read; the person opens a tab, and
 * never enters or leaves anything.
 */
export function TenantPage(): ReactNode {
    const { t } = useTranslation();
    const { code = '' } = useParams();
    const [search, setSearch] = useSearchParams();
    const requested = search.get('tab');
    const tab: Tab = TABS.find((candidate) => candidate === requested) ?? 'overview';
    const read = useQuery({
        queryKey: ['tenant', code],
        queryFn: () => api.tenant(code),
        retry: false,
    });
    const statuses = useQuery({ queryKey: ['tenant-statuses'], queryFn: api.tenantStatuses });
    const types = useQuery({ queryKey: ['tenant-types'], queryFn: api.tenantTypes });
    const navigate = useNavigate();
    const [removing, setRemoving] = useState(false);

    const back = (
        <LinkButton to="/tenants" variant="secondary">
            {t('tenants.detail.back')}
        </LinkButton>
    );

    if (read.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }

    if (read.isError) {
        const missing = read.error instanceof ApiFailure && read.error.status === 404;
        const reason = read.error instanceof Error ? read.error.message : String(read.error);
        return (
            <div>
                <PageHeader
                    title={missing ? t('tenants.detail.notFound') : t('tenants.detail.failed')}
                    actions={back}
                />
                {!missing && <Notice tone="error">{reason}</Notice>}
            </div>
        );
    }

    const { tenant } = read.data;
    const status = (statuses.data ?? []).find((row) => row.code === tenant.status);
    const type = (types.data ?? []).find((row) => row.code === tenant.type);

    return (
        <div>
            <PageHeader
                title={tenant.name}
                description={t('tenants.detail.lead')}
                actions={
                    <>
                        {tenant.id !== SYSTEM_TENANT_ID && (
                            <Button variant="danger" onClick={() => setRemoving(true)}>
                                {t('tenants.removeAction')}
                            </Button>
                        )}
                        {back}
                    </>
                }
            />
            {removing && (
                <RemoveTenantDialog
                    tenant={tenant}
                    onClose={() => setRemoving(false)}
                    onRemoved={() => void navigate('/tenants')}
                />
            )}
            <div className="mb-6 flex flex-wrap items-center justify-between gap-3 border-b border-line">
                <div role="tablist" aria-label={tenant.name} className="flex gap-1">
                    {TABS.map((candidate) => (
                        <button
                            key={candidate}
                            type="button"
                            role="tab"
                            aria-selected={tab === candidate}
                            onClick={() =>
                                setSearch(candidate === 'overview' ? {} : { tab: candidate })
                            }
                            className={`-mb-px border-b-2 px-3 py-2 text-sm ${
                                tab === candidate
                                    ? 'border-accent text-ink'
                                    : 'border-transparent text-ink-muted hover:text-ink'
                            }`}
                        >
                            {t(`tenants.detail.tab.${candidate}`)}
                        </button>
                    ))}
                </div>
                {tenant.id !== SYSTEM_TENANT_ID && (
                    <span title={t('tenants.detail.viewOnlyHint')}>
                        <Tag tone="muted">{t('tenants.detail.viewOnly')}</Tag>
                    </span>
                )}
            </div>

            {tab === 'parties' && <TenantParties code={tenant.code} />}
            {tab === 'people' && <TenantPeople code={tenant.code} />}
            {tab === 'overview' && (
                <>
                    <section className="card mb-6 p-6">
                        <h2 className="mb-4 text-sm font-semibold">
                            {t('tenants.detail.details')}
                        </h2>
                        <dl className="grid grid-cols-1 gap-4 sm:grid-cols-3">
                            <Detail label={t('tenants.code')} value={tenant.code} mono />
                            <Detail label={t('tenants.name')} value={tenant.name} />
                            <Detail label={t('tenants.hostname')} value={tenant.hostname} mono />
                            <div>
                                <dt className="text-[11px] uppercase tracking-wide text-ink-faint">
                                    {t('tenants.type')}
                                </dt>
                                <dd className="mt-0.5">
                                    <PaintedValue value={tenant.type} known={type} />
                                </dd>
                            </div>
                            <div>
                                <dt className="text-[11px] uppercase tracking-wide text-ink-faint">
                                    {t('tenants.status')}
                                </dt>
                                <dd className="mt-0.5">
                                    <PaintedValue value={tenant.status} known={status} />
                                </dd>
                            </div>
                            <Detail
                                label={t('tenants.detail.registrationDefault')}
                                value={
                                    tenant.registrationDefault
                                        ? t('tenants.detail.yes')
                                        : t('tenants.detail.no')
                                }
                            />
                            <Detail
                                label={t('tenants.detail.description')}
                                value={tenant.description}
                            />
                        </dl>
                        <h3 className="mb-3 mt-6 text-xs font-semibold text-ink-muted">
                            {t('tenants.detail.provenance')}
                        </h3>
                        <dl className="grid grid-cols-1 gap-4 sm:grid-cols-3">
                            <Detail
                                label={t('tenants.detail.version')}
                                value={String(tenant.version)}
                            />
                            <Detail
                                label={t('tenants.detail.modifiedBy')}
                                value={tenant.modifiedBy}
                            />
                            <Detail
                                label={t('tenants.detail.performedBy')}
                                value={tenant.performedBy}
                            />
                            <Detail
                                label={t('tenants.detail.changeReason')}
                                value={tenant.changeReasonCode}
                                mono
                            />
                            <Detail
                                label={t('tenants.detail.changeCommentary')}
                                value={tenant.changeCommentary}
                            />
                            <Detail
                                label={t('tenants.detail.recordedAt')}
                                value={tenant.recordedAt}
                            />
                        </dl>
                    </section>

                    <section className="card mb-6 p-6">
                        <h2 className="mb-4 text-sm font-semibold">{t('tenants.setup')}</h2>
                        <SetupPanel detail={read.data} />
                    </section>
                </>
            )}
        </div>
    );
}

/** One page of the tenant's parties, each named with the party it belongs to. */
function TenantParties({ code }: { readonly code: string }): ReactNode {
    const { t, plural } = useTranslation();
    const [offset, setOffset] = useState(0);
    const read = useQuery({
        queryKey: ['tenant-parties', code, offset],
        queryFn: () => api.tenantParties(code, { offset, limit: DEFAULT_PAGE_SIZE }),
        placeholderData: keepPreviousData,
    });

    if (read.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (read.isError) {
        return <Notice tone="error">{read.error.message}</Notice>;
    }
    const { parties, totalCount } = read.data;
    if (totalCount === 0) {
        return <p className="text-sm text-ink-muted">{t('tenants.detail.noParties')}</p>;
    }
    const { first, last } = pageBounds(offset, parties.length);
    return (
        <div>
            <div className="overflow-x-auto">
                <table className="w-full text-left text-sm">
                    <thead>
                        <tr className="border-b border-line text-xs text-ink-muted">
                            <th className="py-2 pr-4 font-medium">{t('parties.name')}</th>
                            <th className="py-2 pr-4 font-medium">{t('parties.code')}</th>
                            <th className="py-2 pr-4 font-medium">{t('parties.businessCentre')}</th>
                            <th className="py-2 pr-4 font-medium">{t('parties.parent')}</th>
                        </tr>
                    </thead>
                    <tbody>
                        {parties.map((party) => (
                            <tr key={party.id} className="border-b border-line-subtle">
                                <td className="py-2 pr-4 text-ink">{party.name}</td>
                                <td className="py-2 pr-4 font-mono text-xs text-ink-muted">
                                    {party.code}
                                </td>
                                <td className="py-2 pr-4 text-ink-muted">
                                    <FlaggedCode
                                        code={party.businessCentreCode}
                                        src={
                                            party.flagImageId === null
                                                ? null
                                                : imageUrl(party.flagImageId, code)
                                        }
                                    />
                                </td>
                                <td className="py-2 pr-4">
                                    {party.parentId === null ? (
                                        <span className="text-ink-faint">
                                            {t('tenants.detail.topOfGroup')}
                                        </span>
                                    ) : party.parentName !== null ? (
                                        party.parentName
                                    ) : (
                                        <span className="text-ink-faint">
                                            {t('parties.parentElsewhere')}
                                        </span>
                                    )}
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </div>
            <Pager
                offset={offset}
                shown={parties.length}
                total={totalCount}
                pageSize={DEFAULT_PAGE_SIZE}
                showing={plural('parties.showing', totalCount, { first, last })}
                onMove={setOffset}
            />
        </div>
    );
}

/** One page of the people who can sign in to the tenant. */
function TenantPeople({ code }: { readonly code: string }): ReactNode {
    const { t, plural } = useTranslation();
    const navigate = useNavigate();
    const [offset, setOffset] = useState(0);
    const read = useQuery({
        queryKey: ['tenant-people', code, offset],
        queryFn: () => api.tenantPeople(code, { offset, limit: DEFAULT_PAGE_SIZE }),
        placeholderData: keepPreviousData,
    });

    if (read.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (read.isError) {
        return <Notice tone="error">{read.error.message}</Notice>;
    }
    const { accounts, totalCount } = read.data;
    if (totalCount === 0) {
        return <p className="text-sm text-ink-muted">{t('tenants.detail.noPeople')}</p>;
    }
    const { first, last } = pageBounds(offset, accounts.length);
    return (
        <div>
            <div className="overflow-x-auto">
                <table className="w-full text-left text-sm">
                    <thead>
                        <tr className="border-b border-line text-xs text-ink-muted">
                            <th className="py-2 pr-4 font-medium">{t('tenants.detail.person')}</th>
                            <th className="py-2 pr-4 font-medium">
                                {t('tenants.detail.username')}
                            </th>
                            <th className="py-2 pr-4 font-medium">{t('tenants.detail.email')}</th>
                            <th className="py-2 pr-4 font-medium">{t('signIns.kind')}</th>
                        </tr>
                    </thead>
                    <tbody>
                        {accounts.map((account) => (
                            <tr
                                key={account.id}
                                className="cursor-pointer border-b border-line-subtle hover:bg-surface-hover"
                                onClick={() =>
                                    void navigate(
                                        `/tenants/${encodeURIComponent(code)}/people/${encodeURIComponent(account.username)}`,
                                    )
                                }
                            >
                                <td className="py-2 pr-4 text-ink">
                                    <span className="flex items-center gap-3">
                                        <Avatar
                                            name={
                                                account.fullName === ''
                                                    ? account.username
                                                    : account.fullName
                                            }
                                            src={
                                                account.imageId === null
                                                    ? null
                                                    : imageUrl(account.imageId, code)
                                            }
                                        />
                                        {account.fullName === ''
                                            ? account.username
                                            : account.fullName}
                                    </span>
                                </td>
                                <td className="py-2 pr-4 font-mono text-xs text-ink-muted">
                                    {account.username}
                                </td>
                                <td className="py-2 pr-4 text-ink-muted">{account.email}</td>
                                <td className="py-2 pr-4">
                                    {account.accountType === 'user' ? (
                                        <span className="text-ink-muted">
                                            {t('signIns.person')}
                                        </span>
                                    ) : (
                                        <Tag tone="accent">{t('signIns.service')}</Tag>
                                    )}
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </div>
            <Pager
                offset={offset}
                shown={accounts.length}
                total={totalCount}
                pageSize={DEFAULT_PAGE_SIZE}
                showing={plural('tenants.detail.showingPeople', totalCount, { first, last })}
                onMove={setOffset}
            />
        </div>
    );
}

/**
 * How the tenant's setup went.
 *
 * An unfinished run links to its page, which is where it is watched or
 * retried, and a failed one shows the error the engine stopped on.
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
