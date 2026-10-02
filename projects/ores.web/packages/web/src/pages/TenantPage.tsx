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
import { Link, useParams } from 'react-router';
import type { TenantDetailResponse, TenantParty } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { ApiFailure } from '../api/transport.js';
import { PaintedValue, SetupCell } from './TenantParts.js';
import { Detail, LinkButton, Notice, PageHeader } from '../ui/Primitives.js';

/**
 * One tenant, opened from the roster: what it is, how its setup went, and the
 * parties it holds.
 *
 * The address names the tenant's code, its stable name in a request, so a link
 * opens the same tenant again. The screen writes nothing. A run or a party list
 * the server could not read empties its own panel and says so; the tenant is
 * still the registry's answer.
 */
export function TenantPage(): ReactNode {
    const { t } = useTranslation();
    const { code = '' } = useParams();
    const read = useQuery({
        queryKey: ['tenant', code],
        queryFn: () => api.tenant(code),
        retry: false,
    });
    const statuses = useQuery({ queryKey: ['tenant-statuses'], queryFn: api.tenantStatuses });
    const types = useQuery({ queryKey: ['tenant-types'], queryFn: api.tenantTypes });

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
            <PageHeader title={tenant.name} description={t('tenants.detail.lead')} actions={back} />

            <section className="card mb-6 p-6">
                <h2 className="mb-4 text-sm font-semibold">{t('tenants.detail.details')}</h2>
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
                    <Detail label={t('tenants.detail.description')} value={tenant.description} />
                </dl>
                <h3 className="mb-3 mt-6 text-xs font-semibold text-ink-muted">
                    {t('tenants.detail.provenance')}
                </h3>
                <dl className="grid grid-cols-1 gap-4 sm:grid-cols-3">
                    <Detail label={t('tenants.detail.version')} value={String(tenant.version)} />
                    <Detail label={t('tenants.detail.modifiedBy')} value={tenant.modifiedBy} />
                    <Detail label={t('tenants.detail.performedBy')} value={tenant.performedBy} />
                    <Detail
                        label={t('tenants.detail.changeReason')}
                        value={tenant.changeReasonCode}
                        mono
                    />
                    <Detail
                        label={t('tenants.detail.changeCommentary')}
                        value={tenant.changeCommentary}
                    />
                    <Detail label={t('tenants.detail.recordedAt')} value={tenant.recordedAt} />
                </dl>
            </section>

            <section className="card mb-6 p-6">
                <h2 className="mb-4 text-sm font-semibold">{t('tenants.setup')}</h2>
                <SetupPanel detail={read.data} />
            </section>

            <section className="card p-6">
                <h2 className="mb-4 text-sm font-semibold">{t('tenants.detail.parties')}</h2>
                <PartiesPanel detail={read.data} />
            </section>
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

/**
 * The parties the tenant holds: its system party first, then the others by
 * name. A tenant with only its system party has no business party yet.
 */
function PartiesPanel({ detail }: { readonly detail: TenantDetailResponse }): ReactNode {
    const { t, plural } = useTranslation();
    if (detail.partiesUnavailable) {
        return <Notice tone="warn">{t('tenants.detail.partiesUnavailable')}</Notice>;
    }
    const business = detail.parties.filter((party) => party.category !== 'System');
    return (
        <div>
            {business.length === 0 && (
                <p className="mb-4 text-sm text-ink-muted">{t('tenants.detail.onlySystemParty')}</p>
            )}
            <div className="overflow-x-auto">
                <table className="w-full text-left text-sm">
                    <thead>
                        <tr className="border-b border-line text-xs text-ink-muted">
                            <th className="py-2 pr-4 font-medium">{t('tenants.code')}</th>
                            <th className="py-2 pr-4 font-medium">{t('tenants.name')}</th>
                            <th className="py-2 pr-4 font-medium">
                                {t('tenants.detail.category')}
                            </th>
                            <th className="py-2 pr-4 font-medium">{t('tenants.type')}</th>
                            <th className="py-2 pr-4 font-medium">{t('tenants.status')}</th>
                            <th className="py-2 font-medium">{t('tenants.detail.parent')}</th>
                        </tr>
                    </thead>
                    <tbody>
                        {detail.parties.map((party: TenantParty) => (
                            <tr key={party.id} className="border-b border-line-subtle">
                                <td className="py-2.5 pr-4 font-mono text-xs">{party.code}</td>
                                <td className="py-2.5 pr-4">{party.name}</td>
                                <td className="py-2.5 pr-4">{party.category}</td>
                                <td className="py-2.5 pr-4">{party.type}</td>
                                <td className="py-2.5 pr-4">{party.status}</td>
                                <td className="py-2.5">{party.parentName ?? ''}</td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </div>
            <p className="mt-4 text-sm text-ink-muted">
                {plural('tenants.detail.partyCount', detail.partyCount)}
            </p>
        </div>
    );
}
