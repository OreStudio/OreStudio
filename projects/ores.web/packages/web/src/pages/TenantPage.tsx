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
import { Link, useParams } from 'react-router';
import type { TenantDetailResponse } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { ApiFailure } from '../api/transport.js';
import { PaintedValue, SetupCell } from './TenantParts.js';
import { Button, Detail, LinkButton, Notice, PageHeader } from '../ui/Primitives.js';

/**
 * One tenant, opened from the roster: what it is and how its setup went.
 *
 * The address names the tenant's code, its stable name in a request, so a link
 * opens the same tenant again. The screen writes nothing. A run the server
 * could not read empties its own panel and says so. The tenant's own data,
 * its parties among it, is read after entering the tenant.
 */
export interface TenantPageProps {
    /**
     * Enters the tenant, reading only. The wiring owns the session, so the
     * screen asks for the entry rather than making it.
     */
    readonly onEnterTenant: (tenantId: string) => Promise<void>;
}

export function TenantPage({ onEnterTenant }: TenantPageProps): ReactNode {
    const { t } = useTranslation();
    const [entering, setEntering] = useState(false);
    const [entryRefused, setEntryRefused] = useState<string | null>(null);
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
            <PageHeader
                title={tenant.name}
                description={t('tenants.detail.lead')}
                actions={
                    <>
                        <Button
                            disabled={entering}
                            onClick={() => {
                                setEntering(true);
                                setEntryRefused(null);
                                onEnterTenant(tenant.id).catch((error: unknown) => {
                                    setEntering(false);
                                    setEntryRefused(
                                        error instanceof Error ? error.message : String(error),
                                    );
                                });
                            }}
                        >
                            {t('tenants.detail.enter')}
                        </Button>
                        {back}
                    </>
                }
            />
            {entryRefused !== null && <Notice tone="error">{entryRefused}</Notice>}

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

            <Notice tone="info">{t('tenants.detail.readInside')}</Notice>
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
