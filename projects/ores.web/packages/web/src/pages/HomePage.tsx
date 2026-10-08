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

import { useHolds } from '../access/holds.js';
import { displayName } from '../access/names.js';
import { useQuery } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { Link } from 'react-router';
import type {
    Account,
    DeploymentOverview,
    SessionMode,
    SetupActivity,
    TenantStatus,
    TenantSummary,
    TenantType,
} from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { LinkButton, Notice, PageHeader } from '../ui/Primitives.js';
import { PaintedValue } from './TenantParts.js';
import { Tiles, type Tile } from '../ui/Tiles.js';

/**
 * Where a signed-in person lands.
 *
 * Home shows the state of the work in the person's own words, and the next
 * things they can do. What it shows is decided by the mode the session runs
 * in: the deployment's tenants for the system administrator, the tenant's own
 * screens for its administrator, and the person's work for everyone else.
 */
export interface HomePageProps {
    readonly username: string;
    readonly email: string;
    readonly tenantName: string;
    readonly partyName: string;
    /** The context the session runs in, which decides what this page shows. */
    readonly mode: SessionMode;
    /**
     * The signed-in person's own account, when the wiring has read it.
     *
     * The greeting shows the name it holds and falls back to the username,
     * for the member who may not read their own account and for every account
     * created before names were recorded.
     */
    readonly self?: Account | null;
}

export function HomePage({
    username,
    tenantName,
    partyName,
    mode,
    self,
}: HomePageProps): ReactNode {
    const name = displayName(self, username);
    if (mode === 'system-administration') {
        return <SystemHome name={name} />;
    }
    if (mode === 'tenant-administration') {
        return <TenantHome name={name} tenantName={tenantName} />;
    }
    return <PartyHome name={name} partyName={partyName} />;
}

function SystemHome({ name }: { readonly name: string }): ReactNode {
    const { t, plural } = useTranslation();
    const overview = useQuery({ queryKey: ['overview'], queryFn: api.overview });
    /*
     * The types and statuses paint the badges. They are a second read, and a
     * failure to read them leaves the words unpainted rather than the page
     * unread.
     */
    const types = useQuery({ queryKey: ['tenant-types'], queryFn: api.tenantTypes });
    const statuses = useQuery({ queryKey: ['tenant-statuses'], queryFn: api.tenantStatuses });
    const data = overview.data;

    return (
        <div className="space-y-6">
            <PageHeader
                title={t('home.welcome', { name })}
                {...(data === undefined
                    ? {}
                    : { description: plural('home.system.lead', data.totalCount) })}
                actions={
                    <div className="flex flex-wrap gap-2">
                        <LinkButton to="/tenants" variant="secondary">
                            {t('home.system.manageTenants')}
                        </LinkButton>
                        <LinkButton to="/tenants/new" variant="primary">
                            {t('home.system.addTenant')}
                        </LinkButton>
                    </div>
                }
            />
            {overview.isError && <Notice tone="error">{overview.error.message}</Notice>}
            {data !== undefined && (
                <>
                    <Health overview={data} />
                    {data.attention.length > 0 && <Attention overview={data} />}
                    <FirstTenants
                        overview={data}
                        types={types.data ?? []}
                        statuses={statuses.data ?? []}
                    />
                </>
            )}
        </div>
    );
}

function Health({ overview }: { readonly overview: DeploymentOverview }): ReactNode {
    const { t, plural } = useTranslation();
    const allClear = overview.attention.length === 0;

    return (
        <section className="card grid gap-6 p-5 sm:grid-cols-2 lg:grid-cols-5">
            <h2 className="text-sm font-semibold text-ink sm:col-span-2 lg:col-span-5">
                {t('home.system.health')}
            </h2>
            <Figure value={overview.inService} label={t('home.system.inService')} />
            <Figure value={overview.onEvaluation} label={t('home.system.onEvaluation')} />
            <Figure value={overview.settingUp} label={t('home.system.settingUp')} />
            <div className="flex items-center gap-3" role="status">
                <span
                    aria-hidden="true"
                    className={`grid h-9 w-9 shrink-0 place-items-center rounded-full border font-bold ${
                        allClear
                            ? 'border-up/50 bg-up/10 text-up'
                            : 'border-warn/50 bg-warn/10 text-warn'
                    }`}
                >
                    {allClear ? '✓' : '!'}
                </span>
                <span className="text-sm text-ink">
                    {allClear
                        ? t('home.system.allClear')
                        : plural('home.system.needAttention', overview.attention.length)}
                </span>
            </div>
            <Activity overview={overview} />
        </section>
    );
}

function Figure({ value, label }: { readonly value: number; readonly label: string }): ReactNode {
    return (
        <div className="grid gap-1">
            <span className="text-3xl font-semibold tabular-nums text-ink">{value}</span>
            <span className="text-xs text-ink-muted">{label}</span>
        </div>
    );
}

/** The engine states the activity list has words for; any other is shown as written. */
const ACTIVITY_STATES = ['completed', 'in_progress', 'failed', 'compensating', 'compensated'];

function Activity({ overview }: { readonly overview: DeploymentOverview }): ReactNode {
    const { t } = useTranslation();
    const describe = (run: SetupActivity): string =>
        ACTIVITY_STATES.includes(run.status)
            ? t(`home.system.activity.${run.status}`, {
                  tenant: run.tenantName,
                  done: run.stepsDone,
                  count: run.stepCount,
              })
            : `${run.tenantName}: ${run.status}`;

    return (
        <div className="grid content-start gap-2 border-l-2 border-up pl-3 text-sm">
            <span className="text-xs text-ink-faint">{t('home.system.recentActivity')}</span>
            {overview.activityUnavailable ? (
                <span className="text-ink-muted">{t('home.system.activityUnavailable')}</span>
            ) : overview.activity.length === 0 ? (
                <span className="text-ink-muted">{t('home.system.noActivity')}</span>
            ) : (
                overview.activity.map((run) => (
                    <Link
                        key={run.instanceId}
                        to={`/tenants/runs/${encodeURIComponent(run.instanceId)}`}
                        className="grid text-ink hover:text-accent-bright"
                    >
                        <span>{describe(run)}</span>
                        <span className="text-xs text-ink-faint">
                            {new Date(run.at).toLocaleString()}
                        </span>
                    </Link>
                ))
            )}
        </div>
    );
}

function Attention({ overview }: { readonly overview: DeploymentOverview }): ReactNode {
    const { t } = useTranslation();

    return (
        <section className="card">
            <h2 className="px-5 pt-4 pb-2 text-sm font-semibold text-ink">
                {t('home.system.attention')}
            </h2>
            <ul>
                {overview.attention.map(({ tenant, reason }) => {
                    const failed = reason === 'setup-failed' && tenant.setup !== null;
                    return (
                        <li
                            key={`${reason}-${tenant.id}`}
                            className="flex items-center gap-3 border-t border-line-subtle px-5 py-3"
                        >
                            <span
                                aria-hidden="true"
                                className={`h-2 w-2 shrink-0 rounded-full ${failed ? 'bg-down' : 'bg-warn'}`}
                            />
                            <div className="min-w-0 flex-1">
                                <div className="text-sm text-ink">{tenant.name}</div>
                                <div className="text-xs text-ink-faint">
                                    {failed && tenant.setup !== null
                                        ? t('home.system.setupFailed', {
                                              done: tenant.setup.stepsDone,
                                              count: tenant.setup.stepCount,
                                          })
                                        : t('home.system.suspended')}
                                </div>
                            </div>
                            {failed && tenant.setup !== null ? (
                                <LinkButton
                                    to={`/tenants/runs/${encodeURIComponent(tenant.setup.instanceId)}`}
                                    size="sm"
                                >
                                    {t('home.system.seeFailure')}
                                </LinkButton>
                            ) : (
                                <LinkButton
                                    to={`/tenants/${encodeURIComponent(tenant.code)}`}
                                    size="sm"
                                >
                                    {t('home.system.open')}
                                </LinkButton>
                            )}
                        </li>
                    );
                })}
            </ul>
        </section>
    );
}

function FirstTenants({
    overview,
    types,
    statuses,
}: {
    readonly overview: DeploymentOverview;
    readonly types: readonly TenantType[];
    readonly statuses: readonly TenantStatus[];
}): ReactNode {
    const { t, plural } = useTranslation();
    const typeByCode = new Map(types.map((type) => [type.code, type]));
    const statusByCode = new Map(statuses.map((status) => [status.code, status]));

    return (
        <section className="card">
            <div className="flex flex-wrap items-center justify-between gap-3 px-5 pt-4 pb-2">
                <div>
                    <h2 className="text-sm font-semibold text-ink">{t('home.system.tenants')}</h2>
                    <p className="text-xs text-ink-muted">{t('home.system.tenantsLead')}</p>
                </div>
            </div>
            {overview.tenants.length === 0 ? (
                <p className="px-5 pb-5 text-sm text-ink-muted">{t('home.system.noTenants')}</p>
            ) : (
                <>
                    <div className="overflow-x-auto">
                        <table className="w-full text-left text-sm">
                            <thead>
                                <tr className="border-b border-line text-xs text-ink-muted">
                                    <th className="px-5 py-2 font-medium">
                                        {t('home.system.name')}
                                    </th>
                                    <th className="py-2 pr-4 font-medium">
                                        {t('home.system.hostname')}
                                    </th>
                                    <th className="py-2 pr-4 font-medium">
                                        {t('home.system.type')}
                                    </th>
                                    <th className="py-2 pr-5 font-medium">
                                        {t('home.system.status')}
                                    </th>
                                </tr>
                            </thead>
                            <tbody>
                                {overview.tenants.map((tenant: TenantSummary) => (
                                    <tr key={tenant.id} className="border-b border-line-subtle">
                                        <td className="px-5 py-2">
                                            <Link
                                                to={`/tenants/${encodeURIComponent(tenant.code)}`}
                                                className="text-ink hover:text-accent-bright"
                                            >
                                                {tenant.name}
                                            </Link>
                                        </td>
                                        <td className="py-2 pr-4 text-ink-muted">
                                            {tenant.hostname}
                                        </td>
                                        <td className="py-2 pr-4">
                                            <PaintedValue
                                                value={tenant.type}
                                                known={typeByCode.get(tenant.type)}
                                            />
                                        </td>
                                        <td className="py-2 pr-5">
                                            <PaintedValue
                                                value={tenant.status}
                                                known={statusByCode.get(tenant.status)}
                                            />
                                        </td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    </div>
                    <div className="flex flex-wrap items-center justify-between gap-3 px-5 py-3 text-xs text-ink-muted">
                        <span className="tabular-nums">
                            {plural('home.system.showing', overview.totalCount, {
                                shown: overview.tenants.length,
                            })}
                        </span>
                        <LinkButton to="/tenants" size="sm">
                            {t('home.system.seeAll')}
                        </LinkButton>
                    </div>
                </>
            )}
        </section>
    );
}

function TenantHome({
    name,
    tenantName,
}: {
    readonly name: string;
    readonly tenantName: string;
}): ReactNode {
    const { t } = useTranslation();
    const tiles: Tile[] = [
        { title: t('home.tenant.parties'), body: t('home.tenant.partiesBody'), to: '/parties' },
        { title: t('home.tenant.people'), body: t('home.tenant.peopleBody'), to: '/people' },
        { title: t('home.tenant.roles'), body: t('home.tenant.rolesBody'), to: '/roles' },
        { title: t('home.party.refdata'), body: t('home.party.refdataBody'), to: '/refdata' },
        {
            title: t('home.tenant.newParty'),
            body: t('home.tenant.newPartyBody'),
            to: '/parties/new',
        },
        { title: t('home.tenant.rescue'), body: t('home.tenant.rescueBody'), to: '/rescue' },
        { title: t('home.tenant.audit'), body: t('home.tenant.auditBody'), to: '/audit' },
        {
            title: t('home.tenant.versions'),
            body: t('home.tenant.versionsBody'),
            to: '/operations/versions',
        },
        { title: t('home.tenant.security'), body: t('home.tenant.securityBody'), to: '/security' },
    ];

    return (
        <div className="space-y-6">
            <PageHeader title={tenantName} description={t('home.tenant.lead', { name })} />
            <Tiles tiles={tiles} />
        </div>
    );
}

function PartyHome({
    name,
    partyName,
}: {
    readonly name: string;
    readonly partyName: string;
}): ReactNode {
    const { t } = useTranslation();
    const holds = useHolds();
    const coming: Tile[] = (['marketdata', 'trading', 'reporting'] as const).map((area) => ({
        title: t(`home.party.${area}`),
        body: t(`home.party.${area}Body`),
    }));

    return (
        <div className="space-y-6">
            <PageHeader
                title={t('home.welcome', { name })}
                description={t('home.party.lead', { party: partyName })}
            />
            <Tiles
                tiles={[
                    ...(holds('iam::accounts:read')
                        ? [
                              {
                                  title: t('home.tenant.people'),
                                  body: t('home.tenant.peopleBody'),
                                  to: '/people',
                              },
                          ]
                        : []),
                    {
                        title: t('home.party.refdata'),
                        body: t('home.party.refdataBody'),
                        to: '/refdata',
                    },
                    {
                        title: t('home.party.security'),
                        body: t('home.party.securityBody'),
                        to: '/security',
                    },
                    {
                        title: t('home.tenant.access'),
                        body: t('home.tenant.accessBody'),
                        to: '/access',
                    },
                    {
                        title: t('home.tenant.versions'),
                        body: t('home.tenant.versionsBody'),
                        to: '/operations/versions',
                    },
                ]}
            />
            <Notice tone="info">{t('home.party.note')}</Notice>
            <Tiles tiles={coming} later={t('home.party.comingLater')} />
        </div>
    );
}
