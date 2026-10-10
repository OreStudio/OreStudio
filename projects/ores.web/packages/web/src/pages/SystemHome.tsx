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

import type { ReactNode } from 'react';
import { Link } from 'react-router';
import { useQuery } from '@tanstack/react-query';
import type {
    BusView,
    DeploymentOverview,
    GridView,
    SetupActivity,
    TenantStatus,
    TenantSummary,
    TenantType,
} from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { BUS_QUERY_KEY } from '../operations/BusPage.js';
import { GRID_QUERY_KEY } from '../operations/GridPage.js';
import {
    DEFAULT_HEALTH_RANGE,
    InstallationFigures,
    useInstallationHealth,
} from '../operations/InstallationHealth.js';
import { LinkButton, Notice, PageHeader } from '../ui/Primitives.js';
import { Tiles, type Tile } from '../ui/Tiles.js';
import { useTabs } from '../ui/Tabs.js';
import { PaintedValue } from './TenantParts.js';

/**
 * The system administrator's home.
 *
 * Three tabs. The Dashboard answers how the installation is doing, in four
 * panels that all state their status in the same words. The Active modules tab
 * holds the places the person works in. The Upcoming modules tab holds the
 * journeys that are designed and have a backend but no screen yet.
 */

/** The range the bus is read over on the dashboard. */
const BUS_DASHBOARD_RANGE = '15m';

const TABS = ['dashboard', 'active', 'upcoming'] as const;

export function SystemHome({ name }: { readonly name: string }): ReactNode {
    const { t } = useTranslation();
    const { tab, bar } = useTabs({
        label: t('home.system.tabs.label'),
        tabs: TABS,
        titleOf: (candidate) => t(`home.system.tabs.${candidate}`),
    });

    return (
        <div className="space-y-6">
            <PageHeader title={t('home.welcome', { name })} />
            {bar}
            {tab === 'dashboard' && <Dashboard />}
            {tab === 'active' && <ActiveModules />}
            {tab === 'upcoming' && <UpcomingModules />}
        </div>
    );
}

/** How a panel states its status: the tone paints the mark, the words are the panel's. */
type StatusTone = 'ok' | 'attention' | 'none';

interface PanelStatus {
    readonly tone: StatusTone;
    readonly text: string;
}

const MARK: Readonly<Record<StatusTone, { readonly glyph: string; readonly classes: string }>> = {
    ok: { glyph: '✓', classes: 'border-up/50 bg-up/10 text-up' },
    attention: { glyph: '!', classes: 'border-warn/50 bg-warn/10 text-warn' },
    none: { glyph: '–', classes: 'border-line bg-surface text-ink-faint' },
};

/**
 * One dashboard panel: a title that leads to its screen, one line of status,
 * and the figures behind it.
 *
 * Every panel has the same three parts in the same order, so the four read as
 * one family: the first thing a person reads in each is whether it needs them.
 */
function Panel({
    title,
    to,
    status,
    children,
}: {
    readonly title: string;
    readonly to: string;
    readonly status: PanelStatus;
    readonly children: ReactNode;
}): ReactNode {
    const { t } = useTranslation();
    const mark = MARK[status.tone];

    return (
        <section className="card space-y-4 p-5">
            <header className="flex flex-wrap items-center justify-between gap-2">
                <h2 className="text-sm font-semibold text-ink">{title}</h2>
                <LinkButton to={to} size="sm">
                    {t('home.system.panels.open')}
                </LinkButton>
            </header>
            <div className="flex items-center gap-3">
                <span
                    aria-hidden="true"
                    className={`grid h-9 w-9 shrink-0 place-items-center rounded-full border font-bold ${mark.classes}`}
                >
                    {mark.glyph}
                </span>
                <span className="text-sm text-ink">{status.text}</span>
            </div>
            {children}
        </section>
    );
}

function Figure({ value, label }: { readonly value: string; readonly label: string }): ReactNode {
    return (
        <div className="grid gap-1">
            <span className="text-3xl font-semibold tabular-nums text-ink">{value}</span>
            <span className="text-xs text-ink-muted">{label}</span>
        </div>
    );
}

function Dashboard(): ReactNode {
    return (
        <div className="space-y-4">
            <TenantsPanel />
            <div className="grid gap-4 lg:grid-cols-3">
                <ServicesPanel />
                <GridPanel />
                <QueuePanel />
            </div>
        </div>
    );
}

function TenantsPanel(): ReactNode {
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

    const status: PanelStatus =
        data === undefined || overview.isError
            ? { tone: 'none', text: t('home.system.panels.unread') }
            : data.attention.length === 0
              ? { tone: 'ok', text: t('home.system.allClear') }
              : {
                    tone: 'attention',
                    text: plural('home.system.needAttention', data.attention.length),
                };

    return (
        <Panel title={t('home.system.tenants')} to="/tenants" status={status}>
            {overview.isError && <Notice tone="error">{overview.error.message}</Notice>}
            {data !== undefined && (
                <>
                    <div className="grid gap-6 sm:grid-cols-2 lg:grid-cols-4">
                        <Figure value={String(data.inService)} label={t('home.system.inService')} />
                        <Figure
                            value={String(data.onEvaluation)}
                            label={t('home.system.onEvaluation')}
                        />
                        <Figure value={String(data.settingUp)} label={t('home.system.settingUp')} />
                        <Activity overview={data} />
                    </div>
                    {data.attention.length > 0 && <Attention overview={data} />}
                    <TenantTable
                        overview={data}
                        types={types.data ?? []}
                        statuses={statuses.data ?? []}
                    />
                </>
            )}
        </Panel>
    );
}

function ServicesPanel(): ReactNode {
    const { t, plural } = useTranslation();
    const { roster, rosterFailed } = useInstallationHealth(DEFAULT_HEALTH_RANGE);
    const behind = roster === undefined ? 0 : roster.lost + roster.missing;

    const status: PanelStatus =
        roster === undefined
            ? {
                  tone: 'none',
                  text: t(rosterFailed ? 'home.system.panels.unread' : 'common.loading'),
              }
            : behind === 0
              ? { tone: 'ok', text: t('home.system.allClear') }
              : {
                    tone: 'attention',
                    text: plural('home.system.panels.services.needAttention', behind),
                };

    return (
        <Panel
            title={t('home.system.panels.services.title')}
            to="/operations/services"
            status={status}
        >
            <InstallationFigures range={DEFAULT_HEALTH_RANGE} compact />
        </Panel>
    );
}

/** The nodes that are not reporting, which is what the grid panel asks the person to look at. */
export function nodesNotOnline(grid: GridView): number {
    return Math.max(0, grid.total_hosts - grid.online_hosts);
}

function GridPanel(): ReactNode {
    const { t, plural } = useTranslation();
    const grid = useQuery({ queryKey: GRID_QUERY_KEY, queryFn: api.grid });
    const view: GridView | undefined = grid.data;
    const notOnline = view === undefined ? 0 : nodesNotOnline(view);

    const status: PanelStatus =
        view === undefined
            ? {
                  tone: 'none',
                  text: t(grid.isError ? 'home.system.panels.unread' : 'common.loading'),
              }
            : view.total_hosts === 0
              ? { tone: 'none', text: t('home.system.panels.grid.noNodes') }
              : notOnline === 0
                ? { tone: 'ok', text: t('home.system.allClear') }
                : {
                      tone: 'attention',
                      text: plural('home.system.panels.grid.needAttention', notOnline),
                  };

    return (
        <Panel title={t('home.system.panels.grid.title')} to="/operations/grid" status={status}>
            {view !== undefined && (
                <div className="grid grid-cols-2 gap-4">
                    <Figure
                        value={t('operations.services.count', {
                            reported: view.online_hosts,
                            expected: view.total_hosts,
                        })}
                        label={t('home.system.panels.grid.online')}
                    />
                    <Figure
                        value={String(view.idle_hosts)}
                        label={t('home.system.panels.grid.idle')}
                    />
                    <Figure
                        value={String(view.active_batches)}
                        label={t('home.system.panels.grid.activeBatches')}
                    />
                    <Figure
                        value={String(view.outcomes_client_error + view.outcomes_no_reply)}
                        label={t('home.system.panels.grid.failed')}
                    />
                </div>
            )}
        </Panel>
    );
}

/** The slow consumers in the newest server sample, which the queue panel asks the person to look at. */
export function slowConsumers(bus: BusView): number {
    return bus.samples[0]?.slow_consumers ?? 0;
}

function QueuePanel(): ReactNode {
    const { t, plural } = useTranslation();
    const bus = useQuery({
        queryKey: [BUS_QUERY_KEY, BUS_DASHBOARD_RANGE],
        queryFn: () => api.bus(BUS_DASHBOARD_RANGE),
    });
    const view: BusView | undefined = bus.data;
    const newest = view?.samples[0];
    const slow = view === undefined ? 0 : slowConsumers(view);

    const status: PanelStatus =
        view === undefined
            ? {
                  tone: 'none',
                  text: t(bus.isError ? 'home.system.panels.unread' : 'common.loading'),
              }
            : newest === undefined
              ? { tone: 'none', text: t('home.system.panels.queue.noSample') }
              : slow === 0
                ? { tone: 'ok', text: t('home.system.allClear') }
                : {
                      tone: 'attention',
                      text: plural('home.system.panels.queue.needAttention', slow),
                  };

    return (
        <Panel title={t('home.system.panels.queue.title')} to="/operations/bus" status={status}>
            {view !== undefined && newest !== undefined && (
                <div className="grid grid-cols-2 gap-4">
                    <Figure
                        value={String(newest.connections)}
                        label={t('home.system.panels.queue.connections')}
                    />
                    <Figure
                        value={String(newest.slow_consumers)}
                        label={t('home.system.panels.queue.slowConsumers')}
                    />
                    <Figure
                        value={String(view.streams.length)}
                        label={t('home.system.panels.queue.streams')}
                    />
                    <Figure
                        value={String(
                            view.streams.reduce((total, stream) => total + stream.messages, 0),
                        )}
                        label={t('home.system.panels.queue.stored')}
                    />
                </div>
            )}
        </Panel>
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
        <div>
            <h3 className="pb-2 text-xs font-semibold text-ink-muted">
                {t('home.system.attention')}
            </h3>
            <ul>
                {overview.attention.map(({ tenant, reason }) => {
                    const failed = reason === 'setup-failed' && tenant.setup !== null;
                    return (
                        <li
                            key={`${reason}-${tenant.id}`}
                            className="flex items-center gap-3 border-t border-line-subtle py-3"
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
        </div>
    );
}

function TenantTable({
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

    if (overview.tenants.length === 0) {
        return <p className="text-sm text-ink-muted">{t('home.system.noTenants')}</p>;
    }

    return (
        <div>
            <div className="overflow-x-auto">
                <table className="w-full text-left text-sm">
                    <thead>
                        <tr className="border-b border-line text-xs text-ink-muted">
                            <th className="py-2 pr-4 font-medium">{t('home.system.name')}</th>
                            <th className="py-2 pr-4 font-medium">{t('home.system.hostname')}</th>
                            <th className="py-2 pr-4 font-medium">{t('home.system.type')}</th>
                            <th className="py-2 font-medium">{t('home.system.status')}</th>
                        </tr>
                    </thead>
                    <tbody>
                        {overview.tenants.map((tenant: TenantSummary) => (
                            <tr key={tenant.id} className="border-b border-line-subtle">
                                <td className="py-2 pr-4">
                                    <Link
                                        to={`/tenants/${encodeURIComponent(tenant.code)}`}
                                        className="text-ink hover:text-accent-bright"
                                    >
                                        {tenant.name}
                                    </Link>
                                </td>
                                <td className="py-2 pr-4 text-ink-muted">{tenant.hostname}</td>
                                <td className="py-2 pr-4">
                                    <PaintedValue
                                        value={tenant.type}
                                        known={typeByCode.get(tenant.type)}
                                    />
                                </td>
                                <td className="py-2">
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
            <div className="flex flex-wrap items-center justify-between gap-3 pt-3 text-xs text-ink-muted">
                <span className="tabular-nums">
                    {plural('home.system.showing', overview.totalCount, {
                        shown: overview.tenants.length,
                    })}
                </span>
                <LinkButton to="/tenants" size="sm">
                    {t('home.system.seeAll')}
                </LinkButton>
            </div>
        </div>
    );
}

function ActiveModules(): ReactNode {
    const { t } = useTranslation();
    const active: Tile[] = [
        {
            title: t('home.system.tenants'),
            body: t('home.system.tenantsLead'),
            to: '/tenants',
            icon: 'party',
        },
        {
            title: t('shell.menu.accounts'),
            body: t('home.system.accountsBody'),
            to: '/people',
            icon: 'people',
        },
        {
            title: t('shell.menu.operations'),
            body: t('operations.hub.description'),
            to: '/operations',
            icon: 'server',
        },
        {
            title: t('home.tenant.security'),
            body: t('home.tenant.securityBody'),
            to: '/security',
            icon: 'locked',
        },
    ];

    return <Tiles tiles={active} />;
}

function UpcomingModules(): ReactNode {
    const { t } = useTranslation();
    const coming: Tile[] = [
        {
            title: t('home.system.feeds'),
            body: t('home.system.feedsBody'),
            icon: 'chart',
        },
        {
            title: t('home.system.processTypes'),
            body: t('home.system.processTypesBody'),
            icon: 'trend',
        },
        {
            title: t('home.system.catalogue'),
            body: t('home.system.catalogueBody'),
            icon: 'database',
        },
    ];

    return <Tiles tiles={coming} later={t('home.party.comingLater')} />;
}
