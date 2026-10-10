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
    DeploymentOverview,
    SetupActivity,
    TenantStatus,
    TenantSummary,
    TenantType,
} from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';

import { BUS_QUERY_KEY, sampleTime } from '../operations/BusPage.js';
import { GRID_QUERY_KEY } from '../operations/GridPage.js';
import {
    DEFAULT_HEALTH_RANGE,
    InstallationFigures,
    releaseLabel,
    useInstallationHealth,
} from '../operations/InstallationHealth.js';
import { Metric } from '../ui/Metric.js';
import { LinkButton, Notice, PageHeader } from '../ui/Primitives.js';
import { Sparkline } from '../ui/Sparkline.js';
import { Tiles, type Tile } from '../ui/Tiles.js';
import { useTabs } from '../ui/Tabs.js';
import {
    nodesNotOnline,
    installationVerdict,
    slowConsumers,
    throughputSeries,
    type StatusTone,
} from './dashboardStatus.js';
import { PaintedValue } from './TenantParts.js';

/** What the translation hook answers, passed to the pure status functions. */
type Translator = ReturnType<typeof useTranslation>;

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

/** How a panel states its status: the tone paints the mark, the words are the panel's. */
interface PanelStatus {
    readonly tone: StatusTone;
    readonly text: string;
}

/**
 * Every read the dashboard stands on, made once.
 *
 * The status at the top of the page and the four panels all draw from this, so
 * they cannot disagree about what was read.
 */
function useDashboardData() {
    const overview = useQuery({ queryKey: ['overview'], queryFn: api.overview });
    /*
     * The types and statuses paint the badges. They are a second read, and a
     * failure to read them leaves the words unpainted rather than the page
     * unread.
     */
    const types = useQuery({ queryKey: ['tenant-types'], queryFn: api.tenantTypes });
    const tenantStatuses = useQuery({ queryKey: ['tenant-statuses'], queryFn: api.tenantStatuses });
    const health = useInstallationHealth(DEFAULT_HEALTH_RANGE);
    const grid = useQuery({ queryKey: GRID_QUERY_KEY, queryFn: api.grid });
    const bus = useQuery({
        queryKey: [BUS_QUERY_KEY, BUS_DASHBOARD_RANGE],
        queryFn: () => api.bus(BUS_DASHBOARD_RANGE),
    });
    return { overview, types, tenantStatuses, health, grid, bus };
}

type DashboardData = ReturnType<typeof useDashboardData>;

function tenantsStatus(data: DashboardData, tr: Translator): PanelStatus {
    const { t, plural } = tr;
    const overview = data.overview.data;
    if (overview === undefined || data.overview.isError) {
        return { tone: 'pending', text: t('home.system.panels.unread') };
    }
    return overview.attention.length === 0
        ? { tone: 'ok', text: t('home.system.allClear') }
        : {
              tone: 'attention',
              text: plural('home.system.needAttention', overview.attention.length),
          };
}

function servicesStatus(data: DashboardData, tr: Translator): PanelStatus {
    const { t, plural } = tr;
    const { roster, rosterFailed } = data.health;
    if (roster === undefined) {
        return {
            tone: 'pending',
            text: t(rosterFailed ? 'home.system.panels.unread' : 'common.loading'),
        };
    }
    const behind = roster.lost + roster.missing;
    return behind === 0
        ? { tone: 'ok', text: t('home.system.allClear') }
        : { tone: 'attention', text: plural('home.system.panels.services.needAttention', behind) };
}

function gridStatus(data: DashboardData, tr: Translator): PanelStatus {
    const { t, plural } = tr;
    const view = data.grid.data;
    if (view === undefined) {
        return {
            tone: 'pending',
            text: t(data.grid.isError ? 'home.system.panels.unread' : 'common.loading'),
        };
    }
    if (view.total_hosts === 0) {
        return { tone: 'quiet', text: t('home.system.panels.grid.noNodes') };
    }
    const offline = nodesNotOnline(view);
    return offline === 0
        ? { tone: 'ok', text: t('home.system.allClear') }
        : { tone: 'attention', text: plural('home.system.panels.grid.needAttention', offline) };
}

function queueStatus(data: DashboardData, tr: Translator): PanelStatus {
    const { t, plural } = tr;
    const view = data.bus.data;
    if (view === undefined) {
        return {
            tone: 'pending',
            text: t(data.bus.isError ? 'home.system.panels.unread' : 'common.loading'),
        };
    }
    if (view.samples[0] === undefined) {
        return { tone: 'quiet', text: t('home.system.panels.queue.noSample') };
    }
    const slow = slowConsumers(view);
    return slow === 0
        ? { tone: 'ok', text: t('home.system.allClear') }
        : { tone: 'attention', text: plural('home.system.panels.queue.needAttention', slow) };
}

export function SystemHome({ name }: { readonly name: string }): ReactNode {
    const translator = useTranslation();
    const { t, plural } = translator;
    const data = useDashboardData();
    const { tab, bar } = useTabs({
        label: t('home.system.tabs.label'),
        tabs: TABS,
        titleOf: (candidate) => t(`home.system.tabs.${candidate}`),
    });

    const statuses = [
        tenantsStatus(data, translator),
        servicesStatus(data, translator),
        gridStatus(data, translator),
        queueStatus(data, translator),
    ];
    const verdict = installationVerdict(statuses.map((status) => status.tone));

    return (
        <div className="space-y-6">
            <PageHeader
                title={t('home.welcome', { name })}
                actions={
                    verdict.kind === 'pending' ? undefined : (
                        <StatusChip
                            status={
                                verdict.kind === 'ok'
                                    ? { tone: 'ok', text: t('home.system.allClear') }
                                    : {
                                          tone: 'attention',
                                          text: plural(
                                              'home.system.panels.summary.needAttention',
                                              verdict.count,
                                          ),
                                      }
                            }
                        />
                    )
                }
            />
            {bar}
            {tab === 'dashboard' && <Dashboard data={data} translator={translator} />}
            {tab === 'active' && <ActiveModules />}
            {tab === 'upcoming' && <UpcomingModules />}
        </div>
    );
}

const MARK: Readonly<Record<StatusTone, { readonly glyph: string; readonly classes: string }>> = {
    ok: { glyph: '✓', classes: 'border-up/50 bg-up/10 text-up' },
    attention: { glyph: '!', classes: 'border-warn/50 bg-warn/10 text-warn' },
    quiet: { glyph: '–', classes: 'border-line bg-surface-base text-ink-faint' },
    pending: { glyph: '…', classes: 'border-line bg-surface-base text-ink-faint' },
};

function StatusMark({
    tone,
    label,
}: {
    readonly tone: StatusTone;
    /** What a screen reader says, since the mark itself is a glyph. */
    readonly label: string;
}): ReactNode {
    const mark = MARK[tone];
    return (
        <span
            role="img"
            aria-label={label}
            className={`grid h-5 w-5 shrink-0 place-items-center rounded-full border text-xs font-bold ${mark.classes}`}
        >
            {mark.glyph}
        </span>
    );
}

/** The one verdict for the whole installation, in the words every panel uses. */
function StatusChip({ status }: { readonly status: PanelStatus }): ReactNode {
    const classes =
        status.tone === 'ok'
            ? 'border-up/50 bg-up/10 text-up'
            : 'border-warn/50 bg-warn/10 text-warn';
    return (
        <span
            className={`inline-flex items-center gap-2 rounded-full border px-3 py-1.5 text-xs font-medium ${classes}`}
        >
            <span aria-hidden="true">{MARK[status.tone].glyph}</span>
            {status.text}
        </span>
    );
}

/**
 * One dashboard panel: a title with a mark that says how it is doing, the
 * figures behind it, and a footer that says how old they are.
 *
 * A panel that is fine says so with its mark alone, because four sentences
 * saying the same thing take the room the figures need. A panel that needs the
 * person, or has not been read, says why in a line under its title.
 */
function Panel({
    title,
    to,
    status,
    footer,
    children,
}: {
    readonly title: string;
    readonly to: string;
    readonly status: PanelStatus;
    readonly footer?: ReactNode;
    readonly children: ReactNode;
}): ReactNode {
    const { t } = useTranslation();

    return (
        <section className="card flex flex-col justify-between gap-4 p-5">
            <div className="space-y-4">
                <header className="flex flex-wrap items-center justify-between gap-2">
                    <div className="flex items-center gap-2">
                        <h2 className="text-sm font-semibold text-ink">{title}</h2>
                        <StatusMark tone={status.tone} label={status.text} />
                    </div>
                    <LinkButton to={to} size="sm">
                        {t('home.system.panels.open')}
                    </LinkButton>
                </header>
                {status.tone !== 'ok' && (
                    <p
                        className={`text-sm ${status.tone === 'attention' ? 'text-warn' : 'text-ink-muted'}`}
                    >
                        {status.text}
                    </p>
                )}
                {children}
            </div>
            {footer !== undefined && (
                <footer className="flex flex-wrap items-center justify-between gap-2 border-t border-line-subtle pt-3 text-xs text-ink-muted">
                    {footer}
                </footer>
            )}
        </section>
    );
}

function Dashboard({
    data,
    translator,
}: {
    readonly data: DashboardData;
    readonly translator: Translator;
}): ReactNode {
    return (
        <div className="grid gap-4 lg:grid-cols-2">
            <TenantsPanel data={data} translator={translator} />
            <ServicesPanel data={data} translator={translator} />
            <GridPanel data={data} translator={translator} />
            <QueuePanel data={data} translator={translator} />
        </div>
    );
}

interface PanelProps {
    readonly data: DashboardData;
    readonly translator: Translator;
}

function TenantsPanel({ data: reads, translator }: PanelProps): ReactNode {
    const { t, plural } = translator;
    const { overview } = reads;
    const data = overview.data;

    return (
        <Panel
            title={t('home.system.tenants')}
            to="/tenants"
            status={tenantsStatus(reads, translator)}
            footer={
                data === undefined || data.tenants.length === 0 ? undefined : (
                    <>
                        <span className="tabular-nums">
                            {plural('home.system.showing', data.totalCount, {
                                shown: data.tenants.length,
                            })}
                        </span>
                        <LinkButton to="/tenants" size="sm">
                            {t('home.system.seeAll')}
                        </LinkButton>
                    </>
                )
            }
        >
            {overview.isError && <Notice tone="error">{overview.error.message}</Notice>}
            {data !== undefined && (
                <>
                    <div className="grid grid-cols-3 gap-3">
                        <Metric value={String(data.inService)} label={t('home.system.inService')} />
                        <Metric
                            value={String(data.onEvaluation)}
                            label={t('home.system.onEvaluation')}
                        />
                        <Metric value={String(data.settingUp)} label={t('home.system.settingUp')} />
                    </div>
                    <Activity overview={data} />
                    {data.attention.length > 0 && <Attention overview={data} />}
                    <TenantTable
                        overview={data}
                        types={reads.types.data ?? []}
                        statuses={reads.tenantStatuses.data ?? []}
                    />
                </>
            )}
        </Panel>
    );
}

function ServicesPanel({ data: reads, translator }: PanelProps): ReactNode {
    const { t, plural } = translator;
    const { roster } = reads.health;

    return (
        <Panel
            title={t('home.system.panels.services.title')}
            to="/operations/services"
            status={servicesStatus(reads, translator)}
            footer={
                roster?.newest === undefined ? undefined : (
                    <>
                        <span>{t('operations.overview.release')}</span>
                        <span className="flex items-center gap-2">
                            {roster.servicesBehind > 0 && (
                                <span className="text-warn">
                                    {plural('operations.overview.behind', roster.servicesBehind)}
                                </span>
                            )}
                            <span className="font-mono text-ink">
                                {releaseLabel(roster.newest)}
                            </span>
                        </span>
                    </>
                )
            }
        >
            {/* The figures read the same queries as the status above, so the cache serves both. */}
            <InstallationFigures range={DEFAULT_HEALTH_RANGE} compact />
        </Panel>
    );
}

function GridPanel({ data: reads, translator }: PanelProps): ReactNode {
    const { t } = translator;
    const view = reads.grid.data;
    const sampled = view === undefined ? undefined : sampleTime(view.sampled_at);

    return (
        <Panel
            title={t('home.system.panels.grid.title')}
            to="/operations/grid"
            status={gridStatus(reads, translator)}
            footer={
                sampled === undefined ? undefined : (
                    <>
                        <span>{t('home.system.panels.sampled')}</span>
                        <span className="font-mono text-ink">{sampled}</span>
                    </>
                )
            }
        >
            {view !== undefined && (
                <div className="grid grid-cols-2 gap-3">
                    <Metric
                        value={t('operations.services.count', {
                            reported: view.online_hosts,
                            expected: view.total_hosts,
                        })}
                        label={t('home.system.panels.grid.online')}
                        tone={
                            view.total_hosts > 0 && view.online_hosts === view.total_hosts
                                ? 'good'
                                : 'neutral'
                        }
                    />
                    <Metric
                        value={String(view.idle_hosts)}
                        label={t('home.system.panels.grid.idle')}
                    />
                    <Metric
                        value={String(view.active_batches)}
                        label={t('home.system.panels.grid.activeBatches')}
                    />
                    <Metric
                        value={String(view.outcomes_client_error + view.outcomes_no_reply)}
                        label={t('home.system.panels.grid.failed')}
                        tone={
                            view.outcomes_client_error + view.outcomes_no_reply > 0
                                ? 'bad'
                                : 'neutral'
                        }
                    />
                </div>
            )}
        </Panel>
    );
}

function QueuePanel({ data: reads, translator }: PanelProps): ReactNode {
    const { t } = translator;
    const view = reads.bus.data;
    const newest = view?.samples[0];
    const rates = view === undefined ? [] : throughputSeries(view);
    const latest = rates[rates.length - 1];

    return (
        <Panel
            title={t('home.system.panels.queue.title')}
            to="/operations/bus"
            status={queueStatus(reads, translator)}
            footer={
                newest === undefined ? undefined : (
                    <>
                        <span>{t('home.system.panels.sampled')}</span>
                        <span className="font-mono text-ink">
                            {sampleTime(newest.sampled_at) ?? ''}
                        </span>
                    </>
                )
            }
        >
            {view !== undefined && newest !== undefined && (
                <>
                    <div className="grid grid-cols-2 gap-3">
                        <Metric
                            value={String(newest.connections)}
                            label={t('home.system.panels.queue.connections')}
                        />
                        <Metric
                            value={String(newest.slow_consumers)}
                            label={t('home.system.panels.queue.slowConsumers')}
                            tone={newest.slow_consumers > 0 ? 'warn' : 'neutral'}
                        />
                        <Metric
                            value={String(view.streams.length)}
                            label={t('home.system.panels.queue.streams')}
                        />
                        <Metric
                            value={String(
                                view.streams.reduce((total, stream) => total + stream.messages, 0),
                            )}
                            label={t('home.system.panels.queue.stored')}
                        />
                    </div>
                    {latest !== undefined && rates.length >= 2 && (
                        <div className="rounded-lg border border-line-subtle bg-surface-base p-3">
                            <div className="mb-1 flex items-center justify-between text-xs text-ink-muted">
                                <span>{t('home.system.panels.queue.throughput')}</span>
                                <span className="font-mono text-accent">
                                    {t('home.system.panels.queue.perSecond', {
                                        rate: latest.toFixed(1),
                                    })}
                                </span>
                            </div>
                            <Sparkline points={rates} />
                        </div>
                    )}
                </>
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
    const { t } = useTranslation();
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
