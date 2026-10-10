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

import type { ReactNode } from 'react';
import { useQuery } from '@tanstack/react-query';
import { useHolds } from '../access/holds.js';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { readTime } from '../operations/ServicesPage.js';
import { Metric } from '../ui/Metric.js';
import { LinkButton, PageHeader } from '../ui/Primitives.js';
import { Tiles, type Tile } from '../ui/Tiles.js';
import { useTabs } from '../ui/Tabs.js';
import { AUDIT_ACCOUNTS_QUERY_KEY, AUDIT_SESSIONS_QUERY_KEY } from './AuditPage.js';
import { Panel, StatusChip, type PanelStatus } from './DashboardParts.js';
import {
    accountsWithFailedSignIns,
    installationVerdict,
    lockedAccounts,
    passwordResetsDue,
} from './dashboardStatus.js';

/**
 * The home of a person who works inside a tenant.
 *
 * A tenant administrator and a member both land here, because the session mode
 * says only that the session acts inside a tenant. What differs between them is
 * what they may do, so the home is built from the permissions the person holds:
 * a panel appears for a person who may read what it shows, and a card for a
 * person who may open the screen. A member who holds none of the panels' reads
 * gets no Dashboard tab and lands on the Active modules tab.
 *
 * The same three tabs as the system administrator's home. The Dashboard holds
 * panels that state their status in the same words, the Active modules tab holds
 * the places the person works in, and the Upcoming modules tab holds the areas
 * that are still to come.
 */

/** What the translation hook answers, passed to the pure status functions. */
type Translator = ReturnType<typeof useTranslation>;

/** The permissions that decide what the home shows. */
const PERMISSION = {
    accountsRead: 'iam::accounts:read',
    organisationRead: 'iam::organisation:read',
    loginInfoRead: 'iam::login_info:read',
    sessionsRead: 'iam::sessions:read',
    rolesAssign: 'iam::roles:assign',
    rolesRead: 'iam::roles:read',
    accountsLock: 'iam::accounts:lock',
    partiesRead: 'refdata::parties:read',
    partiesWrite: 'refdata::parties:write',
    counterpartiesWrite: 'refdata::counterparties:write',
} as const;

/** Which panels this person may see, from the permissions they hold. */
interface Visible {
    readonly people: boolean;
    readonly signIns: boolean;
    readonly requests: boolean;
    readonly parties: boolean;
}

function visiblePanels(holds: (code: string) => boolean): Visible {
    const people = holds(PERMISSION.loginInfoRead) && holds(PERMISSION.accountsRead);
    const signIns = holds(PERMISSION.loginInfoRead) && holds(PERMISSION.sessionsRead);
    const requests = holds(PERMISSION.rolesAssign);
    const parties = holds(PERMISSION.partiesRead);
    return { people, signIns, requests, parties };
}

/** How often the open dashboard reads its figures again. */
const DASHBOARD_REFRESH_MS = 15_000;

/** The most login records one read returns, which the server caps. */
const LOGIN_RECORDS_READ = 1000;

/**
 * Every read the dashboard stands on, made once and only for a panel that is
 * shown.
 *
 * The accounts and sessions reads share their keys with the sign-ins screen. The
 * login records are read in a larger page than that screen reads, so they have a
 * key of their own. A read the person may not make is not sent, because the
 * server would refuse it.
 */
function useDashboardData(visible: Visible) {
    const accounts = useQuery({
        queryKey: [AUDIT_ACCOUNTS_QUERY_KEY],
        queryFn: () => api.accounts(),
        enabled: visible.people,
        retry: false,
    });
    const logins = useQuery({
        queryKey: ['tenant-home-logins'],
        queryFn: () => api.loginInfoPage({ limit: LOGIN_RECORDS_READ }),
        enabled: visible.people || visible.signIns,
        retry: false,
        staleTime: 0,
        refetchInterval: DASHBOARD_REFRESH_MS,
    });
    const sessions = useQuery({
        queryKey: [AUDIT_SESSIONS_QUERY_KEY],
        queryFn: () => api.activeSessions(),
        enabled: visible.signIns,
        retry: false,
        staleTime: 0,
        refetchInterval: DASHBOARD_REFRESH_MS,
    });
    const requests = useQuery({
        queryKey: ['tenant-home-requests'],
        queryFn: () => api.requestQueue({ offset: 0, limit: 1 }),
        enabled: visible.requests,
        retry: false,
        staleTime: 0,
        refetchInterval: DASHBOARD_REFRESH_MS,
    });
    const parties = useQuery({
        queryKey: ['tenant-home-parties'],
        queryFn: () => api.parties({ offset: 0, limit: 1 }),
        enabled: visible.parties,
        retry: false,
    });
    return { accounts, logins, sessions, requests, parties };
}

type DashboardData = ReturnType<typeof useDashboardData>;

interface Pending {
    readonly isError: boolean;
}

/** The status of a panel whose read has not answered: still reading, or could not be read. */
function pending(read: Pending, tr: Translator): PanelStatus {
    return {
        tone: 'pending',
        text: tr.t(read.isError ? 'home.system.panels.unread' : 'common.loading'),
    };
}

function peopleStatus(data: DashboardData, tr: Translator): PanelStatus {
    const rows = data.logins.data?.loginInfo;
    if (rows === undefined || data.logins.isError) {
        return pending(data.logins, tr);
    }
    const locked = lockedAccounts(rows);
    return locked === 0
        ? { tone: 'ok', text: tr.t('home.system.allClear') }
        : {
              tone: 'attention',
              text: tr.plural('home.tenantDashboard.people.needAttention', locked),
          };
}

function signInsStatus(data: DashboardData, tr: Translator): PanelStatus {
    const rows = data.logins.data?.loginInfo;
    if (rows === undefined || data.logins.isError) {
        return pending(data.logins, tr);
    }
    const failed = accountsWithFailedSignIns(rows);
    return failed === 0
        ? { tone: 'ok', text: tr.t('home.system.allClear') }
        : {
              tone: 'attention',
              text: tr.plural('home.tenantDashboard.signIns.needAttention', failed),
          };
}

function requestsStatus(data: DashboardData, tr: Translator): PanelStatus {
    const queue = data.requests.data;
    if (queue === undefined || data.requests.isError) {
        return pending(data.requests, tr);
    }
    return queue.total === 0
        ? { tone: 'ok', text: tr.t('home.system.allClear') }
        : {
              tone: 'attention',
              text: tr.plural('home.tenantDashboard.requests.needAttention', queue.total),
          };
}

function partiesStatus(data: DashboardData, tr: Translator): PanelStatus {
    const page = data.parties.data;
    if (page === undefined || data.parties.isError) {
        return pending(data.parties, tr);
    }
    return page.totalCount === 0
        ? { tone: 'quiet', text: tr.t('home.tenantDashboard.parties.none') }
        : { tone: 'ok', text: tr.t('home.system.allClear') };
}

const TABS_WITH_DASHBOARD = ['dashboard', 'active', 'upcoming'] as const;
const TABS_WITHOUT_DASHBOARD = ['active', 'upcoming'] as const;

export function ApplicationHome({
    name,
    partyName,
}: {
    readonly name: string;
    readonly partyName: string;
}): ReactNode {
    const translator = useTranslation();
    const { t, plural } = translator;
    const holds = useHolds();
    const visible = visiblePanels(holds);
    const hasDashboard = visible.people || visible.signIns || visible.requests || visible.parties;
    const data = useDashboardData(visible);
    const { tab, bar } = useTabs({
        label: t('home.system.tabs.label'),
        tabs: hasDashboard ? TABS_WITH_DASHBOARD : TABS_WITHOUT_DASHBOARD,
        titleOf: (candidate) => t(`home.system.tabs.${candidate}`),
    });

    const statuses = [
        ...(visible.people ? [peopleStatus(data, translator)] : []),
        ...(visible.signIns ? [signInsStatus(data, translator)] : []),
        ...(visible.requests ? [requestsStatus(data, translator)] : []),
        ...(visible.parties ? [partiesStatus(data, translator)] : []),
    ];
    const verdict = installationVerdict(statuses.map((status) => status.tone));

    return (
        <div className="space-y-6">
            <PageHeader
                title={t('home.welcome', { name })}
                description={t('home.party.lead', { party: partyName })}
                actions={
                    !hasDashboard || verdict.kind === 'pending' ? undefined : (
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
            {tab === 'dashboard' && (
                <Dashboard data={data} translator={translator} visible={visible} />
            )}
            {tab === 'active' && <ActiveModules holds={holds} />}
            {tab === 'upcoming' && <UpcomingModules />}
        </div>
    );
}

interface PanelProps {
    readonly data: DashboardData;
    readonly translator: Translator;
}

function Dashboard({
    data,
    translator,
    visible,
}: PanelProps & { readonly visible: Visible }): ReactNode {
    return (
        <div className="grid gap-4 lg:grid-cols-2">
            {visible.people && <PeoplePanel data={data} translator={translator} />}
            {visible.signIns && <SignInsPanel data={data} translator={translator} />}
            {visible.requests && <RequestsPanel data={data} translator={translator} />}
            {visible.parties && <PartiesPanel data={data} translator={translator} />}
        </div>
    );
}

/** The footer every panel ends in: when its figures were read. */
function ReadAt({ at, t }: { readonly at: number; readonly t: Translator['t'] }): ReactNode {
    return (
        <>
            <span>{t('home.tenantDashboard.read')}</span>
            <span className="font-mono text-ink">{at === 0 ? '' : readTime(at)}</span>
        </>
    );
}

function count(value: number | undefined, complete = true): string {
    if (value === undefined) {
        return '—';
    }
    return complete || value === 0 ? String(value) : `${String(value)}+`;
}

/** Whether the records read are all there are; if not, a count from them is a lower bound. */
function allRecordsRead(data: DashboardData): boolean {
    const page = data.logins.data;
    return page === undefined || page.loginInfo.length >= page.totalCount;
}

function PeoplePanel({ data, translator }: PanelProps): ReactNode {
    const { t } = translator;
    const rows = data.logins.data?.loginInfo;

    return (
        <Panel
            title={t('home.tenantDashboard.people.title')}
            to="/organisation"
            status={peopleStatus(data, translator)}
            footer={<ReadAt at={data.logins.dataUpdatedAt} t={t} />}
        >
            <div className="grid grid-cols-3 gap-3">
                <Metric
                    value={count(data.accounts.data?.totalCount)}
                    label={t('home.tenantDashboard.people.accounts')}
                />
                <Metric
                    value={count(
                        rows === undefined ? undefined : lockedAccounts(rows),
                        allRecordsRead(data),
                    )}
                    label={t('home.tenantDashboard.people.locked')}
                    tone={
                        rows === undefined ? 'neutral' : lockedAccounts(rows) > 0 ? 'bad' : 'good'
                    }
                    to="/rescue"
                />
                <Metric
                    value={count(
                        rows === undefined ? undefined : passwordResetsDue(rows),
                        allRecordsRead(data),
                    )}
                    label={t('home.tenantDashboard.people.resets')}
                />
            </div>
        </Panel>
    );
}

function SignInsPanel({ data, translator }: PanelProps): ReactNode {
    const { t } = translator;
    const rows = data.logins.data?.loginInfo;
    const failed = rows === undefined ? undefined : accountsWithFailedSignIns(rows);

    return (
        <Panel
            title={t('home.tenantDashboard.signIns.title')}
            to="/audit"
            status={signInsStatus(data, translator)}
            footer={<ReadAt at={data.sessions.dataUpdatedAt} t={t} />}
        >
            <div className="grid grid-cols-2 gap-3">
                <Metric
                    value={count(data.sessions.data?.length)}
                    label={t('home.tenantDashboard.signIns.signedIn')}
                />
                <Metric
                    value={count(failed, allRecordsRead(data))}
                    label={t('home.tenantDashboard.signIns.failed')}
                    tone={failed === undefined ? 'neutral' : failed > 0 ? 'bad' : 'good'}
                />
            </div>
        </Panel>
    );
}

function RequestsPanel({ data, translator }: PanelProps): ReactNode {
    const { t } = translator;
    const queue = data.requests.data;

    return (
        <Panel
            title={t('home.tenantDashboard.requests.title')}
            to="/requests"
            status={requestsStatus(data, translator)}
            footer={<ReadAt at={data.requests.dataUpdatedAt} t={t} />}
        >
            <div className="grid grid-cols-2 gap-3">
                <Metric
                    value={count(queue?.total)}
                    label={t('home.tenantDashboard.requests.waiting')}
                    tone={queue === undefined ? 'neutral' : queue.total > 0 ? 'warn' : 'good'}
                />
                <Metric
                    value={count(queue?.answered.length)}
                    label={t('home.tenantDashboard.requests.answered')}
                />
            </div>
        </Panel>
    );
}

function PartiesPanel({ data, translator }: PanelProps): ReactNode {
    const { t } = translator;
    const holds = useHolds();

    return (
        <Panel
            title={t('home.tenant.parties')}
            to="/parties"
            status={partiesStatus(data, translator)}
            footer={
                <>
                    <ReadAt at={data.parties.dataUpdatedAt} t={t} />
                    {holds(PERMISSION.partiesWrite) && (
                        <LinkButton to="/parties/new" size="sm">
                            {t('home.tenant.newParty')}
                        </LinkButton>
                    )}
                </>
            }
        >
            <div className="grid grid-cols-2 gap-3">
                <Metric
                    value={count(data.parties.data?.totalCount)}
                    label={t('home.tenantDashboard.parties.count')}
                />
            </div>
        </Panel>
    );
}

/**
 * The places this person can go, each offered only to a person who may open it.
 *
 * The run is built rather than written out, so a member sees the few screens
 * that are theirs and a tenant administrator sees the administration as well.
 */
function ActiveModules({ holds }: { readonly holds: (code: string) => boolean }): ReactNode {
    const { t } = useTranslation();
    const maybe = (allowed: boolean, tile: Tile): readonly Tile[] => (allowed ? [tile] : []);
    const active: Tile[] = [
        ...maybe(holds(PERMISSION.partiesRead), {
            title: t('home.tenant.parties'),
            body: t('home.tenant.partiesBody'),
            to: '/parties',
            icon: 'party',
        }),
        ...maybe(holds(PERMISSION.accountsRead) || holds(PERMISSION.organisationRead), {
            title: t('home.tenant.organisation'),
            body: t('home.tenant.organisationBody'),
            to: '/organisation',
            icon: 'people',
        }),
        ...maybe(holds(PERMISSION.rolesRead), {
            title: t('home.tenant.roles'),
            body: t('home.tenant.rolesBody'),
            to: '/roles',
            icon: 'access',
        }),
        {
            title: t('home.party.refdata'),
            body: t('home.party.refdataBody'),
            to: '/refdata',
            icon: 'database',
        },
        ...maybe(holds(PERMISSION.partiesWrite), {
            title: t('home.tenant.newParty'),
            body: t('home.tenant.newPartyBody'),
            to: '/parties/new',
            icon: 'add',
        }),
        ...maybe(holds(PERMISSION.partiesWrite), {
            title: t('home.tenant.partyDetails'),
            body: t('home.tenant.partyDetailsBody'),
            to: '/parties/details',
            icon: 'party',
        }),
        ...maybe(holds(PERMISSION.counterpartiesWrite), {
            title: t('home.tenant.counterpartyOnboard'),
            body: t('home.tenant.counterpartyOnboardBody'),
            to: '/counterparties/onboard',
            icon: 'people',
        }),
        ...maybe(holds(PERMISSION.accountsLock), {
            title: t('home.tenant.rescue'),
            body: t('home.tenant.rescueBody'),
            to: '/rescue',
            icon: 'unlock',
        }),
        ...maybe(holds(PERMISSION.sessionsRead) || holds(PERMISSION.loginInfoRead), {
            title: t('home.tenant.audit'),
            body: t('home.tenant.auditBody'),
            to: '/audit',
            icon: 'record',
        }),
        {
            title: t('home.tenant.access'),
            body: t('home.tenant.accessBody'),
            to: '/access',
            icon: 'person',
        },
        {
            title: t('home.tenant.versions'),
            body: t('home.tenant.versionsBody'),
            to: '/operations/versions',
            icon: 'history',
        },
        {
            title: t('home.party.security'),
            body: t('home.party.securityBody'),
            to: '/security',
            icon: 'locked',
        },
    ];

    return <Tiles tiles={active} />;
}

function UpcomingModules(): ReactNode {
    const { t } = useTranslation();
    const icons = { marketdata: 'chart', trading: 'trend', reporting: 'document' } as const;
    const coming: Tile[] = (['marketdata', 'trading', 'reporting'] as const).map((area) => ({
        title: t(`home.party.${area}`),
        body: t(`home.party.${area}Body`),
        icon: icons[area],
    }));

    return <Tiles tiles={coming} later={t('home.party.comingLater')} />;
}
