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
import {
    AUDIT_ACCOUNTS_QUERY_KEY,
    AUDIT_FAILURES_QUERY_KEY,
    AUDIT_SESSIONS_QUERY_KEY,
} from './AuditPage.js';
import { Panel, StatusChip, type PanelStatus } from './DashboardParts.js';
import {
    accountsWithFailedSignIns,
    installationVerdict,
    lockedAccounts,
    passwordResetsDue,
} from './dashboardStatus.js';

/**
 * The tenant administrator's home.
 *
 * The same three tabs as the system administrator's home. The Dashboard holds
 * panels that state their status in the same words, the Active modules tab
 * holds the places the person works in, and the Upcoming modules tab holds the
 * areas that are still to come.
 */

/** What the translation hook answers, passed to the pure status functions. */
type Translator = ReturnType<typeof useTranslation>;

const TABS = ['dashboard', 'active', 'upcoming'] as const;

/** The permission that opens the access request queue, as the menu gates it. */
const ASSIGN_ROLES = 'iam::roles:assign';

/**
 * Every read the dashboard stands on, made once.
 *
 * The sign-in reads share their keys with the sign-ins screen, so a person who
 * opens it after the home reads nothing twice. The request queue is read only
 * for a person who may answer it, because the server would refuse the rest.
 */
function useTenantDashboardData(mayAnswerRequests: boolean) {
    const accounts = useQuery({
        queryKey: [AUDIT_ACCOUNTS_QUERY_KEY],
        queryFn: () => api.accounts(),
        retry: false,
    });
    const logins = useQuery({
        queryKey: [AUDIT_FAILURES_QUERY_KEY],
        queryFn: () => api.loginInfoPage(),
        retry: false,
    });
    const sessions = useQuery({
        queryKey: [AUDIT_SESSIONS_QUERY_KEY],
        queryFn: () => api.activeSessions(),
        retry: false,
    });
    const requests = useQuery({
        queryKey: ['tenant-home-requests'],
        queryFn: () => api.requestQueue({ offset: 0, limit: 1 }),
        enabled: mayAnswerRequests,
        retry: false,
    });
    const parties = useQuery({
        queryKey: ['tenant-home-parties'],
        queryFn: () => api.parties({ offset: 0, limit: 1 }),
        retry: false,
    });
    return { accounts, logins, sessions, requests, parties };
}

type DashboardData = ReturnType<typeof useTenantDashboardData>;

type Read = { readonly isError: boolean; readonly data: unknown };

/** A panel whose read has not answered says why: still reading, or could not be read. */
function unanswered(read: Read, tr: Translator): PanelStatus | undefined {
    if (read.data !== undefined && !read.isError) {
        return undefined;
    }
    return {
        tone: 'pending',
        text: tr.t(read.isError ? 'home.system.panels.unread' : 'common.loading'),
    };
}

function peopleStatus(data: DashboardData, tr: Translator): PanelStatus {
    const waiting = unanswered(data.logins, tr);
    if (waiting !== undefined || data.logins.data === undefined) {
        return waiting ?? { tone: 'pending', text: tr.t('common.loading') };
    }
    const locked = lockedAccounts(data.logins.data.loginInfo);
    return locked === 0
        ? { tone: 'ok', text: tr.t('home.system.allClear') }
        : {
              tone: 'attention',
              text: tr.plural('home.tenantDashboard.people.needAttention', locked),
          };
}

function signInsStatus(data: DashboardData, tr: Translator): PanelStatus {
    const waiting = unanswered(data.logins, tr);
    if (waiting !== undefined || data.logins.data === undefined) {
        return waiting ?? { tone: 'pending', text: tr.t('common.loading') };
    }
    const failed = accountsWithFailedSignIns(data.logins.data.loginInfo);
    return failed === 0
        ? { tone: 'ok', text: tr.t('home.system.allClear') }
        : {
              tone: 'attention',
              text: tr.plural('home.tenantDashboard.signIns.needAttention', failed),
          };
}

function requestsStatus(data: DashboardData, tr: Translator): PanelStatus {
    const waiting = unanswered(data.requests, tr);
    if (waiting !== undefined || data.requests.data === undefined) {
        return waiting ?? { tone: 'pending', text: tr.t('common.loading') };
    }
    const count = data.requests.data.total;
    return count === 0
        ? { tone: 'ok', text: tr.t('home.system.allClear') }
        : {
              tone: 'attention',
              text: tr.plural('home.tenantDashboard.requests.needAttention', count),
          };
}

function partiesStatus(data: DashboardData, tr: Translator): PanelStatus {
    const waiting = unanswered(data.parties, tr);
    if (waiting !== undefined || data.parties.data === undefined) {
        return waiting ?? { tone: 'pending', text: tr.t('common.loading') };
    }
    return data.parties.data.totalCount === 0
        ? { tone: 'quiet', text: tr.t('home.tenantDashboard.parties.none') }
        : { tone: 'ok', text: tr.t('home.system.allClear') };
}

export function TenantHome({
    tenantName,
}: {
    readonly name: string;
    readonly tenantName: string;
}): ReactNode {
    const translator = useTranslation();
    const { t, plural } = translator;
    const holds = useHolds();
    const mayAnswerRequests = holds(ASSIGN_ROLES);
    const data = useTenantDashboardData(mayAnswerRequests);
    const { tab, bar } = useTabs({
        label: t('home.system.tabs.label'),
        tabs: TABS,
        titleOf: (candidate) => t(`home.system.tabs.${candidate}`),
    });

    const statuses = [
        peopleStatus(data, translator),
        signInsStatus(data, translator),
        ...(mayAnswerRequests ? [requestsStatus(data, translator)] : []),
        partiesStatus(data, translator),
    ];
    const verdict = installationVerdict(statuses.map((status) => status.tone));

    return (
        <div className="space-y-6">
            <PageHeader
                title={tenantName}
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
            {tab === 'dashboard' && (
                <Dashboard
                    data={data}
                    translator={translator}
                    mayAnswerRequests={mayAnswerRequests}
                />
            )}
            {tab === 'active' && <ActiveModules />}
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
    mayAnswerRequests,
}: PanelProps & { readonly mayAnswerRequests: boolean }): ReactNode {
    return (
        <div className="grid gap-4 lg:grid-cols-2">
            <PeoplePanel data={data} translator={translator} />
            <SignInsPanel data={data} translator={translator} />
            {mayAnswerRequests && <RequestsPanel data={data} translator={translator} />}
            <PartiesPanel data={data} translator={translator} />
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

function count(value: number | undefined): string {
    return value === undefined ? '—' : String(value);
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
                    value={count(rows === undefined ? undefined : lockedAccounts(rows))}
                    label={t('home.tenantDashboard.people.locked')}
                    tone={rows !== undefined && lockedAccounts(rows) > 0 ? 'warn' : 'neutral'}
                    to="/rescue"
                />
                <Metric
                    value={count(rows === undefined ? undefined : passwordResetsDue(rows))}
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
                    value={count(failed)}
                    label={t('home.tenantDashboard.signIns.failed')}
                    tone={failed !== undefined && failed > 0 ? 'warn' : 'neutral'}
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
                    tone={queue !== undefined && queue.total > 0 ? 'warn' : 'neutral'}
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

    return (
        <Panel
            title={t('home.tenant.parties')}
            to="/parties"
            status={partiesStatus(data, translator)}
            footer={
                <>
                    <ReadAt at={data.parties.dataUpdatedAt} t={t} />
                    <LinkButton to="/parties/new" size="sm">
                        {t('home.tenant.newParty')}
                    </LinkButton>
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

function ActiveModules(): ReactNode {
    const { t } = useTranslation();
    const active: Tile[] = [
        {
            title: t('home.tenant.parties'),
            body: t('home.tenant.partiesBody'),
            to: '/parties',
            icon: 'party',
        },
        {
            title: t('home.tenant.organisation'),
            body: t('home.tenant.organisationBody'),
            to: '/organisation',
            icon: 'people',
        },
        {
            title: t('home.tenant.roles'),
            body: t('home.tenant.rolesBody'),
            to: '/roles',
            icon: 'access',
        },
        {
            title: t('home.party.refdata'),
            body: t('home.party.refdataBody'),
            to: '/refdata',
            icon: 'database',
        },
        {
            title: t('home.tenant.newParty'),
            body: t('home.tenant.newPartyBody'),
            to: '/parties/new',
            icon: 'add',
        },
        {
            title: t('home.tenant.partyDetails'),
            body: t('home.tenant.partyDetailsBody'),
            to: '/parties/details',
            icon: 'party',
        },
        {
            title: t('home.tenant.counterpartyOnboard'),
            body: t('home.tenant.counterpartyOnboardBody'),
            to: '/counterparties/onboard',
            icon: 'people',
        },
        {
            title: t('home.tenant.bookStructure'),
            body: t('home.tenant.bookStructureBody'),
            to: '/books/structure',
            icon: 'database',
        },
        {
            title: t('home.tenant.conventions'),
            body: t('home.tenant.conventionsBody'),
            to: '/conventions',
            icon: 'database',
        },
        {
            title: t('home.tenant.rescue'),
            body: t('home.tenant.rescueBody'),
            to: '/rescue',
            icon: 'unlock',
        },
        {
            title: t('home.tenant.audit'),
            body: t('home.tenant.auditBody'),
            to: '/audit',
            icon: 'record',
        },
        {
            title: t('home.tenant.versions'),
            body: t('home.tenant.versionsBody'),
            to: '/operations/versions',
            icon: 'history',
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
    const icons = { marketdata: 'chart', trading: 'trend', reporting: 'document' } as const;
    const coming: Tile[] = (['marketdata', 'trading', 'reporting'] as const).map((area) => ({
        title: t(`home.party.${area}`),
        body: t(`home.party.${area}Body`),
        icon: icons[area],
    }));

    return <Tiles tiles={coming} later={t('home.party.comingLater')} />;
}
