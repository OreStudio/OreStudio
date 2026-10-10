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

import { type IconName } from '../ui/Icon.js';
import { useHolds } from '../access/holds.js';
import { displayName } from '../access/names.js';
import type { ReactNode } from 'react';
import type { Account, SessionMode } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { PageHeader } from '../ui/Primitives.js';
import { SystemHome } from './SystemHome.js';
import { Tiles, TilesSection, type Tile } from '../ui/Tiles.js';

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

function TenantHome({
    name,
    tenantName,
}: {
    readonly name: string;
    readonly tenantName: string;
}): ReactNode {
    const { t } = useTranslation();
    const tiles: Tile[] = [
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

    return (
        <div className="space-y-6">
            <PageHeader title={tenantName} description={t('home.tenant.lead', { name })} />
            <TilesSection title={t('home.activeModules')} icon="apps">
                <Tiles tiles={tiles} />
            </TilesSection>
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
    const comingIcons: Record<'marketdata' | 'trading' | 'reporting', IconName> = {
        marketdata: 'chart',
        trading: 'trend',
        reporting: 'document',
    };
    const coming: Tile[] = (['marketdata', 'trading', 'reporting'] as const).map((area) => ({
        title: t(`home.party.${area}`),
        body: t(`home.party.${area}Body`),
        icon: comingIcons[area],
    }));

    /*
     * The places this person can go. The directory is offered only to somebody
     * who may read accounts, so the run is built rather than written out.
     */
    const active: Tile[] = [
        ...(holds('iam::accounts:read') || holds('iam::organisation:read')
            ? ([
                  {
                      title: t('home.tenant.organisation'),
                      body: t('home.tenant.organisationBody'),
                      to: '/organisation',
                      icon: 'people',
                  },
              ] satisfies Tile[])
            : []),
        {
            title: t('home.party.refdata'),
            body: t('home.party.refdataBody'),
            to: '/refdata',
            icon: 'database',
        },
        {
            title: t('home.party.security'),
            body: t('home.party.securityBody'),
            to: '/security',
            icon: 'locked',
        },
        {
            title: t('home.tenant.access'),
            body: t('home.tenant.accessBody'),
            to: '/access',
            icon: 'person',
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
    ];

    return (
        <div className="space-y-6">
            <PageHeader
                title={t('home.welcome', { name })}
                description={t('home.party.lead', { party: partyName })}
            />
            <TilesSection title={t('home.activeModules')} icon="apps">
                <Tiles tiles={active} />
            </TilesSection>
            <TilesSection title={t('home.upcomingModules')} icon="chart">
                <Tiles tiles={coming} later={t('home.party.comingLater')} />
            </TilesSection>
        </div>
    );
}
