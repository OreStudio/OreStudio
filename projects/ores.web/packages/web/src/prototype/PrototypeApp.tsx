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

/*
 * PROTOTYPE. Kept on main as the design record of the journeys it drew.
 *
 * The prototypes' own route table, reached before the application's gate. A
 * prototype renders with no session and no server, so a reviewer opens one URL
 * on the standard web server of the environment:
 *
 *   http://127.0.0.1:20402/prototype
 *
 * Each route draws one journey as its prototype was accepted. The shell around
 * it states who the journey's actor is, so the menu shows the context that
 * person works in.
 */

import type { ReactNode } from 'react';
import type { SessionMode } from '@ores/wire-protocol/browser';
import { AppShell } from '../components/AppShell.js';
import { PublicShell } from '../components/PublicShell.js';
import type { ShellWidth } from '../shell/layout.js';
import { Notice } from '../ui/Primitives.js';
import {
    FirstRunJourneyPrototype,
    NewTenantJourneyPrototype,
} from '../pages/prototype/newTenantJourney/NewTenantJourneyPrototype.js';
import { NewPartyJourneyPrototype } from '../pages/prototype/newTenantJourney/NewPartyJourneyPrototype.js';
import { ProtectMyAccountPrototype, PrototypeAccountStrip } from './ProtectMyAccountPrototype.js';
import { RescueAccessPrototype } from './RescueAccessPrototype.js';
import { AuditSignInsPrototype } from './AuditSignInsPrototype.js';
import { ServicesPrototype } from './ServicesPrototype.js';
import { GridPrototype } from './GridPrototype.js';
import { BusPrototype } from './BusPrototype.js';
import { LogsPrototype } from './LogsPrototype.js';
import { VersionsPrototype } from './VersionsPrototype.js';

const PREFIX = '/prototype';

interface SignedIn {
    readonly username: string;
    readonly tenantName: string;
    readonly partyName: string | undefined;
    readonly mode: SessionMode;
    readonly width: ShellWidth;
}

const MEMBER: SignedIn = {
    username: 'amara.okafor',
    tenantName: 'Acme Corporation',
    partyName: 'Acme UK',
    mode: 'application',
    width: 'column',
};

const TENANT_ADMINISTRATOR: SignedIn = { ...MEMBER, mode: 'tenant-administration' };

const SYSTEM_ADMINISTRATOR: SignedIn = {
    username: 'sysadmin',
    tenantName: 'System',
    partyName: undefined,
    mode: 'system-administration',
    width: 'workspace',
};

/** Where a prototype renders: a signed-in shell, or the public one before any sign-in. */
type Audience = SignedIn | 'public';

interface PrototypeRoute {
    readonly path: string;
    readonly query?: string;
    readonly group: string;
    readonly journey: string;
    readonly audience: Audience;
    readonly render: () => ReactNode;
}

const ROUTES: readonly PrototypeRoute[] = [
    {
        path: 'setup',
        group: 'Onboarding',
        journey: 'First run',
        audience: 'public',
        render: () => <FirstRunJourneyPrototype />,
    },
    {
        path: 'tenant',
        group: 'Onboarding',
        journey: 'New tenant',
        audience: 'public',
        render: () => <NewTenantJourneyPrototype />,
    },
    {
        path: 'party',
        group: 'Onboarding',
        journey: 'New party',
        audience: 'public',
        render: () => <NewPartyJourneyPrototype />,
    },
    {
        path: 'security',
        query: '?variant=b',
        group: 'Credentials',
        journey: 'Protect my account',
        audience: MEMBER,
        render: () => (
            <>
                <PrototypeAccountStrip />
                <ProtectMyAccountPrototype />
            </>
        ),
    },
    {
        path: 'rescue',
        query: '?variant=b',
        group: 'Credentials',
        journey: 'Rescue access',
        audience: TENANT_ADMINISTRATOR,
        render: () => <RescueAccessPrototype />,
    },
    {
        path: 'audit',
        query: '?variant=b',
        group: 'Credentials',
        journey: 'Audit sign-ins',
        audience: TENANT_ADMINISTRATOR,
        render: () => <AuditSignInsPrototype />,
    },
    {
        path: 'services',
        group: 'Operations',
        journey: 'See the running services',
        audience: SYSTEM_ADMINISTRATOR,
        render: () => <ServicesPrototype />,
    },
    {
        path: 'grid',
        group: 'Operations',
        journey: 'Watch the compute grid',
        audience: SYSTEM_ADMINISTRATOR,
        render: () => <GridPrototype />,
    },
    {
        path: 'bus',
        group: 'Operations',
        journey: 'Watch the message bus',
        audience: SYSTEM_ADMINISTRATOR,
        render: () => <BusPrototype />,
    },
    {
        path: 'logs',
        group: 'Operations',
        journey: 'Read the telemetry logs',
        audience: SYSTEM_ADMINISTRATOR,
        render: () => <LogsPrototype />,
    },
    {
        path: 'versions',
        group: 'Operations',
        journey: 'Check the versions and the database',
        audience: SYSTEM_ADMINISTRATOR,
        render: () => <VersionsPrototype />,
    },
];

export function isPrototypePath(pathname: string): boolean {
    return pathname === PREFIX || pathname.startsWith(`${PREFIX}/`);
}

export function PrototypeApp({ pathname }: { readonly pathname: string }): ReactNode {
    const path = pathname.replace(/\/+$/, '');
    const route = ROUTES.find((candidate) => path === `${PREFIX}/${candidate.path}`);
    if (route === undefined) {
        return (
            <PublicShell wide serverVersion="prototype">
                <PrototypeIndex />
            </PublicShell>
        );
    }
    if (route.audience === 'public') {
        return (
            <PublicShell wide serverVersion="prototype">
                {route.render()}
            </PublicShell>
        );
    }
    const { username, tenantName, partyName, mode, width } = route.audience;
    return (
        <AppShell
            username={username}
            tenantName={tenantName}
            partyName={partyName}
            mode={mode}
            width={width}
            serverVersion="prototype"
            onSignOut={() => {
                window.location.assign('/');
            }}
        >
            {route.render()}
        </AppShell>
    );
}

function PrototypeIndex(): ReactNode {
    const groups = [...new Set(ROUTES.map((route) => route.group))];
    return (
        <div className="mx-auto max-w-[700px] space-y-4">
            <Notice tone="warn">PROTOTYPE. Nothing on these routes calls the server.</Notice>
            {groups.map((group) => (
                <div key={group} className="card space-y-2 p-6 text-sm">
                    <h1 className="text-lg font-medium">{group} prototypes</h1>
                    <ul className="space-y-1 font-mono">
                        {ROUTES.filter((route) => route.group === group).map((route) => (
                            <li key={route.path}>
                                <a
                                    className="text-accent"
                                    href={`${PREFIX}/${route.path}${route.query ?? ''}`}
                                >
                                    {`${PREFIX}/${route.path}`}
                                </a>{' '}
                                — {route.journey}
                            </li>
                        ))}
                    </ul>
                </div>
            ))}
        </div>
    );
}
