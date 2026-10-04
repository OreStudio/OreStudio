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
 * PROTOTYPE. Throwaway. Delete with the branch.
 *
 * The prototype's own route table, reached before the application's gate. A
 * prototype renders with no session and no server, so a reviewer opens one URL
 * on the standard web server of the environment:
 *
 *   http://127.0.0.1:20402/prototype
 *
 * Operations is an area, like Tenants or Rescue: this route is the area's hub,
 * each journey has its own route, and every screen carries the way back here.
 */

import type { ReactNode } from 'react';
import { AppShell } from '../components/AppShell.js';
import { Notice } from '../ui/Primitives.js';
import { ServicesPrototype } from './ServicesPrototype.js';
import { GridPrototype } from './GridPrototype.js';
import { BusPrototype } from './BusPrototype.js';
import { LogsPrototype } from './LogsPrototype.js';
import { VersionsPrototype } from './VersionsPrototype.js';

const PREFIX = '/prototype';

export function isPrototypePath(pathname: string): boolean {
    return pathname === PREFIX || pathname.startsWith(`${PREFIX}/`);
}

export function PrototypeApp({ pathname }: { readonly pathname: string }): ReactNode {
    return (
        <AppShell
            username="sysadmin"
            tenantName="System"
            partyName={undefined}
            mode="system-administration"
            width="workspace"
            serverVersion="prototype"
            onSignOut={() => {
                window.location.assign('/');
            }}
        >
            {screenFor(pathname)}
        </AppShell>
    );
}

function screenFor(pathname: string): ReactNode {
    if (pathname === `${PREFIX}/services`) {
        return <ServicesPrototype />;
    }
    if (pathname === `${PREFIX}/grid`) {
        return <GridPrototype />;
    }
    if (pathname === `${PREFIX}/bus`) {
        return <BusPrototype />;
    }
    if (pathname === `${PREFIX}/logs`) {
        return <LogsPrototype />;
    }
    if (pathname === `${PREFIX}/versions`) {
        return <VersionsPrototype />;
    }
    return (
        <div className="mx-auto max-w-[700px] space-y-4">
            <Notice tone="warn">PROTOTYPE. Throwaway. Nothing on these routes calls the server.</Notice>
            <div className="card space-y-2 p-6 text-sm">
                <h1 className="text-lg font-medium">Operations prototypes</h1>
                <p className="text-ink-muted">
                    The operations area. One route per journey; each screen carries the way back
                    here, the same way a tenant detail carries the way back to the roster. Every
                    screen is a fixture shaped by the operations the journey names, and every gap
                    the journey records is shown on the screen it belongs to.
                </p>
                <ul className="space-y-1 font-mono">
                    <li>
                        <a className="text-accent" href="/prototype/services">
                            /prototype/services
                        </a>{' '}
                        — See the running services
                    </li>
                    <li>
                        <a className="text-accent" href="/prototype/grid">
                            /prototype/grid
                        </a>{' '}
                        — Watch the compute grid
                    </li>
                    <li>
                        <a className="text-accent" href="/prototype/bus">
                            /prototype/bus
                        </a>{' '}
                        — Watch the message bus
                    </li>
                    <li>
                        <a className="text-accent" href="/prototype/logs">
                            /prototype/logs
                        </a>{' '}
                        — Read the telemetry logs
                    </li>
                    <li>
                        <a className="text-accent" href="/prototype/versions">
                            /prototype/versions
                        </a>{' '}
                        — Check the versions and the database
                    </li>
                </ul>
            </div>
        </div>
    );
}
