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
 * prototype renders with no session and no server, so a reviewer starts the dev
 * server and opens one URL.
 *
 *   npm run dev:web --workspace @ores/web
 *   http://localhost:5173/prototype/security?variant=a
 */

import type { ReactNode } from 'react';
import { AppShell } from '../components/AppShell.js';
import { Notice } from '../ui/Primitives.js';
import { ProtectMyAccountPrototype, PrototypeAccountStrip } from './ProtectMyAccountPrototype.js';
import { RescueAccessPrototype } from './RescueAccessPrototype.js';

const PREFIX = '/prototype';

export function isPrototypePath(pathname: string): boolean {
    return pathname === PREFIX || pathname.startsWith(`${PREFIX}/`);
}

export function PrototypeApp({ pathname }: { readonly pathname: string }): ReactNode {
    return (
        <AppShell
            username="amara.okafor"
            tenantName="Acme Corporation"
            partyName="Acme UK"
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
    if (pathname === `${PREFIX}/security`) {
        return (
            <>
                <PrototypeAccountStrip />
                <ProtectMyAccountPrototype />
            </>
        );
    }
    if (pathname === `${PREFIX}/rescue`) {
        return <RescueAccessPrototype />;
    }
    return (
        <div className="mx-auto max-w-[700px] space-y-4">
            <Notice tone="warn">PROTOTYPE. Throwaway. Nothing on these routes calls the server.</Notice>
            <div className="card space-y-2 p-6 text-sm">
                <h1 className="text-lg font-medium">Credentials prototypes</h1>
                <p className="text-ink-muted">One route per journey:</p>
                <ul className="space-y-1 font-mono">
                    <li>
                        <a className="text-accent" href="/prototype/security?variant=a">
                            /prototype/security
                        </a>{' '}
                        — Protect my account
                    </li>
                    <li>
                        <a className="text-accent" href="/prototype/rescue?variant=a">
                            /prototype/rescue
                        </a>{' '}
                        — Rescue access
                    </li>
                </ul>
            </div>
        </div>
    );
}
