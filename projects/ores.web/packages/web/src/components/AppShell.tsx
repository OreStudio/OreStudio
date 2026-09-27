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
import { Link } from 'react-router';
import { useTranslation } from '../i18n/Provider.js';
import { headerMark } from '../assets/brand.js';
import { Button } from '../ui/Primitives.js';

/**
 * The shell a signed-in person gets.
 *
 * The session is passed in rather than read here, so the shell renders from its
 * props and a test can render it without a server or a session.
 *
 * On the web the navigation is a place a person can see and a screen is a route
 * they can link to; the navigation itself is what the journeys add, so this
 * carries the session and the way out of it. Signing out is the one action that
 * belongs on every screen.
 */
export interface AppShellProps {
    readonly username: string;
    readonly tenantName: string;
    readonly partyName: string | undefined;
    readonly onSignOut: () => void;
    readonly children: ReactNode;
}

export function AppShell({
    username,
    tenantName,
    partyName,
    onSignOut,
    children,
}: AppShellProps): ReactNode {
    const { t } = useTranslation();

    return (
        <div className="flex min-h-full flex-col bg-bg-primary">
            <header className="border-b border-line">
                <div className="flex items-center gap-3 px-5 py-4">
                    <Link to="/" className="flex items-center gap-3">
                        <img src={headerMark} alt="" className="h-7 w-auto" />
                        <span className="text-sm font-semibold tracking-tight text-ink">
                            {t('app.name')}
                        </span>
                    </Link>
                    <div className="ml-auto flex items-center gap-3 text-xs text-ink-muted">
                        <span>
                            {t('shell.session', { username, tenant: tenantName })}
                            {partyName === undefined ? '' : ` · ${partyName}`}
                        </span>
                        <Button variant="ghost" size="sm" onClick={onSignOut}>
                            {t('nav.signOut')}
                        </Button>
                    </div>
                </div>
            </header>
            <main className="min-w-0 flex-1 overflow-y-auto px-5 py-8">{children}</main>
        </div>
    );
}
