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

import { displayName } from '../access/names.js';
import type { ReactNode } from 'react';
import type { Account, SessionMode } from '@ores/wire-protocol/browser';
import { ApplicationHome } from './ApplicationHome.js';
import { SystemHome } from './SystemHome.js';

/**
 * Where a signed-in person lands.
 *
 * Home shows the state of the work in the person's own words, and the next
 * things they can do. The mode the session runs in decides which home it is:
 * the deployment for the system administrator, and the tenant's own work for
 * everyone else. A tenant administrator and a member share that second home,
 * because the session acts inside a tenant for both; the permissions each holds
 * decide what it shows them.
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

export function HomePage({ username, partyName, mode, self }: HomePageProps): ReactNode {
    const name = displayName(self, username);
    if (mode === 'system-administration') {
        return <SystemHome name={name} />;
    }
    return <ApplicationHome name={name} partyName={partyName} />;
}
