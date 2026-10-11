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

import { DEPLOYMENT_READ, permissionCovers } from '@ores/contracts';
import { readMyAccess } from '@ores/wire-protocol';
import { notPermitted } from './errors.js';
import type { LiveSession } from './sessions.js';

/**
 * Refuses a caller who does not act on the deployment as a whole.
 *
 * The deployment's tenants, services, grid, bus and logs are read across every
 * tenant. A session says it acts on the deployment by holding
 * `installation::deployment:read`. A tenant's administrator holds the wildcard
 * `*`, and the wildcard does not cover an installation code, so only an
 * account that was given the read, or the installation component's wildcard,
 * passes. The roles are read for each call, because a grant the caller lost
 * must stop the next read and not the next sign-in.
 *
 * @param what The words that complete the refusal: what the caller tried to read.
 */
export async function requireDeploymentRead(session: LiveSession, what: string): Promise<void> {
    const access = await readMyAccess(session.client);
    const granted = access.roles.flatMap((role) => role.permissionCodes);
    if (!permissionCovers(granted, DEPLOYMENT_READ)) {
        throw notPermitted(`${what} needs the read of the deployment as a whole.`);
    }
}
