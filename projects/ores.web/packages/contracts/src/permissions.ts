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

/**
 * The component whose permissions a wildcard never covers.
 *
 * `installation::` codes say a session acts on the deployment as a whole,
 * across every tenant. A tenant's administrator holds the wildcard `*`, and a
 * wildcard that covered these codes would show them the installation. Only the
 * code itself, or `installation::*`, grants one.
 */
export const INSTALLATION_COMPONENT = 'installation';

/** The read that says a session acts on the deployment: its tenants, services, grid, bus and logs. */
export const DEPLOYMENT_READ = 'installation::deployment:read';

/**
 * Whether a set of granted permission codes covers a required one.
 *
 * A grant covers a code when it is the code itself, the wildcard `*`, or a
 * component wildcard such as `refdata::*`. The wildcard `*` does not cover an
 * `installation::` code. This is the one rule the browser and the BFF use, and
 * it is the rule `ores::security::authorization::grants` states for the
 * services.
 */
export function permissionCovers(granted: Iterable<string>, required: string): boolean {
    const component = required.split('::')[0] ?? '';
    const gated = component === INSTALLATION_COMPONENT;
    for (const grant of granted) {
        if (grant === required || grant === `${component}::*`) {
            return true;
        }
        if (grant === '*' && !gated) {
            return true;
        }
    }
    return false;
}
