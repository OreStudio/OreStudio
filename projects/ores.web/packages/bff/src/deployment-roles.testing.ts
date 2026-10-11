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

import type { SessionMode } from '@ores/wire-protocol';

/** The subject a caller's own roles are read by. */
export const MY_ROLES_SUBJECT = 'iam.v1.ops.get_my_roles';

/**
 * The answer a session of a mode gets when it reads its own roles.
 *
 * A session that acts on the deployment holds the deployment read. Any other
 * session holds the wildcard `*`, which a tenant's administrator holds and
 * which does not cover an installation code, so it is the case that must be
 * refused.
 */
export function myRolesAnswer(
    mode: SessionMode,
    codes?: readonly string[],
): Record<string, unknown> {
    return {
        result: { outcome: 'ok' },
        roles: [
            {
                role: {
                    id: '99999999-9999-4999-8999-999999999999',
                    version: 1,
                    name: mode === 'system-administration' ? 'SuperAdmin' : 'TenantAdmin',
                    description: '',
                    is_registration_default: false,
                    is_requestable: false,
                },
                permission_codes:
                    codes ??
                    (mode === 'system-administration' ? ['installation::deployment:read'] : ['*']),
                assigned_by: 'system',
                assigned_at: '2026-10-05 09:30:00Z',
                change_reason_code: 'access.initial',
                change_commentary: '',
            },
        ],
    };
}
