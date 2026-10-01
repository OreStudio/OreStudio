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

import { SYSTEM_TENANT_ID } from '@ores/wire-protocol';
import type { SessionMode } from '@ores/wire-protocol';

/**
 * The context a session runs in, decided once when the session is opened.
 *
 * The browser must not derive this. A menu built from a value the client
 * inferred is a menu that can disagree with the data, and the disagreement is
 * invisible until somebody is shown a door they may not open. So the rule lives
 * here, on the server side of the boundary, and the browser receives the
 * answer.
 *
 * The rule is the session's tenant, because that is the context the service
 * scopes every request by, and it is the fact the login already answers with.
 * A super administrator's account belongs to the system tenant, so the session
 * acts on the deployment; every other account acts inside a tenant.
 *
 * *Tenant administration* is declared and not yet produced. That context is a
 * session acting as a tenant rather than as one of its parties, which does not
 * exist yet. It arrives with the tenant-scoped session, not with a new rule
 * here. When `ores.iam` states the mode on the login answer, this function goes
 * and the answer is passed through unchanged.
 */
export function sessionModeFor(tenantId: string): SessionMode {
    return tenantId === SYSTEM_TENANT_ID ? 'system-administration' : 'application';
}
