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

import { fieldValue, type Timeline as Stream } from '@ores/wire-protocol/browser';

/** The entity whose versions carry a person's reporting line. */
const ACCOUNT_ENTITY = 'ores.iam.account';

/** The field of an account version that names the manager. */
const MANAGER_FIELD = 'Reports To Account ID';

/** One change of a person's reporting line, as the history list states it. */
export interface LineChange {
    readonly at: string;
    readonly actor: string;
    readonly from: string;
    readonly to: string;
}

/**
 * The versions of a person's account where the manager moved, newest first.
 *
 * The line is a field of the account, so a change of it is a version whose
 * manager differs from the version before. The first version is the account's
 * creation, which is not a change of a line that was never set before it. A
 * manager is named by the caller, so the list reads as people.
 */
export function lineChanges(
    source: Stream | undefined,
    nameFor: (accountId: string | null) => string,
): readonly LineChange[] {
    const versions = (source?.events ?? [])
        .filter((event) => event.entityType === ACCOUNT_ENTITY)
        .sort((a, b) => a.version - b.version);
    const changes: LineChange[] = [];
    for (let index = 1; index < versions.length; index += 1) {
        const before = fieldValue(versions[index - 1]?.fields ?? [], MANAGER_FIELD);
        const event = versions[index];
        if (event === undefined) continue;
        const after = fieldValue(event.fields, MANAGER_FIELD);
        if (before === after) continue;
        changes.push({
            at: event.at,
            actor: event.actor,
            from: nameFor(before === '' ? null : before),
            to: nameFor(after === '' ? null : after),
        });
    }
    return changes.reverse();
}
