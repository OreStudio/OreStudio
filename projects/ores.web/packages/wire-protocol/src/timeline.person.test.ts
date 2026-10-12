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

import { beforeEach, describe, expect, it, vi } from 'vitest';
import type { AuthenticatedCaller } from './account-operations.js';

const reads = vi.hoisted(() => ({
    history: vi.fn(),
    roles: vi.fn(),
}));

vi.mock('./credentials.js', () => ({
    readAccount: vi.fn(async () => ({ id: 'acct-1', username: 'ana', email: '' })),
    readAuthEvents: vi.fn(async () => []),
}));
vi.mock('./profile-operations.js', () => ({ readContactInformation: vi.fn(async () => null) }));
vi.mock('./access.js', () => ({ readRoles: reads.roles }));
vi.mock('./classifications.js', async (importOriginal) => ({
    ...(await importOriginal<typeof import('./classifications.js')>()),
    readEntityHistory: reads.history,
}));

import { readPersonTimeline } from './timeline.js';

function linkVersion(number: number, closedAt = '') {
    return {
        version: number,
        modifiedBy: 'priya',
        recordedAt: `2026-10-0${String(number)}T10:00:00Z`,
        fields: [
            { name: 'ID', value: 'acct-1/role-1' },
            { name: 'Account', value: 'acct-1' },
            { name: 'Role', value: 'role-1' },
            { name: 'Closed At', value: closedAt },
            { name: 'Change Reason Code', value: 'system.new_record' },
        ],
        changes: { entries: [] },
    };
}

describe('a person’s roles in their timeline', () => {
    beforeEach(() => {
        reads.history.mockReset();
        reads.roles.mockReset();
        reads.roles.mockResolvedValue([
            { id: 'role-1', name: 'Auditor', description: 'Reads the books' },
        ]);
    });

    it('shows a role given, and a role taken away, by the role’s name', async () => {
        reads.history.mockImplementation(async (_caller: unknown, entity: string) =>
            entity === 'ores.iam.account_role'
                ? [linkVersion(1), linkVersion(2, '2026-10-05T10:00:00Z')]
                : [],
        );

        const story = await readPersonTimeline({} as AuthenticatedCaller, 'ana');

        const roles = story.events.filter((e) => e.entityType === 'ores.iam.account_role');
        expect(roles.map((e) => e.kind)).toEqual(['closed', 'changed', 'granted']);
        expect(roles[0]?.actor).toBe('');
        expect(roles[2]?.actor).toBe('priya');
        expect(roles[2]?.fields.find((f) => f.name === 'Role')?.value).toBe('Auditor');
        expect(story.gaps.map((g) => g.entity)).not.toContain('ores.iam.account_role');
    });

    it('states a gap for the roles only when they could not be read', async () => {
        reads.history.mockImplementation(async (_caller: unknown, entity: string) => {
            if (entity === 'ores.iam.account_role') throw new Error('refused');
            return [];
        });

        const story = await readPersonTimeline({} as AuthenticatedCaller, 'ana');

        expect(story.gaps.map((g) => g.entity)).toContain('ores.iam.account_role');
    });
});
