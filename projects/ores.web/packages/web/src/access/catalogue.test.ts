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

import { describe, expect, it } from 'vitest';
import type { HeldRole } from '@ores/wire-protocol/browser';
import {
    areasOf,
    countCovered,
    covers,
    grantsInArea,
    parseCode,
    rolesGranting,
    search,
} from './catalogue.js';

const CATALOGUE = [
    { code: '*', description: 'Full access to all operations' },
    { code: 'refdata::*', description: 'Full access to reference data' },
    { code: 'refdata::currencies:read', description: 'View currencies' },
    { code: 'refdata::currencies:write', description: 'Create and modify currencies' },
    { code: 'refdata::currencies:delete', description: 'Delete currencies' },
    { code: 'refdata::countries:read', description: 'View countries' },
    { code: 'iam::accounts:read', description: 'View user account details' },
    { code: 'iam::accounts:reset_password', description: 'Force password reset' },
];

function held(name: string, codes: string[]): HeldRole {
    return {
        roleId: '11111111-1111-1111-1111-111111111111' as HeldRole['roleId'],
        name,
        description: '',
        permissionCodes: codes,
        givenBy: 'priya',
        givenAt: '',
        reasonCode: '',
        commentary: '',
    };
}

describe('the permission catalogue', () => {
    it('reads a code as its area, what it is about and the action', () => {
        expect(parseCode('iam::accounts:reset_password')).toEqual({
            component: 'iam',
            resource: 'accounts',
            action: 'reset_password',
        });
        expect(parseCode('refdata::*').resource).toBe('*');
    });

    it('groups the catalogue by area, the largest first, leaving the wildcards out', () => {
        const areas = areasOf(CATALOGUE);

        expect(areas.map((a) => [a.component, a.size])).toEqual([
            ['refdata', 4],
            ['iam', 2],
        ]);
        expect(areas[0]?.resources.map((r) => [r.name, r.actions])).toEqual([
            ['countries', ['read']],
            ['currencies', ['read', 'write', 'delete']],
        ]);
    });

    it('counts a wildcard as covering what it names', () => {
        expect(covers(new Set(['*']), 'iam::accounts:read')).toBe(true);
        expect(covers(new Set(['refdata::*']), 'refdata::countries:read')).toBe(true);
        expect(covers(new Set(['refdata::*']), 'iam::accounts:read')).toBe(false);
        expect(countCovered(new Set(['refdata::*']), CATALOGUE)).toBe(4);
    });

    it('names the roles that grant a code', () => {
        const roles = [
            held('Viewer', ['refdata::currencies:read']),
            held('Operations', ['refdata::*']),
        ];

        expect(rolesGranting(roles, 'refdata::currencies:read')).toEqual(['Viewer', 'Operations']);
        expect(rolesGranting(roles, 'iam::accounts:read')).toEqual([]);
    });

    it('answers a question in a person words or in the code', () => {
        expect(search(CATALOGUE, 'delete currencies', 5).map((e) => e.code)).toEqual([
            'refdata::currencies:delete',
        ]);
        expect(search(CATALOGUE, 'reset password', 5).map((e) => e.code)).toEqual([
            'iam::accounts:reset_password',
        ]);
        expect(search(CATALOGUE, '  ', 5)).toEqual([]);
    });
});

describe('grantsInArea', () => {
    it('is true for the wildcard and for any code of the area', () => {
        expect(grantsInArea(['*'], 'refdata')).toBe(true);
        expect(grantsInArea(['refdata::*'], 'refdata')).toBe(true);
        expect(grantsInArea(['refdata::currencies:read'], 'refdata')).toBe(true);
    });

    it('is false for a role that reaches other areas only', () => {
        expect(grantsInArea(['iam::accounts:read'], 'refdata')).toBe(false);
        expect(grantsInArea([], 'refdata')).toBe(false);
    });
});
