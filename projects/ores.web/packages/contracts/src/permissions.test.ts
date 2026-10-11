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
import { DEPLOYMENT_READ, permissionCovers } from './permissions.js';

describe('permissionCovers', () => {
    it('is covered by the code itself, by everything, and by the wildcard of its component', () => {
        expect(permissionCovers(['refdata::parties:read'], 'refdata::parties:read')).toBe(true);
        expect(permissionCovers(['*'], 'refdata::parties:read')).toBe(true);
        expect(permissionCovers(['refdata::*'], 'refdata::parties:read')).toBe(true);
    });

    it('is not covered by another code or another component', () => {
        expect(permissionCovers(['refdata::parties:write'], 'refdata::parties:read')).toBe(false);
        expect(permissionCovers(['iam::*'], 'refdata::parties:read')).toBe(false);
        expect(permissionCovers([], 'refdata::parties:read')).toBe(false);
    });

    it('does not let everything cover the deployment read', () => {
        expect(permissionCovers(['*'], DEPLOYMENT_READ)).toBe(false);
        expect(permissionCovers(['iam::*', 'refdata::*'], DEPLOYMENT_READ)).toBe(false);
    });

    it('lets the code itself and the wildcard of its component cover the deployment read', () => {
        expect(permissionCovers([DEPLOYMENT_READ], DEPLOYMENT_READ)).toBe(true);
        expect(permissionCovers(['installation::*'], DEPLOYMENT_READ)).toBe(true);
    });
});
