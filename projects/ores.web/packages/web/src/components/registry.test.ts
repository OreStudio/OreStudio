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
import { tenantTypeDescriptor } from '../generated/iam/web/tenant_type_declaration.js';
import { assertWiredPath, findEntity, iamComponent } from './registry.js';

/*
 * The sidebar links a wired entity at the registry's path and the router serves
 * it at the declaration's route segment. These cases are that the two are one
 * value: the registry resolves the declaration's segment, and a declaration the
 * registry does not mirror is refused rather than linked to nothing.
 */
describe('wired entity paths', () => {
  it('resolves the registry entity at the declaration route segment', () => {
    expect(tenantTypeDescriptor.routeSegment).toBe('tenant-type');
    const resolved = findEntity(iamComponent.id, tenantTypeDescriptor.routeSegment);
    expect(resolved?.entity.id).toBe('tenantType');
    expect(() => assertWiredPath(tenantTypeDescriptor)).not.toThrow();
  });

  it('refuses a declaration whose route segment the registry does not declare', () => {
    const disagreeing = { ...tenantTypeDescriptor, routeSegment: 'tenant-types' };
    expect(() => assertWiredPath(disagreeing)).toThrow(/tenant-types/);
  });
});
