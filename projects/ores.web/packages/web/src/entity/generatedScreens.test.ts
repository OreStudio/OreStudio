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
import { assertWiredPath, COMPONENTS } from '../components/registry.js';
import { generatedDescriptors } from './generatedEntities.js';

/**
 * Every declared screen set is complete and reachable.
 *
 * A model opts into the web facet and codegen emits a declaration; whether that
 * declaration is usable is a separate question. These cases ask it of every
 * entity at once, so an entity that opts in without columns, without a key, or
 * at a route the navigation does not declare fails here rather than at the
 * first click.
 */
describe('the generated screen sets', () => {
  it('declares at least one entity', () => {
    expect(generatedDescriptors.length).toBeGreaterThan(0);
  });

  it('gives every entity an address, a collection and something to draw', () => {
    for (const descriptor of generatedDescriptors) {
      // The address is the key the model declares, which is one member for
      // most entities and the pair a junction links. A junction declares no
      // natural key, so its metadata names none and the address is the whole
      // key.
      expect(descriptor.keyFields.length, descriptor.entity).toBeGreaterThan(0);
      expect(descriptor.meta.collection, descriptor.entity).not.toBe('');
      expect(descriptor.meta.columns.length, descriptor.entity).toBeGreaterThan(0);
      expect(descriptor.apiBase, descriptor.entity).toMatch(/^\/api\//);
    }
  });

  it('is reachable from the navigation, at the route it declares', () => {
    for (const descriptor of generatedDescriptors) {
      expect(() => assertWiredPath(descriptor), descriptor.entity).not.toThrow();
    }
  });

  it('is in the navigation as a wired entity rather than a planned one', () => {
    for (const descriptor of generatedDescriptors) {
      const component = COMPONENTS.find((c) => c.id === descriptor.component);
      const entity = component?.entities.find(
        (candidate) => candidate.path === descriptor.routeSegment,
      );
      expect(entity?.descriptor, descriptor.entity).toBeDefined();
      expect(entity?.planned, descriptor.entity).toBeUndefined();
    }
  });
});
