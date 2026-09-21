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

import { findComponent } from '../components/registry.js';
import type { EntityDescriptor } from './descriptor.js';

/**
 * Where an entity's screens live.
 *
 * The two halves of the path come from two declarations and neither knows the
 * other. The entity's segment is the descriptor's, because the model carries it.
 * The component's segment is the registry's, because a component's route segment
 * is not always its model name -- market data is `market-data` on the route and
 * `marketdata` in the model.
 *
 * Composing them here is what keeps the sidebar, the breadcrumbs and the router
 * from each inventing a path. A link built anywhere else is a link that can
 * disagree with the route it points at.
 */
export function entityBasePath(descriptor: EntityDescriptor): string {
  const component = findComponent(descriptor.component);
  const componentPath = component?.path ?? descriptor.component;
  return `/${componentPath}/${descriptor.routeSegment}`;
}

/** One record's path. The key is a URL segment, so it is encoded. */
export function entityRecordPath(descriptor: EntityDescriptor, key: string): string {
  return `${entityBasePath(descriptor)}/${encodeURIComponent(key)}`;
}
