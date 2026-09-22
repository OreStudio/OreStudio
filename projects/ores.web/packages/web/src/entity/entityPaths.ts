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

/**
 * One record's path: one URL segment per key member, each encoded.
 *
 * The segments are the key's own members rather than one synthetic value,
 * because that is what the route serves and what the request fills. A
 * junction states two, so addressing it takes two.
 */
export function entityRecordPath(
  descriptor: EntityDescriptor,
  row: Readonly<Record<string, unknown>>,
): string {
  const segments = descriptor.keyFields.map((field) =>
    encodeURIComponent(String(row[field] ?? '')),
  );
  return `${entityBasePath(descriptor)}/${segments.join('/')}`;
}

/**
 * The key a route's parameters state, as the record a request fills.
 *
 * The route names one parameter per key member, so reading them back is the
 * inverse of building the path and the two cannot drift.
 */
export function keyFromParams(
  descriptor: EntityDescriptor,
  params: Readonly<Record<string, string | undefined>>,
): Record<string, string> {
  const key: Record<string, string> = {};
  for (const field of descriptor.keyFields) {
    key[field] = params[field] ?? '';
  }
  return key;
}

/** The key a form's values state, as the record the request fills. */
export function recordKeyFromValues(
  descriptor: EntityDescriptor,
  values: Readonly<Record<string, unknown>>,
): Record<string, string> {
  const key: Record<string, string> = {};
  for (const field of descriptor.keyFields) {
    key[field] = String(values[field] ?? '');
  }
  return key;
}

/**
 * A record named for a person: its key members' values, joined.
 *
 * A confirmation that says which row it is about names the whole key, because
 * half a junction's key names one side of the link and not the link.
 */
export function recordLabel(
  descriptor: EntityDescriptor,
  row: Readonly<Record<string, unknown>>,
): string {
  return descriptor.keyFields
    .map((field) => String(row[field] ?? ''))
    .join(' / ');
}
