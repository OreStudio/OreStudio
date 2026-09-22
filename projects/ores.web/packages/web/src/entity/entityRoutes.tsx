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

import type { ReactNode } from 'react';
import { Route } from 'react-router';
import { assertWiredPath } from '../components/registry.js';
import { EntityDetailContainer } from './EntityDetailContainer.js';
import { EntityHistoryContainer } from './EntityHistoryContainer.js';
import { EntityListContainer } from './EntityListContainer.js';
import type { EntityDescriptor } from './descriptor.js';

/**
 * The routes one entity adds, built from its declaration.
 *
 * Five at most, and which of them exist is the declaration's answer: an entity
 * that cannot be created gets no create route, so a typed URL reaches nothing
 * rather than a form that will be refused. The paths are composed from the
 * component's segment and the entity's own, so an entity adds routes by being
 * declared and not by being routed.
 *
 * `componentPath` is the registry's segment for the component -- the one the
 * sidebar and the breadcrumbs link with -- so the routes and the links cannot
 * be built from two different ideas of where the component lives.
 *
 * The record route is a route rather than a mode of the list, because a person
 * who deep-links to a record, or comes back to one, expects the back button to
 * mean something.
 */
export function entityRoutes(
  descriptor: EntityDescriptor,
  componentPath: string,
): readonly ReactNode[] {
  assertWiredPath(descriptor);
  const base = `/${componentPath}/${descriptor.routeSegment}`;
  const record = `${base}/:${descriptor.keyParam}`;

  return [
    <Route
      key="list"
      path={base}
      element={<EntityListContainer descriptor={descriptor} />}
    />,
    ...(descriptor.capabilities.create
      ? [
          <Route
            key="create"
            path={`${base}/new`}
            element={<EntityDetailContainer descriptor={descriptor} mode="create" />}
          />,
        ]
      : []),
    <Route
      key="detail"
      path={record}
      element={<EntityDetailContainer descriptor={descriptor} mode="read" />}
    />,
    ...(descriptor.capabilities.edit
      ? [
          <Route
            key="edit"
            path={`${record}/edit`}
            element={<EntityDetailContainer descriptor={descriptor} mode="edit" />}
          />,
        ]
      : []),
    ...(descriptor.capabilities.history
      ? [
          <Route
            key="history"
            path={`${record}/history`}
            element={<EntityHistoryContainer descriptor={descriptor} />}
          />,
        ]
      : []),
  ];
}
