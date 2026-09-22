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

import type { EntityMeta, FieldGroup } from '../ui-contract.js';

/**
 * Everything one entity's screens need, and nothing about how they render.
 *
 * This is generated from the entity's model, one module per entity, and the
 * shared machinery reads it. A screen never declares a route, a subject or a
 * collection of its own: if a screen needs to know something, it is here, and
 * if it is here it came from the model.
 *
 * The distinction that keeps this honest is that a descriptor holds /values/,
 * never behaviour. Anything that would be a function -- how a row is projected,
 * how a save request is built -- is derived by the shared machinery from the
 * metadata and the protocol schemas, not written per entity.
 */
export interface EntityDescriptor {
  /** The component this entity belongs to, as the registry names it. */
  readonly component: string;
  /** The entity's own identifier, as the registry names it. */
  readonly entity: string;
  /** The generated columns, fields, collection and key. */
  readonly meta: EntityMeta;
  /**
   * The entity's segment of the route, e.g. `country`.
   *
   * The component's own segment is not here: the registry owns it, because the
   * route segment a component uses is not always its model name -- market data
   * is `market-data` on the route and `marketdata` in the model. The shell
   * composes the two, so the one place that knows the component's path is the
   * one place that declares it.
   */
  readonly routeSegment: string;
  /** Where the BFF serves it, e.g. `/api/countries`. */
  readonly apiBase: string;
  /**
   * The route parameter that carries the natural key, e.g. `id`.
   *
   * The parameter's name in the URL, which is not always the field's name: a
   * route reads `/refdata/country/:id` and the key it carries is `alpha2_code`.
   */
  readonly keyParam: string;
  /** What the entity lets a person do. An absent capability is not rendered. */
  readonly capabilities: EntityCapabilities;
  /** The fields the list's search reaches across. */
  readonly searchFields: readonly string[];
  /**
   * How the detail form's fields group into tabs.
   *
   * Emitted when the model declares a grouping. Absent means the form shows the
   * fields in one group, which is what an entity with no grouping means rather
   * than a defect.
   */
  readonly fieldGroups?: readonly FieldGroup[];
}

export interface EntityCapabilities {
  readonly create: boolean;
  readonly edit: boolean;
  readonly remove: boolean;
  /** True when the entity is temporal and the service serves its history. */
  readonly history: boolean;
}
