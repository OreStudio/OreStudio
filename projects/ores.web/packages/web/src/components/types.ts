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

/**
 * What a component is.
 *
 * A component owns a set of entities and its own place in the navigation. It is
 * declared once, and the sidebar, the routes, the breadcrumbs and the document
 * title all derive from the declaration rather than being written per component.
 *
 * That is the point: with a hundred entities, the thing that costs is not any one
 * screen, it is the same decision being made a hundred times and drifting.
 */
import type { IconName } from '../ui/icons/index.js';

/** A shortcut card on a component's landing page. */
export interface ShortcutDefinition {
  readonly id: string;
  /** Catalogue keys, when the card has words. Most fall back to the identifier. */
  readonly titleKey?: string;
  readonly descriptionKey?: string;
  readonly icon: IconName;
  /** Where it goes. Internal paths only. */
  readonly to: string;
  /** True when the destination is not built yet, so the card says so. */
  readonly planned?: boolean;
}

export interface EntityDefinition {
  /**
   * Stable key, used in URLs and query keys, and the fallback for the name.
   *
   * The name and description come from the catalogue under `entity.<id>` when a
   * translation exists, and from the humanised identifier when it does not. Most
   * entities are declared before they are built, so requiring a translation for
   * each would mean inventing words nobody chose.
   */
  readonly id: string;
  readonly icon: IconName;
  /** The route segment. */
  readonly path: string;
  /** True when there is no screen for it yet. */
  readonly planned?: boolean;
}

export interface ComponentDefinition {
  readonly id: string;
  /** Catalogue key for the component's name. */
  readonly titleKey: string;
  readonly icon: IconName;
  /** Where the component itself lives. */
  readonly path: string;
  readonly entities: readonly EntityDefinition[];
  /** Cards shown on the component's landing page. */
  readonly shortcuts?: readonly ShortcutDefinition[];
  /**
   * Reached from the sidebar footer rather than the component list, because it
   * belongs to the deployment rather than to a component.
   */
  readonly group?: 'platform';
}

/** An entity plus the component it belongs to, which is what a route needs. */
export interface ResolvedEntity {
  readonly component: ComponentDefinition;
  readonly entity: EntityDefinition;
}
