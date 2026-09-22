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

import type { Catalogue } from '../i18n/translate.js';
import type { EntityDescriptor } from './descriptor.js';

/**
 * Every screen set the models declare, and every word they state.
 *
 * A model opts into the web facet, and codegen emits a declaration and a set of
 * messages beside it. This module collects them, so an entity that opts in
 * appears in the navigation, gets its routes and reads its own labels without
 * anybody adding a line here. That is the point of the collection: the model
 * is the declaration, and a second place to declare it is a place to forget.
 *
 * The collection is a build-time glob rather than a generated list because the
 * bundler already knows the directory; a generated index would be one more
 * artefact to keep in step with the files it indexes.
 */

/** What a declaration module exports: one descriptor, under its own name. */
type DeclarationModule = Readonly<Record<string, unknown>>;

/** What a metadata module exports, including its messages. */
type MetadataModule = Readonly<Record<string, unknown>>;

const screenedEntities = new Set<string>();

const declarations = import.meta.glob<DeclarationModule>(
  '../generated/*/web/*_declaration.ts',
  { eager: true },
);

const metadata = import.meta.glob<MetadataModule>(
  '../generated/*/ui/*_ui.ts',
  { eager: true },
);

function isDescriptor(value: unknown): value is EntityDescriptor {
  if (typeof value !== 'object' || value === null) return false;
  const candidate = value as Record<string, unknown>;
  return (
    typeof candidate['component'] === 'string' &&
    typeof candidate['entity'] === 'string' &&
    typeof candidate['routeSegment'] === 'string' &&
    typeof candidate['apiBase'] === 'string' &&
    typeof candidate['meta'] === 'object' &&
    candidate['meta'] !== null
  );
}

/** Every descriptor a model declared, in a stable order. */
export const generatedDescriptors: readonly EntityDescriptor[] = Object.keys(declarations)
  .sort()
  .flatMap((path) =>
    Object.values(declarations[path] ?? {}).filter(isDescriptor),
  )
  .sort((a, b) =>
    a.component === b.component
      ? a.routeSegment.localeCompare(b.routeSegment)
      : a.component.localeCompare(b.component),
  );

// The entities a screen renders, which are the ones whose words belong in the
// catalogue.
for (const descriptor of generatedDescriptors) {
  screenedEntities.add(descriptor.entity);
}

/**
 * The entity words a model stated, keyed the way the catalogue is.
 *
 * Only entities with a screen set contribute: a component whose models have
 * metadata but no declaration has no screen to read the words, and a catalogue
 * of labels nothing renders is a catalogue nobody can check.
 */
export const generatedMessages: Catalogue = Object.keys(metadata)
  .sort()
  .reduce<Record<string, string | Catalogue>>((all, path) => {
    const module = metadata[path] ?? {};
    for (const [name, value] of Object.entries(module)) {
      // One export per entity, named for the entity and suffixed for what it
      // is; every other export is metadata keyed to a different contract.
      if (!name.endsWith('Messages')) continue;
      if (typeof value !== 'object' || value === null || Array.isArray(value)) {
        continue;
      }
      for (const [entity, words] of Object.entries(value as Catalogue)) {
        if (!screenedEntities.has(entity)) continue;
        all[entity] = words;
      }
    }
    return all;
  }, {});

/** The keys that come from a model, and so are English by construction. */
export const generatedMessageKeys: readonly string[] = Object.keys(
  flattenMessages(generatedMessages),
);

/** Flattens a nested catalogue to dot paths, as the translator reads them. */
function flattenMessages(catalogue: Catalogue, prefix = ''): Record<string, string> {
  const flat: Record<string, string> = {};
  for (const [key, value] of Object.entries(catalogue)) {
    const path = prefix.length === 0 ? key : `${prefix}.${key}`;
    if (typeof value === 'string') {
      flat[path] = value;
    } else {
      Object.assign(flat, flattenMessages(value, path));
    }
  }
  return flat;
}
