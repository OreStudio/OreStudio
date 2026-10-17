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

import type { RecordRow } from '../../api/client.js';
import { bindings } from '../../api/bindings.js';
import type { RecordStore } from '../../refdata/store.js';

/** The resource the source picker names. The store answers for this one only. */
export const BINDABLE = 'bindable-sources';

/** The store is a read: the picker chooses a source, and the binding is written as its own record. */
function refuse(): never {
    throw new Error(`${BINDABLE} is read from the party's sources and cannot be written.`);
}

/**
 * The synthetic sources the party may still bind, as the record picker reads them:
 * one row per source, labelled by the feed and carrying the series it would write.
 *
 * The list is the BFF's, which joins the party's sources to its bindings; nothing
 * here decides what may be bound.
 */
export const bindableSourcesStore: RecordStore = {
    name: 'bindings',
    registry: async () => [],
    page: refuse,
    records: async (resource) => {
        if (resource !== BINDABLE) {
            return [];
        }
        const sources = await bindings.sources();
        return sources.map(
            (source) =>
                ({
                    source_name: source.source_name,
                    label: source.label === '' ? source.source_name : source.label,
                    series: source.oresmd_uri,
                    held_by: source.held_by,
                    version: 0,
                }) satisfies RecordRow,
        );
    },
    record: refuse,
    save: refuse,
    remove: refuse,
};
