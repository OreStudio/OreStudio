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

import { createContext, useContext, type ReactNode } from 'react';
import { api, type RecordRow } from '../api/client.js';

/** What a record screen needs to know of one resource before it draws an action. */
export interface StoreResource {
    readonly key: string;
    readonly writable: boolean;
    readonly search: boolean;
    readonly sortable: readonly string[];
    readonly writePermission: string;
    readonly deletePermission: string;
    /** Where the services announce changes to the resource, when they do. */
    readonly events?: { readonly component: string; readonly entity: string };
}

/** What a write carries to say why it was made. */
export interface WriteIntent {
    readonly reasonCode: string;
    readonly commentary: string;
}

/**
 * Where a family of record kinds is read and written: the refdata routes or
 * the synthetic routes. The shared record parts read the store from the
 * context, so a screen chooses its family once and its parts follow.
 */
export interface RecordStore {
    /** The permission namespace, such as `refdata`; its wildcard grants every resource in it. */
    readonly name: string;
    readonly registry: () => Promise<readonly StoreResource[]>;
    readonly page: (
        resource: string,
        page: {
            readonly offset: number;
            readonly limit: number;
            readonly search: string;
            readonly sort: string;
            readonly descending: boolean;
        },
    ) => Promise<{ readonly rows: readonly RecordRow[]; readonly total: number }>;
    readonly records: (resource: string, parent?: string) => Promise<readonly RecordRow[]>;
    readonly record: (resource: string, key: string) => Promise<RecordRow>;
    readonly save: (
        resource: string,
        input: WriteIntent & {
            readonly write: Readonly<Record<string, unknown>>;
            readonly version: number | null;
        },
    ) => Promise<void>;
    readonly remove: (
        resource: string,
        input: WriteIntent & {
            readonly key: Readonly<Record<string, string>>;
            readonly version?: number | null;
        },
    ) => Promise<void>;
}

export const refdataStore: RecordStore = {
    name: 'refdata',
    registry: () => api.refdataRegistry(),
    page: (resource, page) => api.recordPage(resource, page),
    records: (resource, parent) => api.records(resource, parent),
    record: (resource, key) => api.record(resource, key),
    save: (resource, input) => api.saveRecord(resource, input),
    remove: (resource, input) => api.removeRecord(resource, input),
};

/** A synthetic row is named by one key field, so a removal sends that field's value. */
function syntheticKey(key: Readonly<Record<string, string>>): string {
    const [value] = Object.values(key);
    if (value === undefined || Object.keys(key).length !== 1) {
        throw new Error('A synthetic row is named by one key field.');
    }
    return value;
}

export const syntheticStore: RecordStore = {
    name: 'synthetic',
    registry: () => api.syntheticRegistry(),
    page: (resource, page) => api.syntheticPage(resource, page),
    records: (resource, parent) => api.syntheticRecords(resource, parent),
    record: (resource, key) => api.syntheticRecord(resource, key),
    save: (resource, { write, version, reasonCode, commentary }) =>
        api.saveSynthetic(resource, [{ write, version }], { reasonCode, commentary }),
    remove: (resource, { key, version, reasonCode, commentary }) =>
        api.removeSynthetic(resource, [{ key: syntheticKey(key), version: version ?? null }], {
            reasonCode,
            commentary,
        }),
};

const StoreContext = createContext<RecordStore>(refdataStore);

/** The store the record parts inside it read and write. */
export function RecordStoreProvider({
    store,
    children,
}: {
    readonly store: RecordStore;
    readonly children: ReactNode;
}): ReactNode {
    return <StoreContext value={store}>{children}</StoreContext>;
}

export function useRecordStore(): RecordStore {
    return useContext(StoreContext);
}
