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

import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter, Routes } from 'react-router';
import type { AccountAccess } from '@ores/wire-protocol/browser';
import type { RecordResourceView } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';

/**
 * What the record screens' tests share: a resource as the registry describes
 * it, the access of a person who holds some permissions, and a page rendered at
 * an address from a seeded query cache.
 */

/** One resource as the BFF's registry lists it, writable, with the permissions its name gives. */
export function resourceView(
    key: string,
    keyFields: string[],
    options: {
        readonly versioned?: boolean;
        readonly search?: boolean;
        readonly sortable?: string[];
        /** The events collection and permission name, when it is not the key with underscores. */
        readonly name?: string;
    } = {},
): RecordResourceView {
    const name = options.name ?? key.replaceAll('-', '_');
    return {
        key,
        entityType: `ores.refdata.${name}`,
        keyFields,
        versioned: options.versioned ?? true,
        writable: true,
        search: options.search ?? false,
        sortable: options.sortable ?? [],
        events: { component: 'refdata', entity: name },
        writePermission: `refdata::${name}:write`,
        deletePermission: `refdata::${name}:delete`,
    };
}

/** The access of a person who holds exactly these permissions. */
export function accessOf(codes: string[]): AccountAccess {
    return {
        roles: [
            {
                roleId: '33333333-3333-3333-3333-333333333333' as AccountAccess['roles'][number]['roleId'],
                name: 'Operations',
                description: '',
                permissionCodes: codes,
                givenBy: 'priya',
                givenAt: '2026-10-04',
                reasonCode: 'access.new_joiner',
                commentary: '',
            },
        ],
    };
}

/** A page at an address, drawn by the routes given, from a cache the test seeds. */
export function renderRecords(
    path: string,
    registry: RecordResourceView[],
    routes: ReactNode,
    seed: (client: QueryClient) => void,
): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['registry', 'refdata'], registry);
    client.setQueryData(['labels'], { labels: [], domains: {} });
    seed(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>{routes}</Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}
