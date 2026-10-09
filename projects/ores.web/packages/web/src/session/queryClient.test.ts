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

import { beforeEach, describe, expect, it } from 'vitest';
import { current, dismissAll } from '../api/errors.js';
import { createQueryClient } from './SessionProvider.js';

/**
 * The wiring that makes every screen speak, asserted against the client the
 * application actually builds rather than a hand-made one: a query and a
 * mutation that fail both have to arrive in the store, because the banner is
 * the only place a person sees them.
 */
describe('the query client the application builds', () => {
    beforeEach(() => {
        dismissAll();
    });

    it('reports a failed query', async () => {
        const client = createQueryClient();

        await expect(
            client.fetchQuery({
                queryKey: ['the roster'],
                queryFn: () => Promise.reject(new Error('the roster did not load')),
                retry: false,
            }),
        ).rejects.toThrow('the roster did not load');

        expect(current().map((entry) => entry.message)).toEqual(['the roster did not load']);
    });

    it('reports a failed mutation', async () => {
        const client = createQueryClient();
        const mutation = client.getMutationCache().build(client, {
            mutationFn: () => Promise.reject(new Error('the save did not finish')),
            retry: false,
        });

        await expect(mutation.execute(undefined)).rejects.toThrow('the save did not finish');

        expect(current().map((entry) => entry.message)).toEqual(['the save did not finish']);
    });
});
