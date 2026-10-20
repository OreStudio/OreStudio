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
import { ApiFailure } from '../api/transport.js';
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

    it('tries a read the server refused once, and says so once', async () => {
        const client = createQueryClient();
        const refusal = new ApiFailure(404, {
            code: 'not-found',
            message: 'There is no such record, or you may not see it.',
        });
        let attempts = 0;

        await expect(
            client.fetchQuery({
                queryKey: ['one record'],
                queryFn: async () => {
                    attempts += 1;
                    throw refusal;
                },
            }),
        ).rejects.toThrow('There is no such record');

        // A 4xx is an answer, not a fault that may pass on a second try.
        expect(attempts).toBe(1);
        expect(current()).toHaveLength(1);
    });

    it('still tries a server fault again, because it may pass', async () => {
        const retry = createQueryClient().getDefaultOptions().queries?.retry;
        const fault = new ApiFailure(502, {
            code: 'upstream-unavailable',
            message: 'The server refused the request.',
        });

        expect(typeof retry).toBe('function');
        expect(typeof retry === 'function' ? retry(0, fault) : false).toBe(true);
        expect(typeof retry === 'function' ? retry(2, fault) : false).toBe(false);
    });

    it('never reads again because the window gained focus or the network came back', () => {
        const options = createQueryClient().getDefaultOptions().queries;

        expect(options?.refetchOnWindowFocus).toBe(false);
        expect(options?.refetchOnReconnect).toBe(false);
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
