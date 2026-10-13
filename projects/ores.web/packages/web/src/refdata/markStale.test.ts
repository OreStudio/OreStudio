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

import { QueryClient, QueryObserver } from '@tanstack/react-query';
import { describe, expect, it } from 'vitest';
import { markStale } from './records.js';

/**
 * A write marks what it made out of date and reads none of it: the screen under
 * the write learns of the change from the change stream and waits for Refresh.
 */
describe('marking what a write made out of date', () => {
    const KEY = ['records', 'parties', 'key', 'p1'] as const;

    const show = async (
        client: QueryClient,
    ): Promise<{ reads: () => number; stop: () => void }> => {
        let count = 0;
        const observer = new QueryObserver(client, {
            queryKey: KEY,
            queryFn: () => {
                count += 1;
                return Promise.resolve({ count });
            },
        });
        const stop = observer.subscribe(() => undefined);
        await observer.refetch();
        return { reads: () => count, stop };
    };

    it('reads nothing again, even a read that is on screen', async () => {
        const client = new QueryClient();
        const shown = await show(client);

        markStale(client, [['records']]);
        await Promise.resolve();

        expect(shown.reads()).toBe(1);
        shown.stop();
    });

    it('leaves the read stale, so the screen that shows it next reads it fresh', async () => {
        const client = new QueryClient();
        const shown = await show(client);

        markStale(client, [['records']]);

        expect(client.getQueryState(KEY)?.isInvalidated).toBe(true);
        shown.stop();
    });
});
