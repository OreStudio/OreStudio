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

import { QueryCache, QueryClient } from '@tanstack/react-query';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { current, dismissAll, reportQueryError } from '../api/errors.js';
import { ApiFailure } from '../api/transport.js';
import { QA_SCENARIOS_KEY, docKey, qaQueries, scenarioKey } from './queries.js';

/** A client that reports a failed query to the banner, as the app's client does. */
function appClient(): QueryClient {
    return new QueryClient({
        queryCache: new QueryCache({ onError: reportQueryError }),
        defaultOptions: { queries: { retry: false } },
    });
}

function answerWith(status: number, body: unknown): void {
    vi.stubGlobal(
        'fetch',
        vi.fn(async () => new Response(JSON.stringify(body), { status })),
    );
}

describe('the reads of the QA validation runner', () => {
    beforeEach(() => {
        dismissAll();
    });
    afterEach(() => {
        vi.unstubAllGlobals();
    });

    it('name their keys', () => {
        expect(qaQueries.scenarios().queryKey).toEqual(QA_SCENARIOS_KEY);
        expect(qaQueries.scenario('a').queryKey).toEqual(scenarioKey('a'));
        expect(qaQueries.doc('a').queryKey).toEqual(docKey('a'));
    });

    it('leave a failure to their screen, so the banner does not repeat it', async () => {
        answerWith(404, { code: 'not-found', message: 'No story, task or scenario has this id.' });
        const client = appClient();
        await client.fetchQuery(qaQueries.doc('x')).catch(() => undefined);
        await client.fetchQuery(qaQueries.scenario('x')).catch(() => undefined);
        await client.fetchQuery(qaQueries.scenarios()).catch(() => undefined);
        expect(current()).toEqual([]);
    });

    it('would be reported if they did not say so, which shows the check is sound', async () => {
        answerWith(404, { code: 'not-found', message: 'No story, task or scenario has this id.' });
        const client = appClient();
        const { meta: _meta, ...loud } = qaQueries.doc('x');
        await client.fetchQuery(loud).catch(() => undefined);
        expect(current().map((entry) => entry.message)).toEqual([
            'No story, task or scenario has this id.',
        ]);
    });

    it('keep the scenario as a snapshot: not kept for a later visit, not refetched', () => {
        const options = qaQueries.scenario('a');
        expect(options.gcTime).toBe(0);
        expect(options.refetchOnWindowFocus).toBe(false);
        expect(options.refetchOnReconnect).toBe(false);
    });

    it('fail with the server sentence, which the screen shows', async () => {
        answerWith(503, {
            code: 'upstream-unavailable',
            message: 'The scenario files cannot be read now.',
        });
        const failure = await appClient()
            .fetchQuery(qaQueries.scenarios())
            .catch((e: unknown) => e);
        expect(failure).toBeInstanceOf(ApiFailure);
        expect((failure as ApiFailure).message).toBe('The scenario files cannot be read now.');
    });
});
