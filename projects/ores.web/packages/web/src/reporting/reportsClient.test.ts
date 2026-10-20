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
 */

import { afterEach, describe, expect, it, vi } from 'vitest';
import { api } from '../api/client.js';

/**
 * What the list asks the server: the search, the order and the party go in the
 * address, so the server narrows and orders and the browser does neither.
 */

const PARTY = '77777777-7777-4777-8777-777777777777';

/** Answers one request with a body and remembers the address it was sent to. */
function answer(body: unknown): { readonly urls: string[] } {
    const urls: string[] = [];
    vi.stubGlobal(
        'fetch',
        vi.fn((url: string) => {
            urls.push(url);
            return Promise.resolve(new Response(JSON.stringify(body), { status: 200 }));
        }),
    );
    return { urls };
}

afterEach(() => {
    vi.unstubAllGlobals();
});

describe('the reads of the reports list', () => {
    it('sends the search, the order and the party of a page', async () => {
        const sent = answer({ rows: [], total: 0, unread: [] });

        await api.reports({
            offset: 15,
            limit: 15,
            sort: 'schedule_expression',
            descending: true,
            partyId: PARTY,
            search: 'audit trail',
        });

        const query = new URL(sent.urls[0] ?? '', 'http://localhost').searchParams;
        expect(query.get('sort')).toBe('schedule_expression');
        expect(query.get('descending')).toBe('true');
        expect(query.get('party_id')).toBe(PARTY);
        expect(query.get('search')).toBe('audit trail');
    });

    it('leaves out a search nobody typed and a party nobody chose', async () => {
        const sent = answer({ rows: [], total: 0, unread: [] });

        await api.reports({
            offset: 0,
            limit: 15,
            sort: '',
            descending: false,
            partyId: null,
            search: '',
        });

        const query = new URL(sent.urls[0] ?? '', 'http://localhost').searchParams;
        expect(query.has('search')).toBe(false);
        expect(query.has('party_id')).toBe(false);
    });

    it('reads the tally for the same party and search as the list', async () => {
        const tally = { counts: [{ health: 'failed', count: 2 }], total: 2 };
        const sent = answer(tally);

        const read = await api.reportHealth({ partyId: PARTY, search: 'audit' });

        const url = new URL(sent.urls[0] ?? '', 'http://localhost');
        expect(url.pathname).toBe('/api/reporting/report-health');
        expect(url.searchParams.get('party_id')).toBe(PARTY);
        expect(url.searchParams.get('search')).toBe('audit');
        expect(read).toEqual(tally);
    });
});
