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
import { afterEach, describe, expect, it, vi } from 'vitest';
import { api } from './client.js';

afterEach(() => {
    vi.unstubAllGlobals();
});

function sentHeaders(): () => Headers {
    const fetchMock = vi.fn(() => Promise.resolve(new Response('{}', { status: 200 })));
    vi.stubGlobal('fetch', fetchMock);
    return () =>
        new Headers((fetchMock.mock.calls[0] as unknown as [string, RequestInit])[1].headers);
}

describe('the content type of a request body', () => {
    it('states JSON for a body that names no type, so the server parses it', async () => {
        const headers = sentHeaders();
        await api.acceptProviderTerms('boc', 3);
        expect(headers().get('content-type')).toBe('application/json');
    });

    it('sends no content type when there is no body', async () => {
        const headers = sentHeaders();
        await api.withdrawProviderTerms('boc');
        expect(headers().get('content-type')).toBeNull();
    });
});
