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
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { describe, expect, it } from 'vitest';
import { TranslationProvider } from '../i18n/Provider.js';
import { AgentActivityPanel } from './AgentActivityPanel.js';

const READER = '33333333-3333-3333-3333-333333333333';
const TRADER = '55555555-5555-5555-5555-555555555555';

function render(path: string): string {
    const client = new QueryClient({
        defaultOptions: { queries: { retry: false, retryOnMount: false, staleTime: Infinity } },
    });
    const agent = new URLSearchParams(path.split('?')[1] ?? '').get('agent') ?? '';
    const all = [
        {
            id: 'c1',
            called_at: '2026-11-23 10:00:00Z',
            grant_id: READER,
            method: 'GET',
            route: '/api/v1/reference-data/currencies',
            status: 200,
            duration_ms: 12,
        },
        {
            id: 'c2',
            called_at: '2026-11-23 10:01:00Z',
            grant_id: TRADER,
            method: 'DELETE',
            route: '/api/v1/reference-data/currencies/{iso_code}',
            status: 403,
            duration_ms: 7,
        },
    ];
    client.setQueryData(['agent-activity', agent], {
        pages: [all.filter((c) => agent === '' || c.grant_id === agent)],
        pageParams: [0],
    });
    client.setQueryData(['agent-grants-own'], {
        grants: [
            { id: READER, agent_name: 'research-bot' },
            { id: TRADER, agent_name: 'trade-bot' },
        ],
        keys: [],
    });
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <AgentActivityPanel />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('What your agents did', () => {
    it('lists every call with the agent that made it and how it was answered', () => {
        const html = render('/security');
        expect(html).toContain('research-bot');
        expect(html).toContain('trade-bot');
        expect(html).toContain('GET /api/v1/reference-data/currencies');
        expect(html).toContain('403');
    });

    it('shows one agent when the address names it', () => {
        const html = render(`/security?agent=${READER}`);
        expect(html).toContain('GET /api/v1/reference-data/currencies');
        expect(html).not.toContain('{iso_code}');
    });
});
