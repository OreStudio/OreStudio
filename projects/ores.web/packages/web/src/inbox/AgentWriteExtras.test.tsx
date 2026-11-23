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
import { describe, expect, it } from 'vitest';
import type { InboxRequestView } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { AgentWriteExtras } from './AgentWriteExtras.js';

const REQUEST = '66666666-6666-6666-6666-666666666666';

describe('The review of an agent write', () => {
    it('shows the agent, the call it made and its body', () => {
        const client = new QueryClient({
            defaultOptions: { queries: { retry: false, retryOnMount: false, staleTime: Infinity } },
        });
        client.setQueryData(['agent-held-write', REQUEST], {
            agent_name: 'trade-bot',
            write: {
                method: 'POST',
                route: '/api/v1/reference-data/currencies',
                target: '/api/v1/reference-data/currencies',
                body: '{"change":{"write":{"iso_code":"QZE"}}}',
                state: 'held',
                status: 0,
                outcome: '',
            },
        });
        const html = renderToStaticMarkup(
            <QueryClientProvider client={client}>
                <TranslationProvider>
                    <AgentWriteExtras request={{ id: REQUEST } as unknown as InboxRequestView} />
                </TranslationProvider>
            </QueryClientProvider>,
        );
        expect(html).toContain('trade-bot');
        expect(html).toContain('POST /api/v1/reference-data/currencies');
        expect(html).toContain('QZE');
        expect(html).toContain('Waiting for you');
    });
});
