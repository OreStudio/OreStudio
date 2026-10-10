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
import type { QaDoc } from '@ores/contracts';
import { ApiFailure } from '../api/transport.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { DocPanel, docKey, docRoute } from './DocPanel.js';

const ID = 'AAAAAAAA-AAAA-4AAA-8AAA-AAAAAAAAAAAA';
const OTHER = 'BBBBBBBB-BBBB-4BBB-8BBB-BBBBBBBBBBBB';

const doc: QaDoc = {
    id: ID,
    title: 'Task: Retake the shots',
    type: 'task',
    path: 'sprint_27/story/task_retake.org',
    text: [
        ':PROPERTIES:',
        `:ID: ${ID}`,
        ':END:',
        '#+title: Task: Retake the shots',
        '',
        '* Goal',
        '',
        `Part of [[id:${OTHER}][the story]]. See *the notes*.`,
    ].join('\n'),
};

function panel(setup: (client: QueryClient) => void): string {
    const client = new QueryClient({
        defaultOptions: { queries: { retry: false, retryOnMount: false } },
    });
    setup(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter>
                    <DocPanel id={ID} />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

function failWith(client: QueryClient, error: Error): void {
    const query = client.getQueryCache().build(client, { queryKey: docKey(ID) });
    query.setState({ ...query.state, status: 'error', error, fetchStatus: 'idle' });
}

describe('the route of a doc', () => {
    it('names the id, and encodes one that is not a plain identifier', () => {
        expect(docRoute(ID)).toBe(`/development/docs/${ID}`);
        expect(docRoute('a/b?c#d')).toBe('/development/docs/a%2Fb%3Fc%23d');
    });
});

describe('a doc beside the steps', () => {
    it('shows the title, the path and the rendered text, without the doc header', () => {
        const html = panel((client) => client.setQueryData(docKey(ID), doc));
        expect(html).toContain('Task: Retake the shots');
        expect(html).toContain('sprint_27/story/task_retake.org');
        expect(html).toContain('<strong>the notes</strong>');
        expect(html).toContain('Goal');
        expect(html).not.toContain(':PROPERTIES:');
        expect(html).not.toContain('#+title');
    });

    it('points an id link at the page of the doc it names', () => {
        const html = panel((client) => client.setQueryData(docKey(ID), doc));
        expect(html).toContain(`href="/development/docs/${OTHER}"`);
        expect(html).toContain(`data-org-id="${OTHER}"`);
        expect(html).toContain('>the story</a>');
    });

    it('shows markup in the text as text', () => {
        const hostile = {
            ...doc,
            text: '* Goal\n\n<script>alert(1)</script> [[javascript:alert(1)][x]]',
        };
        const html = panel((client) => client.setQueryData(docKey(ID), hostile));
        expect(html).not.toContain('<script>');
        expect(html).not.toContain('href="javascript:');
    });

    it('says a link does not resolve, and names the id, when the server finds no such doc', () => {
        const html = panel((client) =>
            failWith(client, new ApiFailure(404, { code: 'not-found', message: 'No story.' })),
        );
        expect(html).toContain(`No story, task or scenario has the id ${ID}.`);
        expect(html).toContain('role="status"');
        expect(html).not.toContain('role="alert"');
    });

    it('shows the server message for any other failure, as an alert', () => {
        const html = panel((client) =>
            failWith(
                client,
                new ApiFailure(503, {
                    code: 'upstream-unavailable',
                    message: 'The scenario files cannot be read now. Try again.',
                }),
            ),
        );
        expect(html).toContain('role="alert"');
        expect(html).toContain('The scenario files cannot be read now.');
        expect(html).not.toContain('has the id');
    });

    it('says it is loading before the first answer', () => {
        expect(panel(() => undefined)).toContain('Loading...');
    });
});
