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
import type { Backlog } from '@ores/contracts';
import { TranslationProvider } from '../i18n/Provider.js';
import { Buckets } from './BacklogTab.js';
import { DevelopmentArea } from './DevelopmentArea.js';
import { AGILE_BACKLOG_KEY } from './queries.js';

const item = (over: Record<string, unknown>) => ({
    id: 'C1',
    title: 'A capture',
    description: '',
    created: '2026-10-05',
    kind: 'capture' as const,
    path: 'agile/product_backlog/inbox/a.org',
    tasks: 0,
    ...over,
});

const BACKLOG: Backlog = {
    buckets: [
        {
            id: 'inbox',
            items: [
                item({}),
                item({ id: '', title: 'No id', path: 'agile/product_backlog/inbox/b.org' }),
            ],
        },
        {
            id: 'next',
            items: [
                item({
                    id: 'S1',
                    title: 'A story',
                    kind: 'story',
                    tasks: 1,
                    path: 'agile/product_backlog/next/s/story.org',
                }),
            ],
        },
        { id: 'deferred', items: [] },
        { id: 'discarded', items: [] },
    ],
};

function screen(data?: Backlog): string {
    const client = new QueryClient({
        defaultOptions: { queries: { retry: false, retryOnMount: false } },
    });
    if (data !== undefined) client.setQueryData(AGILE_BACKLOG_KEY, data);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={['/?tab=backlog']}>
                    <DevelopmentArea />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the Backlog tab', () => {
    const markup = screen(BACKLOG);

    it('draws the four buckets in order, with their counts', () => {
        const order = ['Inbox', 'Next', 'Deferred', 'Discarded'].map((name) =>
            markup.indexOf(`>${name}</h2>`),
        );
        expect(order.every((index) => index > 0)).toBe(true);
        expect([...order].sort((a, b) => a - b)).toEqual(order);
    });

    it('links a row with an id to its doc, and shows a row with none as text', () => {
        expect(markup).toContain('href="/development/docs/C1"');
        expect(markup).toContain('>A capture</a>');
        expect(markup).toContain('No id');
        expect(markup).not.toContain('>No id</a>');
    });

    it('says what a row is, with the task count of a story', () => {
        expect(markup).toContain('Capture');
        expect(markup).toContain('Story, 1 task');
    });

    it('says an empty bucket is empty', () => {
        expect(markup).toContain('Nothing in this bucket.');
    });

    it('shows loading before the buckets arrive', () => {
        expect(screen()).toContain('Loading');
    });

    it('draws the buckets without a router query', () => {
        const html = renderToStaticMarkup(
            <TranslationProvider>
                <Buckets backlog={BACKLOG} onOpen={() => undefined} />
            </TranslationProvider>,
        );
        expect(html).toContain('A story');
    });
});
