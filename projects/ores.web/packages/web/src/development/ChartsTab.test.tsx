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
import type { Charts } from '@ores/contracts';
import { TranslationProvider } from '../i18n/Provider.js';
import { ChartGrid } from './ChartsTab.js';
import { DevelopmentArea } from './DevelopmentArea.js';
import { chartsKey } from './queries.js';

const CHARTS: Charts = {
    velocity: [
        { sprint: 'sprint_26', done: 3 },
        { sprint: 'sprint_27', done: 5 },
    ],
    burnup: [
        { date: '2026-10-02', done: 2 },
        { date: '2026-10-04', done: 3 },
    ],
    scope: 4,
    perDay: [
        { date: '2026-10-01', day: 1, done: 0, future: false },
        { date: '2026-10-02', day: 2, done: 2, future: false },
        { date: '2026-10-05', day: 3, done: 0, future: true },
    ],
    byState: [
        { state: 'STARTED', count: 1 },
        { state: 'DONE', count: 3 },
    ],
};

function screen(data?: Charts, failure?: string): string {
    const client = new QueryClient({
        defaultOptions: { queries: { retry: false, retryOnMount: false } },
    });
    if (failure !== undefined) {
        const query = client.getQueryCache().build(client, { queryKey: chartsKey(undefined) });
        query.setState({
            ...query.state,
            status: 'error',
            error: new Error(failure),
            fetchStatus: 'idle',
        });
    } else if (data !== undefined) {
        client.setQueryData(chartsKey(undefined), data);
    }
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={['/?tab=charts']}>
                    <DevelopmentArea />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the Charts tab', () => {
    const markup = screen(CHARTS);

    it('draws the four charts with a name each', () => {
        for (const name of ['Velocity', 'Burn-up', 'Tasks done each day', 'Tasks by state']) {
            expect(markup).toContain(`aria-label="${name}"`);
        }
    });

    it('labels the velocity bars by sprint and the day bars by day', () => {
        expect(markup).toContain('>s26<');
        expect(markup).toContain('>s27<');
        expect(markup).toContain('>D2<');
    });

    it('dims the days after today', () => {
        expect(markup).toContain('opacity="0.35"');
    });

    it('names the states of the tasks', () => {
        expect(markup).toContain('>Started<');
        expect(markup).toContain('>Done<');
    });

    it('says what is missing instead of drawing an empty chart', () => {
        const html = renderToStaticMarkup(
            <TranslationProvider>
                <ChartGrid charts={{ ...CHARTS, burnup: [], perDay: [] }} />
            </TranslationProvider>,
        );
        expect(html).toContain('No task has a finish date yet.');
        expect(html).toContain('This sprint has no start date.');
    });

    it('says what failed, and shows loading first', () => {
        expect(screen(undefined, 'No sprint has this name.')).toContain('No sprint has this name.');
        expect(screen()).toContain('Loading');
    });
});
