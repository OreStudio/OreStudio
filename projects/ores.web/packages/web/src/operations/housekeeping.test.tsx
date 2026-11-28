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

import { describe, expect, it } from 'vitest';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter, Route, Routes } from 'react-router';
import type { StoreBound, StoreSample } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { HousekeepingPage, HOUSEKEEPING_STORES_QUERY_KEY } from './HousekeepingPage.js';
import { HousekeepingStorePage } from './HousekeepingStorePage.js';
import { arrange, formatSize, summarise, trendOf, verdictOf } from './housekeeping.js';

/**
 * The housekeeping screens, read as a person reads them: which store is bounded,
 * what the list says at its head, and what a store's own page states.
 *
 * The fixtures state the bounds view's own answers, including the ones that read
 * as bounded on paper, because the screen's job is to say what the server says and
 * never to infer a bound from a policy's presence.
 */

function store(overrides: Partial<StoreBound> = {}): StoreBound {
    return {
        code: 'marketdata.delivery_counts',
        display_name: 'Market data delivery counts',
        description: 'How many items each feed delivered.',
        owner_component: 'marketdata',
        target_kind: 'table',
        target: 'public.ores_marketdata_delivery_counts_tbl',
        time_column: 'period_start',
        measure_kind: 'relation_rows',
        bound_kind: 'policy',
        bound: {
            action: 'delete_rows',
            retention_days: 30,
            cadence_hours: 24,
            policy_code: 'delivery_counts_30d',
            policy_enabled: true,
            policy_version: 7,
            policy_description: 'A delivery counter is read for a month',
            mechanism: 'sweep',
            bounded_now: true,
        },
        last_run: {
            outcome: 'applied',
            ran_at: '2026-11-27 03:00:00.000000Z',
            removed: 2,
            detail: '',
        },
        size: {
            size_bytes: 32768,
            measure_count: 27,
            sampled_at: '2026-11-27 03:15:00.000000Z',
        },
        ...overrides,
    };
}

/** A store no policy bounds, and one whose bound the sweep cannot perform. */
const calendarDates = store({
    code: 'refdata.calendar_dates',
    display_name: 'Calendar dates',
    description: 'The business days of every calendar.',
    owner_component: 'refdata',
    target_kind: 'chunked_table',
    target: 'public.ores_refdata_calendar_dates_tbl',
    time_column: 'valid_from',
    measure_kind: 'relation_rows',
    bound_kind: 'none',
    bound: {
        action: null,
        retention_days: null,
        cadence_hours: 24,
        policy_code: null,
        policy_enabled: false,
        policy_version: 0,
        policy_description: null,
        mechanism: 'none',
        bounded_now: false,
    },
    last_run: { outcome: null, ran_at: null, removed: 0, detail: null },
    size: {
        size_bytes: 293601280,
        measure_count: 486540,
        sampled_at: '2026-11-27 03:15:00.000000Z',
    },
});

const jobInputs = store({
    code: 'compute.job_inputs',
    display_name: 'Compute job inputs',
    description: 'The input archives a run hands to the grid.',
    owner_component: 'compute',
    target_kind: 'object_prefix',
    target: 'compute/input/',
    time_column: null,
    measure_kind: 'objects',
    bound: {
        action: 'expire_prefix',
        retention_days: 7,
        cadence_hours: 24,
        policy_code: 'job_inputs_7d',
        policy_enabled: true,
        policy_version: 3,
        policy_description: "An input archive is the run's own copy",
        mechanism: 'sweep',
        bounded_now: true,
    },
    last_run: {
        outcome: 'applied',
        ran_at: '2026-11-27 03:00:00.000000Z',
        removed: 4,
        detail: 'the cutoff held 4 object(s) of 2048 byte(s); removed 4 of 2048 byte(s)',
    },
    size: { size_bytes: 0, measure_count: 0, sampled_at: null },
});

/** A store whose bound waits on the node, which is why it is not bounded now. */
const nodeScratch = store({
    code: 'compute.node_scratch',
    display_name: 'Compute node scratch',
    description: 'The working folder each grid node keeps while it runs a job.',
    owner_component: 'compute',
    target_kind: 'directory',
    target: 'publish/run/wrappers',
    time_column: null,
    measure_kind: 'files',
    bound_kind: 'policy',
    bound: {
        action: 'release_directory',
        retention_days: 1,
        cadence_hours: 24,
        policy_code: 'node_scratch_1d',
        policy_enabled: true,
        policy_version: 2,
        policy_description: 'A job folder is scratch once its output is uploaded',
        mechanism: 'node_release',
        bounded_now: false,
    },
    last_run: {
        outcome: 'skipped',
        ran_at: '2026-11-27 03:00:00.000000Z',
        removed: 0,
        detail: "a node releases a job's folder when it uploads the output",
    },
    size: {
        size_bytes: 4096,
        measure_count: 12,
        sampled_at: '2026-11-27 03:15:00.000000Z',
    },
});

const STORES: readonly StoreBound[] = [store(), calendarDates, jobInputs, nodeScratch];

function withProviders(element: React.ReactNode, path: string): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(HOUSEKEEPING_STORES_QUERY_KEY, {
        read_at: '2026-11-27 03:20:00.000000Z',
        rows: STORES,
    });
    client.setQueryData(['operations', 'housekeeping', 'samples', 'marketdata.delivery_counts'], {
        store_code: 'marketdata.delivery_counts',
        samples: [
            { sampled_at: '2026-11-27 01:15:00.000000Z', size_bytes: 30000, measure_count: 20 },
            { sampled_at: '2026-11-27 02:15:00.000000Z', size_bytes: 31000, measure_count: 24 },
            { sampled_at: '2026-11-27 03:15:00.000000Z', size_bytes: 32768, measure_count: 27 },
        ],
    });
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>
                        <Route path="/operations/housekeeping" element={element} />
                        <Route path="/operations/housekeeping/:code" element={element} />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the verdict of a store', () => {
    it('says a store is bounded only when the read says a mechanism runs', () => {
        expect(verdictOf(store())).toEqual({ verdict: 'bounded', reason: null });
    });

    it('says a store whose action never runs is not bounded, whatever its policy says', () => {
        expect(verdictOf(nodeScratch)).toEqual({ verdict: 'unbounded', reason: 'notApplied' });
    });

    it('says an object prefix the sweep expires is bounded', () => {
        expect(verdictOf(jobInputs)).toEqual({ verdict: 'bounded', reason: null });
    });

    it('names the failure when the last sweep failed', () => {
        const failed = store({
            bound: { ...store().bound, bounded_now: false },
            last_run: { ...store().last_run, outcome: 'failed', detail: 'permission denied' },
        });
        expect(verdictOf(failed)).toEqual({ verdict: 'unbounded', reason: 'failed' });
    });

    it('names the off policy before it names a failure', () => {
        const off = store({
            bound: {
                ...store().bound,
                policy_enabled: false,
                mechanism: 'policy_off',
                bounded_now: false,
            },
            last_run: { ...store().last_run, outcome: 'failed' },
        });
        expect(verdictOf(off)).toEqual({ verdict: 'unbounded', reason: 'off' });
    });

    it('says the calendar dates cannot be bounded by a period at all', () => {
        expect(verdictOf(calendarDates)).toEqual({
            verdict: 'unbounded',
            reason: 'notBoundable',
        });
    });

    it('reads a store bounded by its keying as bounded, and calls it keying', () => {
        const keyed = store({
            code: 'compute.package_cache',
            bound_kind: 'keying',
            bound: {
                ...store().bound,
                action: null,
                mechanism: 'content_keying',
                bounded_now: true,
            },
        });
        expect(verdictOf(keyed)).toEqual({ verdict: 'keyed', reason: null });
    });
});

describe('what the list states at its head', () => {
    it('counts the stores nothing bounds, and adds up the ground they occupy', () => {
        expect(summarise(STORES)).toEqual({
            stores: 4,
            bounded: 2,
            unbounded: 2,
            unmeasured: 1,
            bytes: 293601280 + 32768 + 4096,
        });
    });

    it('puts the stores nothing bounds first, and then the largest', () => {
        const arranged = arrange(STORES, 'all', 'attention');
        expect(arranged.map((entry) => entry.code)).toEqual([
            'refdata.calendar_dates',
            'compute.node_scratch',
            'marketdata.delivery_counts',
            'compute.job_inputs',
        ]);
    });

    it('shows only the stores nothing bounds when the person filters to them', () => {
        const arranged = arrange(STORES, 'unbounded', 'name');
        expect(arranged.map((entry) => entry.code)).toEqual([
            'refdata.calendar_dates',
            'compute.node_scratch',
        ]);
    });

    it('orders by size when the question is only what is big', () => {
        const arranged = arrange(STORES, 'all', 'size');
        expect(arranged.map((entry) => entry.display_name)).toEqual([
            'Calendar dates',
            'Market data delivery counts',
            'Compute node scratch',
            'Compute job inputs',
        ]);
    });
});

describe('the sizes a store states', () => {
    it('states a size in the unit a person reads it in', () => {
        expect(formatSize(0)).toBe('0 B');
        expect(formatSize(512)).toBe('512 B');
        expect(formatSize(1024)).toBe('1.0 KiB');
        expect(formatSize(293601280)).toBe('280.0 MiB');
        expect(formatSize(2040109465)).toBe('1.9 GiB');
    });

    it('reads a trend from the first and the last measurement', () => {
        const trend = trendOf([
            { sampled_at: '2026-11-20 00:00:00.000000Z', size_bytes: 1000, measure_count: 1 },
            { sampled_at: '2026-11-27 00:00:00.000000Z', size_bytes: 8000, measure_count: 2 },
        ] as readonly StoreSample[]);
        expect(trend).not.toBeNull();
        expect(trend?.delta).toBe(7000);
        expect(trend?.days).toBeCloseTo(7);
        expect(trend?.perWeek).toBeCloseTo(7000);
    });

    it('states no trend for a store with one measurement', () => {
        expect(
            trendOf([
                { sampled_at: '2026-11-27 00:00:00.000000Z', size_bytes: 10, measure_count: 1 },
            ] as readonly StoreSample[]),
        ).toBeNull();
    });
});

describe('the housekeeping list', () => {
    const html = withProviders(<HousekeepingPage />, '/operations/housekeeping');

    it('names every store by the name a person knows it by, and its target beneath', () => {
        expect(html).toContain('Calendar dates');
        expect(html).toContain('public.ores_refdata_calendar_dates_tbl');
        expect(html).toContain('Compute job inputs');
    });

    it('reads the unbounded stores in red, and says why', () => {
        expect(html).toContain('Unbounded');
        expect(html).toContain('no period can bound it');
        expect(html).toContain('the sweep cannot perform its action');
        expect(html).toContain('text-down');
    });

    it('counts the stores nothing bounds before it states a row', () => {
        expect(html).toContain('The work left');
        expect(html).toContain('280.0 MiB');
    });

    it('says a store nothing measures is not measured rather than empty', () => {
        expect(html).toContain('not measured');
    });

    it('states what the last sweep removed, including nothing', () => {
        expect(html).toContain('2 removed');
        expect(html).toContain('skipped');
    });
});

describe("a store's own page", () => {
    const html = withProviders(
        <HousekeepingStorePage />,
        '/operations/housekeeping/marketdata.delivery_counts',
    );

    it('states the record the catalogue publishes', () => {
        expect(html).toContain('Market data delivery counts');
        expect(html).toContain('period_start');
        expect(html).toContain('Written by');
        expect(html).toContain('marketdata');
    });

    it('states the size now, and plots the measurements it read', () => {
        expect(html).toContain('32.0 KiB');
        expect(html).toContain('27 rows');
        expect(html).toContain('<svg');
        expect(html).toContain('227.1 KiB a week, rising');
    });

    it('states the bound, its period and its cadence', () => {
        expect(html).toContain('Delete the rows past the period');
        expect(html).toContain('30 days');
        expect(html).toContain('every 24 h');
        expect(html).toContain('delivery_counts_30d');
    });

    it('says what the page does not show, and which journey records why', () => {
        expect(html).toContain('Every sweep that touched this store');
        expect(html).toContain('F6622109-4F52-4DE9-9C06-6B0EAA7C8D2E');
    });

    it('offers no form to a role that cannot write a bound', () => {
        expect(html).not.toContain('Change the bound');
    });

    it('states that a store of an unknown name is not kept here', () => {
        const missing = withProviders(
            <HousekeepingStorePage />,
            '/operations/housekeeping/nothing.here',
        );
        expect(missing).toContain('This installation keeps no store called nothing.here.');
    });
});
