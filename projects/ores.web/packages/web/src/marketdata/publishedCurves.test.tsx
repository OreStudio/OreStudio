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
import type { ReactNode } from 'react';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter, Route, Routes } from 'react-router';
import { describe, expect, it } from 'vitest';
import type { PublishedCurvePoint, PublishedCurveSet } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { FIRST_PAGE, pageKey } from '../refdata/RecordList.js';
import { PublishedCurvePage } from './PublishedCurvePage.js';
import { PublishedCurvesPage } from './PublishedCurvesPage.js';
import {
    alignOnPillars,
    baselineChoicesOf,
    candidatesOf,
    comparePoints,
    lastChangedAt,
    newestPublished,
    numberOf,
    overlayCaption,
    publishedSetsOf,
    quoted,
    setInUse,
    somethingChanged,
    stateOf,
    tallyOf,
    yearsOf,
} from './publishedCurves.js';

/**
 * The published curve screens, as the prototype draws them.
 *
 * What is checked is what the screens promise: the list draws a set with the act
 * that published it and the state it is in, a candidate is never drawn as the set
 * a desk prices from, the comparison marks each pillar as changed, added,
 * dropped or unchanged and says in words when nothing changed, and the history is
 * the record's own versions with the decision that published it.
 */

const SERIES = '0a42775e-3da7-4c61-a732-6b62cb9f948f';
const REQUEST = '5b42775e-3da7-4c61-a732-6b62cb9f948f';
const CURVE = 'USD-SOFR-ACME';
const PARTY = 'ACCOUS';

function point(pillar: string, value: string, kind = 'discount'): PublishedCurvePoint {
    return {
        pillar,
        value,
        value_kind: kind,
        oresmd_uri: `oresmd://ir/USD?type=quote&term=${pillar}`,
        observation_datetime: '2026-10-27 12:00:00Z',
    };
}

function set(overrides: Partial<PublishedCurveSet> = {}): PublishedCurveSet {
    return {
        publish_id: '3f1e2d4c-0000-4000-8000-000000000001',
        request_id: REQUEST,
        curve: CURVE,
        series_id: SERIES,
        series_uri:
            'oresmd://ir/USD?type=series&instrument=discount&quote=rate&curve_id=USD-SOFR-ACME',
        bootstrap_config_id: 'd0e1f2a3-b4c5-4d6e-7f80-91a2b3c4d5e6',
        party: PARTY,
        party_id: '3f1e2d4c-0000-4000-8000-000000000010',
        as_of: '2026-10-27 12:00:00Z',
        published_by: 'henry.jones',
        published_at: '2026-10-27 16:07:08Z',
        state_code: 'published',
        point_count: 2,
        withdrawn_by: '',
        withdrawn_at: null,
        withdrawn_reason_code: '',
        withdrawal_request_id: '',
        superseded_at: null,
        superseded_by_request_id: '',
        superseded_as_of: null,
        points: [point('12M', '0.95803793830235673'), point('2Y', '0.92105010131696952')],
        ...overrides,
    };
}

function candidate(overrides: Partial<PublishedCurveSet> = {}): PublishedCurveSet {
    return set({
        publish_id: '3f1e2d4c-0000-4000-8000-000000000002',
        published_by: '',
        published_at: null,
        state_code: 'candidate',
        as_of: '2026-10-28 12:00:00Z',
        points: [point('12M', '0.9581'), point('2Y', '0.9211')],
        ...overrides,
    });
}

describe('the reading of a set', () => {
    it('names the state the server stated, and a set with no instant is a candidate', () => {
        expect(stateOf(set())).toBe('published');
        expect(stateOf(set({ state_code: 'withdrawn' }))).toBe('withdrawn');
        expect(stateOf(set({ state_code: 'superseded' }))).toBe('superseded');
        expect(stateOf(candidate())).toBe('candidate');
        expect(stateOf(set({ state_code: 'nonsense' }))).toBe('candidate');
    });

    it('draws the newest published set as the record, and the newest un-ended one as the set in use', () => {
        const older = set({
            publish_id: 'a',
            as_of: '2026-10-26 12:00:00Z',
            published_at: '2026-10-26 16:00:00Z',
            state_code: 'superseded',
        });
        const newer = set({ publish_id: 'b', published_at: '2026-10-27 16:00:00Z' });
        const sets = [older, newer];

        expect(newestPublished(sets)?.publish_id).toBe('b');
        expect(setInUse([older])).toBeUndefined();
        expect(publishedSetsOf([newer, older]).map((one) => one.publish_id)).toEqual(['a', 'b']);
        expect(candidatesOf([candidate(), newer]).map((one) => one.publish_id)).toEqual([
            '3f1e2d4c-0000-4000-8000-000000000002',
        ]);
    });

    it('offers the newest as-of and two earlier ones as baselines, each instant once', () => {
        const sets = [
            set({ publish_id: 'a', as_of: '2026-10-25 12:00:00Z' }),
            set({ publish_id: 'b', as_of: '2026-10-26 12:00:00Z' }),
            set({ publish_id: 'c', as_of: '2026-10-27 12:00:00Z' }),
            set({ publish_id: 'd', as_of: '2026-10-27 12:00:00Z' }),
        ];
        expect(baselineChoicesOf(sets)).toEqual([
            '2026-10-27 12:00:00Z',
            '2026-10-26 12:00:00Z',
            '2026-10-25 12:00:00Z',
        ]);
    });

    it('reads the instant a set last moved', () => {
        expect(lastChangedAt(set())).toBe('2026-10-27 16:07:08Z');
        expect(lastChangedAt(set({ withdrawn_at: '2026-10-27 17:05:00Z' }))).toBe(
            '2026-10-27 17:05:00Z',
        );
    });
});

describe('a published value and the other side it implies', () => {
    it('reads a tenor as the time it covers', () => {
        expect(yearsOf('12M')).toBeCloseTo(1);
        expect(yearsOf('2Y')).toBe(2);
        expect(yearsOf('6M')).toBeCloseTo(0.5);
        expect(yearsOf('1Y6M')).toBeCloseTo(1.5);
        expect(yearsOf('ON')).toBeCloseTo(1 / 365);
        expect(yearsOf('')).toBeUndefined();
        expect(yearsOf('spot')).toBeUndefined();
    });

    it('derives the rate a published discount factor states and the factor a rate states', () => {
        const discount = quoted(point('12M', '0.95803793830235673'));
        expect(discount.discount).toBeCloseTo(0.95803793830235673);
        // A factor of e^-r for one year implies a continuous rate of about 4.29%.
        expect(discount.rate).toBeCloseTo(0.04287, 4);

        const rate = quoted(point('2Y', '0.05', 'rate'));
        expect(rate.rate).toBe(0.05);
        expect(rate.discount).toBeCloseTo(Math.exp(-0.05 * 2));

        // A pillar that is not a period leaves the derived side empty rather
        // than inventing a time behind it.
        expect(quoted(point('spot', '0.05', 'rate')).discount).toBeUndefined();
        expect(numberOf('')).toBeUndefined();
        expect(numberOf('abc')).toBeUndefined();
        expect(numberOf('0.5')).toBe(0.5);
    });
});

describe('the comparison of two sets', () => {
    const baseline = [
        point('1Y', '0.04', 'rate'),
        point('2Y', '0.05', 'rate'),
        point('3Y', '0.06', 'rate'),
        point('4Y', '0.07', 'rate'),
    ];
    const changed = [
        point('1Y', '0.041', 'rate'),
        point('2Y', '0.05', 'rate'),
        point('3Y', '0.06', 'rate'),
        point('5Y', '0.08', 'rate'),
    ];

    it('marks each pillar changed, added, dropped or unchanged', () => {
        const rows = comparePoints(baseline, changed);

        expect(rows.map((row) => [row.pillar, row.mark])).toEqual([
            ['1Y', 'changed'],
            ['2Y', 'unchanged'],
            ['3Y', 'unchanged'],
            ['4Y', 'dropped'],
            ['5Y', 'added'],
        ]);
        expect(tallyOf(rows)).toEqual({ changed: 1, added: 1, dropped: 1, unchanged: 2 });
        expect(somethingChanged(rows)).toBe(true);
    });

    it('states a change in basis points when the value is a rate, and in the unit otherwise', () => {
        const rows = comparePoints([point('1Y', '0.04', 'rate')], [point('1Y', '0.0415', 'rate')]);
        expect(rows[0]?.unit).toBe('bp');
        expect((rows[0]?.delta ?? 0) * 10000).toBeCloseTo(15);

        const factors = comparePoints([point('1Y', '0.96')], [point('1Y', '0.965')]);
        expect(factors[0]?.unit).toBe('');
        expect(factors[0]?.delta).toBeCloseTo(0.005);
    });

    it('says in words when nothing changed', () => {
        const rows = comparePoints(baseline, baseline);
        expect(somethingChanged(rows)).toBe(false);
        expect(tallyOf(rows).unchanged).toBe(4);
    });
});

describe('the overlay', () => {
    it('aligns the sets on the pillars either holds, and a pillar a set lacks is a gap', () => {
        const { pillars, values } = alignOnPillars([
            { points: [point('1Y', '0.04'), point('2Y', '0.05')] },
            { points: [point('1Y', '0.041'), point('3Y', '0.06')] },
        ]);

        expect(pillars).toEqual(['1Y', '2Y', '3Y']);
        expect(values[0]).toEqual([0.04, 0.05, null]);
        expect(values[1]).toEqual([0.041, null, 0.06]);
    });

    it('states how many sets it drew, in the four cases the prototype names', () => {
        expect(overlayCaption(5, 1, 1).key).toBe('marketdata.published.points.one');
        expect(overlayCaption(5, 3, 3)).toEqual({
            key: 'marketdata.published.points.fewer',
            values: { asked: '5', published: '3' },
        });
        expect(overlayCaption(5, 8, 5)).toEqual({
            key: 'marketdata.published.points.newest',
            values: { count: '5' },
        });
    });
});

/** The screens, with the reads they make already answered. */
function render(node: ReactNode, url: string, seed: (client: QueryClient) => void): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    seed(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[url]}>
                    <Routes>
                        <Route path="/market-data/published-curves" element={node} />
                        <Route path="/market-data/published-curves/:curve" element={node} />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

/** The list's page as the table reads it, with nothing filtered. */
function seedList(client: QueryClient, sets: readonly PublishedCurveSet[], total: number): void {
    client.setQueryData(pageKey({ key: 'published-curves' }, FIRST_PAGE), { rows: sets, total });
    client.setQueryData(['my-parties'], {
        parties: [{ partyId: '3f1e2d4c-0000-4000-8000-000000000010', name: 'Acme' }],
    });
}

/** The record's two reads: the list it resolves by, and the sets it draws. */
function seedRecord(
    client: QueryClient,
    sets: readonly PublishedCurveSet[],
    publishedCount = sets.length,
): void {
    client.setQueryData(['published-curves', 'record', CURVE, PARTY], {
        sets,
        total: sets.length,
    });
    client.setQueryData(['published-curve-sets', SERIES, 5], {
        sets,
        publishedCount,
    });
}

describe('the published curves list', () => {
    it('draws a set with its curve, party, publisher, points and state', () => {
        const html = render(<PublishedCurvesPage />, '/market-data/published-curves', (client) =>
            seedList(client, [set()], 1),
        );

        expect(html).toContain('Published curves');
        expect(html).toContain(CURVE);
        expect(html).toContain(PARTY);
        expect(html).toContain('henry.jones');
        expect(html).toContain('2026-10-27 12:00:00Z');
        expect(html).toContain('>Published<');
        // A candidate is drawn only when a person asks for it, and the filters
        // that ask are on the screen.
        expect(html).toContain('Show candidates');
        expect(html).toContain('Show ended sets');
    });

    it('says what a published set is and why the list has no buttons', () => {
        const html = render(<PublishedCurvesPage />, '/market-data/published-curves', (client) =>
            seedList(client, [set()], 1),
        );

        expect(html).toContain('A published set is read only');
        expect(html).not.toContain('>Add<');
        expect(html).not.toContain('>Delete<');
    });

    it('says there are no published curves yet when the tenant holds none', () => {
        const html = render(<PublishedCurvesPage />, '/market-data/published-curves', (client) =>
            seedList(client, [], 0),
        );

        expect(html).toContain('There are no published curves yet.');
    });
});

describe('the published curve record', () => {
    it('states the subject and the act on the details tab', () => {
        const html = render(
            <PublishedCurvePage />,
            `/market-data/published-curves/${CURVE}?party=${PARTY}`,
            (client) => seedRecord(client, [set()]),
        );

        expect(html).toContain(CURVE);
        expect(html).toContain('The subject the request authorised');
        expect(html).toContain('The act');
        expect(html).toContain('henry.jones');
        expect(html).toContain(REQUEST);
    });

    it('says a withdrawn set left use, with its reason and account', () => {
        const ended = set({
            state_code: 'withdrawn',
            withdrawn_at: '2026-10-27 17:05:00Z',
            withdrawn_by: 'm.oduya',
            withdrawn_reason_code: 'common.stale_data',
        });
        const html = render(
            <PublishedCurvePage />,
            `/market-data/published-curves/${CURVE}?party=${PARTY}`,
            (client) => seedRecord(client, [ended]),
        );

        expect(html).toContain('Withdrawn');
        expect(html).toContain('The set ended');
        expect(html).toContain('m.oduya');
        expect(html).toContain('common.stale_data');
    });

    it('overlays the last sets and draws the newest set’s pillars', () => {
        const html = render(
            <PublishedCurvePage />,
            `/market-data/published-curves/${CURVE}?party=${PARTY}&tab=points`,
            (client) => seedRecord(client, [set()], 1),
        );

        expect(html).toContain('The last published sets');
        expect(html).toContain('Pillar');
        expect(html).toContain('12M');
        expect(html).toContain('2Y');
        expect(html).toContain('0.958038');
        expect(html).toContain('One published set is drawn.');
    });

    it('says when the chart cannot be drawn, and why', () => {
        const one = set({ points: [point('12M', '0.958')], point_count: 1 });
        const html = render(
            <PublishedCurvePage />,
            `/market-data/published-curves/${CURVE}?party=${PARTY}&tab=points`,
            (client) => seedRecord(client, [one], 1),
        );

        expect(html).toContain('The chart cannot be drawn. A curve needs at least two pillars.');
    });

    it('marks the changed, added and dropped pillars of the comparison', () => {
        const published = set();
        const next = candidate({
            points: [
                point('12M', '0.9581'),
                point('2Y', '0.92105010131696952'),
                point('3Y', '0.9'),
            ],
        });
        const html = render(
            <PublishedCurvePage />,
            `/market-data/published-curves/${CURVE}?party=${PARTY}&tab=compare`,
            (client) => seedRecord(client, [published, next]),
        );

        expect(html).toContain('The published set');
        expect(html).toContain('The candidate');
        expect(html).toContain('Nobody has approved it');
        expect(html).toContain('>Changed<');
        expect(html).toContain('>Added<');
        expect(html).toContain('New pillar');
        // An unchanged pillar is counted and hidden until the person asks for it.
        expect(html).toContain('Show 1 unchanged point(s)');
    });

    it('says in words when the candidate matches the published set point for point', () => {
        const published = set();
        const same = candidate({ points: published.points });
        const html = render(
            <PublishedCurvePage />,
            `/market-data/published-curves/${CURVE}?party=${PARTY}&tab=compare`,
            (client) => seedRecord(client, [published, same]),
        );

        expect(html).toContain(
            'No point changed. The candidate matches the published set at this as-of point for point.',
        );
    });

    it('warns that an ended baseline is still compared against, and that the desk prices from no set', () => {
        const ended = set({ state_code: 'withdrawn', withdrawn_at: '2026-10-27 17:05:00Z' });
        const html = render(
            <PublishedCurvePage />,
            `/market-data/published-curves/${CURVE}?party=${PARTY}&tab=compare`,
            (client) => seedRecord(client, [ended, candidate()]),
        );

        expect(html).toContain('The comparison still answers what the candidate changes');
    });

    it('draws the record’s versions with the decision, and offers no Revert', () => {
        const html = render(
            <PublishedCurvePage />,
            `/market-data/published-curves/${CURVE}?party=${PARTY}&tab=history`,
            (client) => {
                seedRecord(client, [set()]);
                client.setQueryData(
                    ['history', 'ores.marketdata.curve_publish', set().publish_id],
                    [
                        {
                            version: 2,
                            modifiedBy: 'henry.jones',
                            recordedAt: '2026-10-27 16:07:08Z',
                            fields: [
                                { name: 'Published By', value: 'henry.jones' },
                                { name: 'Change Reason Code', value: 'system.curve_published' },
                            ],
                            changes: [],
                        },
                        {
                            version: 1,
                            modifiedBy: 'henry.jones',
                            recordedAt: '2026-10-27 16:00:00Z',
                            fields: [{ name: 'Change Reason Code', value: 'system.new_record' }],
                            changes: [],
                        },
                    ],
                );
                client.setQueryData(['request', REQUEST], {
                    id: REQUEST,
                    version: 2,
                    kindCode: 'marketdata.curve_publish',
                    stateCode: 'approved',
                    requestedBy: 'henry.jones',
                    requestedAt: '2026-10-27 16:00:00Z',
                    reason: 'The desk prices off this curve',
                    expiresAt: null,
                    roles: [],
                    decision: {
                        decisionCode: 'approve',
                        decidedBy: 'm.oduya',
                        decidedAt: '2026-10-27 16:05:00Z',
                        comment: 'The fit is good.',
                    },
                    parts: [],
                    subjects: [],
                });
            },
        );

        expect(html).toContain('system.curve_published');
        expect(html).toContain('m.oduya');
        expect(html).toContain('The fit is good.');
        expect(html).toContain('There is no Revert');
        expect(html).not.toContain('>Revert<');
    });

    it('states the decision it cannot read as a gap, and not as a silence', () => {
        const html = render(
            <PublishedCurvePage />,
            `/market-data/published-curves/${CURVE}?party=${PARTY}&tab=history`,
            (client) => {
                seedRecord(client, [set()]);
                client.setQueryData(
                    ['history', 'ores.marketdata.curve_publish', set().publish_id],
                    [
                        {
                            version: 1,
                            modifiedBy: 'henry.jones',
                            recordedAt: '2026-10-27 16:00:00Z',
                            fields: [{ name: 'Change Reason Code', value: 'system.new_record' }],
                            changes: [],
                        },
                    ],
                );
                client.setQueryData(['request', REQUEST], {
                    id: REQUEST,
                    version: 2,
                    kindCode: 'marketdata.curve_publish',
                    stateCode: 'approved',
                    requestedBy: 'henry.jones',
                    requestedAt: '2026-10-27 16:00:00Z',
                    reason: 'The desk prices off this curve',
                    expiresAt: null,
                    roles: [],
                    decision: null,
                    parts: [],
                    subjects: [],
                });
            },
        );

        expect(html).toContain('The decision on this request is not readable here');
    });

    it('says a curve with no published set has none, and draws no tabs', () => {
        const html = render(
            <PublishedCurvePage />,
            `/market-data/published-curves/${CURVE}?party=${PARTY}`,
            (client) => seedRecord(client, [candidate()], 0),
        );

        expect(html).toContain('has no published set');
    });
});
