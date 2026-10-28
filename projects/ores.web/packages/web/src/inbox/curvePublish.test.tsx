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
import type { InboxRequestView } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { descriptorFor, DESCRIBED_KINDS, summaryOf } from './kinds.js';
import { RequestPage } from './RequestPage.js';
import { RequestsPage } from './RequestsPage.js';
import { createTranslator } from '../i18n/translate.js';
import { enFlat } from '../i18n/locales/en.js';

/**
 * The subject a curve publish carries in the requests queue, from
 * doc/prototypes/market_data/published-curves.org.
 *
 * A publish request is about a curve, a party and an as-of, and the request's own
 * header names none of them: the market data component holds the subject, keyed by
 * the request's id, and the join puts it in the row. What is checked here is that
 * the row says which kind it is, the curve it is about and the party and as-of it
 * was raised for, that the opened request shows the same subject with the way to
 * the comparison, and that every other kind's row is the row it was.
 */

const REQUEST = 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa';
const CURVE = 'USD-SOFR-ACME';
const PARTY = 'ACCOUS';

/** The request as a decider reads it: a curve publish, with its subject. */
function publishRequest(overrides: Partial<InboxRequestView> = {}): InboxRequestView {
    return {
        id: REQUEST,
        version: 2,
        kindCode: 'marketdata.curve_publish',
        stateCode: 'waiting',
        requestedBy: 'henry.jones',
        requestedAt: '2026-10-27 16:00:00Z',
        reason: 'The desk prices off this curve.',
        expiresAt: null,
        roles: [],
        decision: null,
        parts: [],
        subjects: [],
        publishedSubject: {
            publish_id: '3f1e2d4c-0000-4000-8000-000000000001',
            curve: CURVE,
            party: PARTY,
            party_id: '3f1e2d4c-0000-4000-8000-000000000010',
            series_id: '0a42775e-3da7-4c61-a732-6b62cb9f948f',
            as_of: '2026-10-27 12:00:00Z',
            point_count: 2,
            state_code: 'candidate',
            published_at: null,
        },
        ...overrides,
    } as unknown as InboxRequestView;
}

/** A role request, which names its roles and no curve. */
function roleRequest(): InboxRequestView {
    return {
        ...publishRequest(),
        kindCode: 'iam.role_grant',
        reason: 'I price the FX book.',
        roles: [
            {
                roleId: 'bbbbbbbb-bbbb-4bbb-bbbb-bbbbbbbbbbbb',
                name: 'Trading',
                description: 'Trading desk access',
            },
        ],
        publishedSubject: undefined,
    } as unknown as InboxRequestView;
}

function render(seed: (client: QueryClient) => void, element: ReactNode, path: string): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['my-access'], {
        roles: [{ roleId: 'r', permissionCodes: ['iam::roles:assign'] }],
    });
    client.setQueryData(['permissions'], [{ code: '*', description: 'Full access' }]);
    client.setQueryData(['my-requests'], { items: [], total: 0 });
    seed(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>
                        <Route path="/requests" element={element} />
                        <Route path="/requests/:id" element={element} />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the curve publish in the queue', () => {
    it('names the kind, the curve, the party, the as-of and the points in the row', () => {
        const html = render(
            (client) => {
                client.setQueryData(['request-queue'], {
                    items: [publishRequest()],
                    total: 1,
                    answered: [],
                });
            },
            <RequestsPage />,
            '/requests',
        );

        expect(html).toContain('marketdata.curve_publish');
        expect(html).toContain(CURVE);
        expect(html).toContain(PARTY);
        expect(html).toContain('2026-10-27 12:00:00Z');
        expect(html).toContain('2 points');
        // The kind's badge takes the wider column, and the row says which kind it
        // is beside the code, as the prototype draws it.
        expect(html).toContain('w-2/5');
    });

    it('leaves a role request’s row as one line of text, with no subject', () => {
        const html = render(
            (client) => {
                client.setQueryData(['request-queue'], {
                    items: [roleRequest()],
                    total: 1,
                    answered: [],
                });
            },
            <RequestsPage />,
            '/requests',
        );

        expect(html).toContain('Trading');
        expect(html).not.toContain('marketdata.curve_publish');
        expect(html).not.toContain('w-2/5');
    });

    it('shows the subject in the opened request, with the way to the comparison', () => {
        const html = render(
            (client) => {
                client.setQueryData(['request', REQUEST], publishRequest());
                client.setQueryData(['request-changes', REQUEST], { changes: [] });
                client.setQueryData(['request-checks', REQUEST], { lines: [] });
                client.setQueryData(['roles'], []);
            },
            <RequestPage />,
            `/requests/${REQUEST}`,
        );

        expect(html).toContain('The subject the request authorises');
        expect(html).toContain(CURVE);
        expect(html).toContain('Open the comparison');
        expect(html).toContain(
            '/market-data/published-curves/USD-SOFR-ACME?party=ACCOUS&amp;tab=compare&amp;asof=2026-10-27+12%3A00%3A00Z',
        );
    });

    it('says the read was refused when this account may not read the curves', () => {
        const html = render(
            (client) => {
                client.setQueryData(
                    ['request', REQUEST],
                    publishRequest({ publishedSubject: null, publishedSubjectState: 'refused' }),
                );
                client.setQueryData(['request-changes', REQUEST], { changes: [] });
                client.setQueryData(['request-checks', REQUEST], { lines: [] });
            },
            <RequestPage />,
            `/requests/${REQUEST}`,
        );

        expect(html).toContain('may not read the published curves');
        expect(html).not.toContain('holds no published set for it');
    });

    it('says the read found nothing when the component holds no set for it', () => {
        const html = render(
            (client) => {
                client.setQueryData(
                    ['request', REQUEST],
                    publishRequest({ publishedSubject: null, publishedSubjectState: 'missing' }),
                );
                client.setQueryData(['request-changes', REQUEST], { changes: [] });
                client.setQueryData(['request-checks', REQUEST], { lines: [] });
            },
            <RequestPage />,
            `/requests/${REQUEST}`,
        );

        expect(html).toContain('holds no published set for it');
        expect(html).not.toContain('may not read the published curves');
    });

    it('names the kind in the summary, so the title of the page says what it is about', () => {
        const { t } = createTranslator('en', enFlat, enFlat);
        const named = summaryOf(t, publishRequest());
        expect(named).toContain(CURVE);
        expect(named).toContain(PARTY);
        // A request whose subject the reader may not read still names its kind.
        expect(summaryOf(t, publishRequest({ publishedSubject: null }))).toBe('Curve publish');
    });

    it('describes the kind, so the screens share one descriptor for it', () => {
        expect(DESCRIBED_KINDS).toContain('marketdata.curve_publish');
        const descriptor = descriptorFor('marketdata.curve_publish');
        expect(descriptor.wideSubject).toBe(true);
        expect(descriptor.subjectCell).toBeDefined();
        // A curve publish keeps none of the shared line-by-line delta: its
        // comparison is the published curve's own tab.
        expect(descriptor.drawsDelta).toBe(false);
    });
});
