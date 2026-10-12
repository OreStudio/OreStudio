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
import type { ReactNode } from 'react';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter, Route, Routes } from 'react-router';
import type { InboxRequestView, RequestChange, RequestChecks } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { enFlat } from '../i18n/locales/en.js';
import { createTranslator } from '../i18n/translate.js';
import { ReviewPanel } from './ReviewPanel.js';
import { RequestPage } from './RequestPage.js';
import { RequestsPage } from './RequestsPage.js';
import { DESCRIBED_KINDS, descriptorFor, summaryOf } from './kinds.js';

/**
 * The shared request screens, drawn for a kind that is not a role request.
 *
 * What is checked is what the task promises: a kind adds a descriptor and
 * nothing else, the request page shows the parts, the delta and the checks and
 * refuses the person who asked, and the review panel either writes or raises.
 */

const REQUEST = 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa';
const BOOK = 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb';

const { t } = createTranslator('en', enFlat, enFlat);

function bookRequest(overrides: Partial<InboxRequestView> = {}): InboxRequestView {
    return {
        id: REQUEST,
        version: 2,
        kindCode: 'refdata.book_change',
        stateCode: 'waiting',
        requestedBy: 'daniel',
        requestedAt: '2026-10-04 09:00:00Z',
        reason: 'Move the rates desk to operations.',
        expiresAt: '',
        roles: [],
        decision: null,
        parts: [
            {
                code: 'controller',
                name: 'Controller',
                answerOrder: 1,
                stateCode: 'approved',
                decidedBy: 'audit',
                decidedAt: '2026-10-04 10:00:00Z',
                comment: 'Fine by me',
            },
            {
                code: 'finance',
                name: 'Finance',
                answerOrder: 2,
                stateCode: 'waiting',
                decidedBy: '',
                decidedAt: '',
                comment: '',
            },
            {
                code: 'operations',
                name: 'Operations',
                answerOrder: 2,
                stateCode: 'waiting',
                decidedBy: '',
                decidedAt: '',
                comment: '',
            },
            {
                code: 'market_risk',
                name: 'Market risk',
                answerOrder: 3,
                stateCode: 'later',
                decidedBy: '',
                decidedAt: '',
                comment: '',
            },
        ],
        ...overrides,
    } as unknown as InboxRequestView;
}

const CHANGES: readonly RequestChange[] = [
    {
        lineNo: 1,
        operation: 'put',
        subjectId: BOOK,
        baseVersion: 4,
        applied: false,
        fields: [
            { name: 'cost_center', before: 'CC1', after: 'CC9', changed: true },
            { name: 'description', before: 'Rates', after: 'Rates', changed: false },
            { name: 'name', before: 'Desk', after: 'Desk', changed: false },
        ],
    },
    {
        lineNo: 2,
        operation: 'delete',
        subjectId: 'cccccccc-cccc-4ccc-8ccc-cccccccccccc',
        baseVersion: 1,
        applied: false,
        fields: [],
    },
];

const STANDING: RequestChecks = { lines: [], partCodes: [] };

function page(
    me: string,
    options: {
        request?: InboxRequestView;
        changes?: readonly RequestChange[];
        checks?: RequestChecks;
    } = {},
): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['permissions'], []);
    client.setQueryData(['request', REQUEST], options.request ?? bookRequest());
    client.setQueryData(['request-changes', REQUEST], options.changes ?? CHANGES);
    client.setQueryData(['request-checks', REQUEST], options.checks ?? STANDING);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[`/requests/${REQUEST}`]}>
                    <Routes>
                        <Route path="/requests/:id" element={<RequestPage me={me} />} />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the request page, for a book change', () => {
    it('names the kind, the person who asked and the reason they gave', () => {
        const html = page('priya');

        expect(html).toContain('Asks for Book change');
        expect(html).toContain('daniel');
        expect(html).toContain('Move the rates desk to operations.');
    });

    it('lists the parts in the order they answer, with who answered', () => {
        const html = page('priya');

        expect(html).toContain('Who must answer');
        expect(html.indexOf('Controller')).toBeLessThan(html.indexOf('Finance'));
        expect(html.indexOf('Finance')).toBeLessThan(html.indexOf('Market risk'));
        expect(html).toContain('Approved');
        expect(html).toContain('audit');
        expect(html).toContain('Fine by me');
        expect(html).toContain('Later');
    });

    it('draws each line as a field beside the value it would replace', () => {
        const html = page('priya');

        expect(html).toContain('Cost center');
        expect(html).toContain('CC1');
        expect(html).toContain('CC9');
        expect(html).toContain('read at version 4');
        // The fields a line leaves alone are folded away, not drawn as changes.
        expect(html).toContain('2 fields unchanged');
        expect(html).toContain('Removes it.');
    });

    it('says a refused check before the decider approves', () => {
        const html = page('priya', {
            checks: {
                lines: [
                    { lineNo: 1, operation: 'put', subjectId: BOOK, refusal: 'The book moved.' },
                ],
                partCodes: [],
            },
        });

        expect(html).toContain('Would be refused');
        expect(html).toContain('The book moved.');
        expect(html).toContain('A line would now be refused.');
    });

    it('says every line stands when the checks pass', () => {
        const html = page('priya', {
            checks: {
                lines: [{ lineNo: 1, operation: 'put', subjectId: BOOK, refusal: '' }],
                partCodes: [],
            },
        });

        expect(html).toContain('Every line would still stand.');
    });

    it('offers a plain approve and the part to answer for', () => {
        const html = page('priya');

        expect(html).toMatch(/<button[^>]*>Approve<\/button>/);
        expect(html).toContain('Answering for');
        expect(html).toContain('<option value="finance"');
        expect(html).toContain('<option value="operations"');
        expect(html).not.toContain('<option value="controller"');
    });

    it('will not let the person who asked answer their own request', () => {
        const html = page('daniel');

        expect(html).toContain('You asked for this yourself');
        expect(html).toMatch(/<button[^>]*disabled=""[^>]*>Approve<\/button>/);
    });

    it('says no turn has come when an earlier part has not answered', () => {
        const html = page('priya', {
            request: bookRequest({
                parts: [
                    {
                        code: 'controller',
                        name: 'Controller',
                        answerOrder: 1,
                        stateCode: 'waiting',
                        decidedBy: '',
                        decidedAt: '',
                        comment: '',
                    },
                ].map((p) => ({ ...p, stateCode: 'later' })),
            }),
        });

        expect(html).toContain('No part is waiting for an answer from you yet.');
        expect(html).toMatch(/<button[^>]*disabled=""[^>]*>Approve<\/button>/);
    });

    it('tells the maker the request was approved and could not be applied', () => {
        const html = page('daniel', { request: bookRequest({ stateCode: 'apply_failed' }) });

        expect(html).toContain('could not be made');
        expect(html).toContain('Could not apply');
        // An answered request offers no answer.
        expect(html).not.toContain('Your answer');
    });

    it('draws a kind it has no descriptor for, by its code', () => {
        const html = page('priya', { request: bookRequest({ kindCode: 'unknown.kind' }) });

        expect(html).toContain('Asks for unknown.kind');
        expect(html).toContain('Who must answer');
    });
});

describe('the kind descriptors', () => {
    it('name every kind the build describes, and fall back for one it does not', () => {
        for (const code of DESCRIBED_KINDS) {
            expect(descriptorFor(code).label(t)).not.toBe(code);
        }
        expect(descriptorFor('some.new_kind').label(t)).toBe('some.new_kind');
        expect(summaryOf(t, { kindCode: 'some.new_kind', roles: [] })).toBe('some.new_kind');
    });

    it('stands a role request for by the roles it names, and a book change by its kind', () => {
        expect(
            summaryOf(t, {
                kindCode: 'iam.role_grant',
                roles: [{ roleId: 'r', name: 'Trading', description: '' }],
            }),
        ).toBe('Trading');
        expect(summaryOf(t, { kindCode: 'refdata.book_change', roles: [] })).toBe('Book change');
    });

    it('words a field through the kind, and by its name when the kind has no word', () => {
        const book = descriptorFor('refdata.book_change');

        expect(book.fieldLabel(t, 'cost_center')).toBe('Cost center');
        expect(book.fieldLabel(t, 'a_column_nobody_named')).toBe('a_column_nobody_named');
    });
});

describe('the request queue, for a kind that is not a role request', () => {
    it('names the kind of each waiting request', () => {
        const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
        client.setQueryData(['permissions'], []);
        client.setQueryData(['my-access'], {
            roles: [{ roleId: 'r', permissionCodes: ['iam::roles:assign'] }],
        });
        client.setQueryData(['request-queue'], {
            items: [bookRequest()],
            total: 1,
            answered: [],
        });
        const html = renderToStaticMarkup(
            <QueryClientProvider client={client}>
                <TranslationProvider>
                    <MemoryRouter initialEntries={['/requests']}>
                        <Routes>
                            <Route path="/requests" element={<RequestsPage />} />
                        </Routes>
                    </MemoryRouter>
                </TranslationProvider>
            </QueryClientProvider>,
        );

        expect(html).toContain('Book change');
        expect(html).toContain('daniel');
        expect(html).toContain('Move the rates desk to operations.');
    });
});

function review(overrides: Partial<Parameters<typeof ReviewPanel>[0]> = {}): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <ReviewPanel
                kindCode="refdata.book_change"
                lines={[
                    {
                        key: '1',
                        label: 'Rates desk',
                        operation: 'put',
                        columns: ['cost_center'],
                        partCodes: ['finance'],
                        refusal: '',
                    },
                ]}
                reason=""
                onReason={() => undefined}
                pending={false}
                onWrite={() => undefined}
                onRaise={() => undefined}
                {...overrides}
            />
        </TranslationProvider>,
    );
}

describe('the review panel', () => {
    it('shows each change with the fields it changes and the function that decides it', () => {
        const html = review();

        expect(html).toContain('Rates desk');
        expect(html).toContain('Cost center');
        expect(html).toContain('Decided by:');
        expect(html).toContain('Finance');
    });

    it('raises a request when the policy names a part, and will not until a reason is given', () => {
        const html = review();

        expect(html).toContain('This goes to Finance.');
        expect(html).toMatch(/<button[^>]*disabled=""[^>]*>Raise the request<\/button>/);
        expect(html).not.toContain('>Save<');

        const reasoned = review({ reason: 'Open the desk' });

        expect(reasoned).toMatch(/<button(?![^>]*disabled="")[^>]*>Raise the request<\/button>/);
    });

    it('writes directly when the policy names no part', () => {
        const html = review({
            lines: [
                {
                    key: '1',
                    label: 'Rates desk',
                    operation: 'put',
                    columns: ['description'],
                    partCodes: [],
                    refusal: '',
                },
            ],
        });

        expect(html).toContain('they are written directly');
        expect(html).toMatch(/<button(?![^>]*disabled="")[^>]*>Save<\/button>/);
        expect(html).not.toContain('Raise the request');
    });

    it('stops at a refused change and says why', () => {
        const html = review({
            reason: 'Open the desk',
            lines: [
                {
                    key: '1',
                    label: 'Rates desk',
                    operation: 'put',
                    columns: ['cost_center'],
                    partCodes: ['finance'],
                    refusal: 'No such currency.',
                },
            ],
        });

        expect(html).toContain('No such currency.');
        expect(html).toContain('A change is refused.');
        expect(html).toMatch(/<button[^>]*disabled=""[^>]*>Raise the request<\/button>/);
    });

    it('shows the error of a write or raise that failed', () => {
        expect(review({ error: 'The server is down.' })).toContain('The server is down.');
    });
});

export type { ReactNode };
