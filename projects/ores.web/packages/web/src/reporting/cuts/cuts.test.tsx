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
import type {
    AccountAccess,
    CutKindRecord,
    CutKindRow,
    CutRecord,
    CutRow,
} from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../../i18n/Provider.js';
import { FIRST_PAGE, pageKey } from '../../refdata/RecordList.js';
import { textOf } from '../../textOf.js';
import { CutPage } from './CutPage.js';
import { CutTypePage } from './CutTypePage.js';
import { CutTypesPage } from './CutTypesPage.js';
import { CutsPage, recentMonths } from './CutsPage.js';

/**
 * The cut screens, rendered from a seeded query cache.
 *
 * What is checked is what the screens promise: the types list derives the
 * signed-or-not from the approval kind and counts the cuts of each type, a
 * type's page draws its rules and its cuts under the month they were taken in,
 * the cuts list draws the label, the derived state with its basis and the run
 * tally, and a cut's page draws the four inputs of the body, the gaps it could
 * not fill and the acts that made it.
 */

const KIND: CutKindRow = {
    code: 'EOD',
    name: 'End of day',
    description: 'The day’s official revaluation of a book.',
    familyCode: 'end_of_day',
    familyName: 'End of day',
    signed: true,
    approvalKindCode: 'reporting.eod_cut_authorisation',
    approvalKindName: 'End of day cut authorisation',
    approvalsRequired: 1,
    decidePermissionCode: 'inbox::approvals:decide_finance',
    outputIsIndicative: false,
    resolutionRuleCode: 'same_day_close',
    resolutionRuleName: 'The close of the day the report covers',
    retentionMonths: 25,
    retentionDestinationCode: 'cold',
    legalHoldDefault: false,
    displayOrder: 10,
    cutsCount: 2,
    version: 3,
};

const FLASH: CutKindRow = {
    ...KIND,
    code: 'FLASH',
    name: 'Intraday flash',
    familyCode: 'intraday',
    familyName: 'Intraday',
    signed: false,
    approvalKindCode: null,
    approvalKindName: null,
    approvalsRequired: null,
    decidePermissionCode: null,
    outputIsIndicative: true,
    resolutionRuleCode: 'the_cut_named',
    resolutionRuleName: 'The cut the selection names',
    retentionMonths: 3,
    cutsCount: 0,
    displayOrder: 40,
    version: 1,
};

function cut(over: Partial<CutRow> & Pick<CutRow, 'id' | 'label'>): CutRow {
    return {
        state: 'authorised',
        kindCode: 'EOD',
        kindName: 'End of day',
        timepoint: '2026-10-06 16:00:00.000000Z',
        timepointTimezone: 'Europe/London',
        scopeCode: 'ACME-LONDON',
        businessCentreCode: 'LONDON',
        takenBy: 'a.reyes',
        takenAt: '2026-10-06 16:02:00.000000Z',
        approvedBy: 'm.silva',
        approvedAt: '2026-10-06 16:20:00.000000Z',
        refusedAt: null,
        requiresAuthorisation: true,
        outputIsIndicative: false,
        takeOrdinal: 1,
        runCount: 4,
        supersededById: null,
        ...over,
    };
}

const AUTHORISED = cut({ id: '11111111-1111-1111-1111-111111111111', label: 'EOD.ACME-LONDON.2026-10-06' });

const FLASH_CUT = cut({
    id: '22222222-2222-2222-2222-222222222222',
    label: 'FLASH.ACME-LONDON.2026-10-07 09:14:00.000000Z',
    kindCode: 'FLASH',
    kindName: 'Intraday flash',
    timepoint: '2026-10-07 09:14:00.000000Z',
    state: 'in_use',
    approvedBy: '',
    approvedAt: null,
    requiresAuthorisation: false,
    outputIsIndicative: true,
    takenBy: 'flash',
    runCount: 1,
});

const TAKEN = cut({
    id: '33333333-3333-3333-3333-333333333333',
    label: 'EOD.ACME-LONDON.2026-10-07',
    timepoint: '2026-10-07 16:00:00.000000Z',
    state: 'taken',
    approvedBy: '',
    approvedAt: null,
    runCount: 0,
});

function access(codes: string[]): AccountAccess {
    return {
        roles: [
            {
                roleId: '33333333-3333-3333-3333-333333333333' as AccountAccess['roles'][number]['roleId'],
                name: 'Finance',
                description: '',
                permissionCodes: codes,
                givenBy: 'priya',
                givenAt: '2026-10-04',
                reasonCode: 'access.new_joiner',
                commentary: '',
            },
        ],
    };
}

function typeRecord(kind: CutKindRow, cuts: readonly CutRow[]): CutKindRecord {
    return { kind, cuts, partial: false };
}

function cutRecord(over: Partial<CutRecord> = {}): CutRecord {
    return {
        cut: {
            id: AUTHORISED.id,
            version: 2,
            partyId: '44444444-4444-4444-4444-444444444444',
            kindCode: 'EOD',
            scopeCode: 'ACME-LONDON',
            businessCentreCode: 'LONDON',
            timepoint: '2026-10-06 16:00:00.000000Z',
            timepointTimezone: 'Europe/London',
            takeOrdinal: 1,
            cutTimeCode: 'LONDON-CLOSE',
            marketDataSnapshotId: '8f2a4c19-0000-4000-8000-000000000001',
            dealPopulationId: '8f2a4c19-0000-4000-8000-000000000002',
            referenceDataVersion: '2026.10.06',
            engineVersion: 'ORE 1.8.15',
            codeVersion: 'a1b2c3d',
            takenBy: 'a.reyes',
            takenAt: '2026-10-06 16:02:00.000000Z',
            requiresAuthorisation: true,
            approvalRequestId: '8f2a4c19-0000-4000-8000-000000000003',
            approvedBy: 'm.silva',
            approvedAt: '2026-10-06 16:20:00.000000Z',
            refusedAt: null,
            refusalComment: '',
            retentionMonths: 25,
            retentionDestinationCode: 'cold',
            legalHold: false,
            outputIsIndicative: false,
            modifiedBy: 'a.reyes',
            performedBy: 'ores_reporting_service',
            changeReasonCode: 'common.creation',
            changeCommentary: '',
            recordedAt: '2026-10-06 16:02:00.000000Z',
        },
        label: AUTHORISED.label,
        state: 'authorised',
        supersededById: null,
        supersededByLabel: null,
        kind: KIND,
        gaps: [],
        runCount: 4,
        ...over,
    };
}

function render(path: string, seed: (client: QueryClient) => void): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    seed(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>
                        <Route path="/refdata/cut-types" element={<CutTypesPage />} />
                        <Route path="/refdata/cut-types/:code" element={<CutTypePage />} />
                        <Route path="/reporting/cuts" element={<CutsPage />} />
                        <Route path="/reporting/cuts/:id" element={<CutPage />} />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

/** Seeds the first page of the type list, as the shared list reads it. */
function seedTypes(client: QueryClient, rows: readonly CutKindRow[]): void {
    client.setQueryData(['cuts', 'types'], { rows, total: rows.length, partial: false });
    client.setQueryData(pageKey({ key: 'cut-types' }, FIRST_PAGE), {
        rows,
        total: rows.length,
    });
}

describe('the cut types list', () => {
    it('names the columns and derives the signed-or-not from the approval kind', () => {
        const html = render('/refdata/cut-types', (client) => {
            seedTypes(client, [KIND, FLASH]);
            client.setQueryData(['my-access'], access(['reporting::cut_kinds:read']));
        });
        const page = textOf(html);
        expect(page).toContain('Cut types');
        expect(page).toContain('Family');
        expect(page).toContain('Signed');
        expect(page).toContain('Retention');
        // The end of day type points at an approval kind, so it is signed and
        // names the permission its decider needs.
        expect(page).toContain('End of day cut authorisation');
        expect(page).toContain('inbox::approvals:decide_finance');
        // The intraday type points at nothing, so nobody signs it.
        expect(page).toContain('No signature');
        expect(page).toContain('25 months');
        expect(page).toContain('to cold');
        expect(page).toContain('Indicative');
    });

    it('says that a type is read here and that a new one is not offered', () => {
        const html = render('/refdata/cut-types', (client) => {
            seedTypes(client, [KIND]);
            client.setQueryData(['my-access'], access(['reporting::cut_kinds:read']));
        });
        expect(textOf(html)).toContain('Creating a new one is not offered yet');
    });

    it('pages the types in the browser, because a firm holds a handful', () => {
        const html = render('/refdata/cut-types', (client) => {
            seedTypes(client, [KIND, FLASH]);
            client.setQueryData(['my-access'], access(['reporting::cut_kinds:read']));
        });
        expect(textOf(html)).toContain('1–2 of 2');
    });
});

describe('one cut type', () => {
    it('draws its rules, with who signs a cut of it and what the take copies', () => {
        const html = render('/refdata/cut-types/EOD', (client) => {
            client.setQueryData(['cuts', 'type', 'EOD'], typeRecord(KIND, [AUTHORISED]));
            client.setQueryData(['my-access'], access(['reporting::cut_kinds:read']));
        });
        const page = textOf(html);
        expect(page).toContain('End of day');
        expect(page).toContain('EOD · version 3');
        expect(page).toContain('The close of the day the report covers');
        expect(page).toContain('26 months'.replace('26', '25'));
        expect(page).toContain('Not held');
        expect(page).toContain('A cut copies its type’s authorisation and retention');
    });

    it('groups its cuts under the month they were taken in', () => {
        const second = cut({
            id: '55555555-5555-5555-5555-555555555555',
            label: 'EOD.ACME-LONDON.2026-10-05',
            timepoint: '2026-10-05 16:00:00.000000Z',
        });
        const html = render('/refdata/cut-types/EOD?tab=cuts', (client) => {
            client.setQueryData(['cuts', 'type', 'EOD'], typeRecord(KIND, [AUTHORISED, second]));
            client.setQueryData(['my-access'], access(['reporting::cut_kinds:read']));
        });
        const page = textOf(html);
        expect(page).toContain('2026-10');
        expect(page).toContain('2 cuts');
        expect(page).toContain('EOD.ACME-LONDON.2026-10-06');
        expect(page).toContain('EOD.ACME-LONDON.2026-10-05');
    });

    it('says a type has no cuts rather than drawing an empty table', () => {
        const html = render('/refdata/cut-types/FLASH?tab=cuts', (client) => {
            client.setQueryData(['cuts', 'type', 'FLASH'], typeRecord(FLASH, []));
            client.setQueryData(['my-access'], access(['reporting::cut_kinds:read']));
        });
        expect(textOf(html)).toContain('No cut of this type has been taken under this party');
    });
});

describe('the cuts list', () => {
    function seedCuts(client: QueryClient, rows: readonly CutRow[]): void {
        client.setQueryData(['cuts', 'types'], { rows: [KIND, FLASH], total: 2, partial: false });
        client.setQueryData(pageKey({ key: 'cuts' }, FIRST_PAGE), {
            rows,
            total: rows.length,
        });
        client.setQueryData(['my-access'], access(['reporting::cuts:read']));
    }

    it('draws the label, the state with its basis, the taker, the authoriser and the runs', () => {
        const html = render('/reporting/cuts', (client) => {
            seedCuts(client, [AUTHORISED, TAKEN, FLASH_CUT]);
        });
        const page = textOf(html);
        expect(page).toContain('Cuts');
        expect(page).toContain('EOD.ACME-LONDON.2026-10-06');
        expect(page).toContain('Authorised');
        expect(page).toContain('Authorised by m.silva');
        expect(page).toContain('a.reyes');
        expect(page).toContain('m.silva');
        // A cut awaiting its signature says so rather than reading as a gap.
        expect(page).toContain('Taken');
        expect(page).toContain('The body is fixed and waits on a person');
        // A cut in use with no signature says why it is admitted.
        expect(page).toContain('In use');
        expect(page).toContain('Its kind requires no authorisation');
        // The indicative mark is the kind's and stands beside the state.
        expect(page).toContain('Indicative');
    });

    it('offers the kind, the state and the month as the list’s filters', () => {
        const html = render('/reporting/cuts', (client) => {
            seedCuts(client, [AUTHORISED]);
        });
        const page = textOf(html);
        expect(page).toContain('Every kind');
        expect(page).toContain('Every state');
        expect(page).toContain('Every month');
        expect(html).toContain('placeholder="Search…"');
    });

    it('says that a cut is read here and that the take is not offered yet', () => {
        const html = render('/reporting/cuts', (client) => {
            seedCuts(client, [AUTHORISED]);
        });
        expect(textOf(html)).toContain('Taking one by hand is not offered on this screen yet');
    });
});

describe('one cut', () => {
    it('draws the four inputs of the body and the identity a person quotes', () => {
        const html = render(`/reporting/cuts/${AUTHORISED.id}`, (client) => {
            client.setQueryData(['cuts', 'cut', AUTHORISED.id], cutRecord());
            client.setQueryData(['my-access'], access(['reporting::cuts:read']));
        });
        const page = textOf(html);
        expect(page).toContain('EOD.ACME-LONDON.2026-10-06');
        expect(page).toContain('The identity');
        expect(page).toContain('The deals');
        expect(page).toContain('The market data');
        expect(page).toContain('The reference data');
        expect(page).toContain('The engine and the code');
        expect(page).toContain('Deal population');
        expect(page).toContain('ORE 1.8.15');
        expect(page).toContain('a1b2c3d');
        expect(page).toContain('25 months, to cold');
        // The page states what no read answers rather than leaving the card empty.
        expect(page).toContain('No read returns the deals a population holds yet');
    });

    it('draws the gaps a body could not close, and what would close each', () => {
        const html = render(`/reporting/cuts/${AUTHORISED.id}`, (client) => {
            client.setQueryData(
                ['cuts', 'cut', AUTHORISED.id],
                cutRecord({
                    state: 'incomplete',
                    gaps: [
                        {
                            id: '66666666-6666-6666-6666-666666666666',
                            partCode: 'market_data',
                            item: 'USD-SOFR-3M',
                            reason: 'No value at or before the instant.',
                        },
                    ],
                }),
            );
            client.setQueryData(['my-access'], access(['reporting::cuts:read']));
        });
        const page = textOf(html);
        expect(page).toContain('The gaps (1)');
        expect(page).toContain('USD-SOFR-3M');
        expect(page).toContain('No value at or before the instant.');
    });

    it('says on the authorisation tab who signs a cut of the kind and who decided it', () => {
        const html = render(`/reporting/cuts/${AUTHORISED.id}?tab=authorisation`, (client) => {
            client.setQueryData(['cuts', 'cut', AUTHORISED.id], cutRecord());
            client.setQueryData(['my-access'], access(['reporting::cuts:read']));
        });
        const page = textOf(html);
        expect(page).toContain('is signed before a run may price from it');
        expect(page).toContain('End of day cut authorisation');
        expect(page).toContain('inbox::approvals:decide_finance');
        expect(page).toContain('Four eyes is the inbox’s rule');
        expect(page).toContain('Raising and deciding an authorisation from this screen is not offered yet');
    });

    it('says a cut whose kind requires no authorisation was usable when taken', () => {
        const html = render(`/reporting/cuts/${AUTHORISED.id}?tab=authorisation`, (client) => {
            client.setQueryData(
                ['cuts', 'cut', AUTHORISED.id],
                cutRecord({
                    state: 'in_use',
                    kind: { ...KIND, signed: false, approvalKindCode: null, outputIsIndicative: true },
                    cut: {
                        ...cutRecord().cut,
                        requiresAuthorisation: false,
                        approvedBy: '',
                        approvedAt: null,
                        outputIsIndicative: true,
                    },
                }),
            );
            client.setQueryData(['my-access'], access(['reporting::cuts:read']));
        });
        const page = textOf(html);
        expect(page).toContain('requires no authorisation: the basis is a fact of the kind');
        expect(page).toContain('No signature');
    });
});

describe('the month the list offers', () => {
    it('names this month and the eleven before it, newest first', () => {
        const months = recentMonths(new Date(Date.UTC(2026, 9, 7)));
        expect(months[0]).toBe('2026-10');
        expect(months[11]).toBe('2025-11');
        expect(months).toHaveLength(12);
    });
});
