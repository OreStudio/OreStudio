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
    CutKindChoices,
    CutKindRecord,
    CutKindRow,
    CutRecord,
    CutRow,
    CutTake,
} from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../../i18n/Provider.js';
import { FIRST_PAGE, pageKey } from '../../refdata/RecordList.js';
import { valuesOf, writeOf } from '../../refdata/records.js';
import { textOf } from '../../textOf.js';
import { CutPage } from './CutPage.js';
import { CutTypePage, formRowOf, specsOf } from './CutTypePage.js';
import { CutTypesPage } from './CutTypesPage.js';
import { CutsPage, PageSummary } from './CutsPage.js';
import { BodyTable, GapTable, PressResult, TakeCutPage } from './TakeCutPage.js';
import { frequencyOf, groupsOf, useGroupHeading } from './shared.js';

/**
 * The cut screens, rendered from a seeded query cache.
 *
 * What is checked is what the screens promise: the types list derives the
 * signed-or-not from the approval kind and counts the cuts of each type, a
 * type's page draws its rules and its cuts under the period one step above its
 * frequency, the cuts list draws the label, the derived state with its basis and
 * the run tally, and a cut's page draws the four inputs of the body, the gaps it
 * could not fill, the acts that made it and the runs that priced from it.
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

const CHOICES: CutKindChoices = {
    families: [
        { code: 'end_of_day', name: 'End of day' },
        { code: 'intraday', name: 'Intraday' },
    ],
    resolutionRules: [
        { code: 'same_day_close', name: 'The close of the day the report covers' },
        { code: 'the_cut_named', name: 'The cut the selection names' },
    ],
    approvalKinds: [
        { code: 'reporting.eod_cut_authorisation', name: 'End of day cut authorisation' },
    ],
    retentionDestinations: ['cold'],
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

const AUTHORISED = cut({
    id: '11111111-1111-1111-1111-111111111111',
    label: 'EOD.ACME-LONDON.2026-10-06',
});

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

const SUPERSEDED = cut({
    id: '44444444-4444-4444-4444-444444444444',
    label: 'EOD.ACME-LONDON.2026-10-05',
    timepoint: '2026-10-05 16:00:00.000000Z',
    state: 'superseded',
    supersededById: '11111111-1111-1111-1111-111111111111',
    runCount: 2,
});

const UNSIGNED_SUPERSEDED = cut({
    ...SUPERSEDED,
    id: '55555555-5555-5555-5555-555555555555',
    label: 'FLASH.ACME-LONDON.2026-10-04 09:00:00.000000Z',
    timepoint: '2026-10-04 09:00:00.000000Z',
    approvedBy: '',
    approvedAt: null,
    requiresAuthorisation: false,
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
        detail: {
            id: '8f2a4c19-0000-4000-8000-000000000004',
            requestId: '8f2a4c19-0000-4000-8000-000000000003',
            cutId: AUTHORISED.id,
            dealCount: 1284,
            marketDataSetCount: 7,
            gapCount: 0,
        },
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
                        <Route path="/reporting/cuts/take" element={<TakeCutPage />} />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

/** The name and paint of one label, as the catalogue answers it. */
function badge(
    code: string,
    label: string,
    backgroundColour: string,
): {
    code: string;
    label: string;
    description: string;
    backgroundColour: string;
    textColour: string;
    severity: string;
} {
    return {
        code,
        label,
        description: `What ${label} means.`,
        backgroundColour,
        textColour: '#ffffff',
        severity: 'info',
    };
}

/**
 * The DQ label catalogue, as the deployment holds it for the cut domains.
 *
 * The screen holds a kind's family code and a boolean, never the badge: the
 * domain, the badge and the mapping from the code are all the catalogue's, and
 * this is the shape the BFF answers with.
 */
const LABELS = {
    labels: [
        badge('cut_family_end_of_day', 'End of day', '#2563eb'),
        badge('cut_family_intraday', 'Intraday', '#f59e0b'),
        badge('cut_values_official', 'Official', '#475569'),
        badge('cut_values_indicative', 'Indicative', '#eab308'),
        badge('cut_authorisation_unsigned', 'No signature', '#6b7280'),
    ],
    domains: {
        cut_family: ['cut_family_end_of_day', 'cut_family_intraday'],
        cut_values: ['cut_values_official', 'cut_values_indicative'],
        cut_authorisation: ['cut_authorisation_unsigned'],
    },
    mapped: {
        cut_family: { end_of_day: 'cut_family_end_of_day', intraday: 'cut_family_intraday' },
        cut_values: { official: 'cut_values_official', indicative: 'cut_values_indicative' },
        cut_authorisation: { unsigned: 'cut_authorisation_unsigned' },
    },
};

/** Seeds the label catalogue the screens paint their coded values from. */
function seedLabels(client: QueryClient): void {
    client.setQueryData(['labels'], LABELS);
}

/** Seeds the first page of the type list, as the shared list reads it. */
function seedTypes(client: QueryClient, rows: readonly CutKindRow[]): void {
    client.setQueryData(['cuts', 'types'], { rows, total: rows.length, partial: false });
    client.setQueryData(pageKey({ key: 'cut-kinds' }, FIRST_PAGE), {
        rows,
        total: rows.length,
    });
}

describe('the cut types list', () => {
    it('names the columns and draws the signed-or-not the server derived', () => {
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

    it('paints the family and the values from the DQ label catalogue', () => {
        const html = render('/refdata/cut-types', (client) => {
            seedTypes(client, [KIND, FLASH]);
            seedLabels(client);
            client.setQueryData(['my-access'], access(['reporting::cut_kinds:read']));
        });
        // The family the row's own code names, in the colour the catalogue holds
        // for that code, and the two families the two rows belong to.
        expect(html).toContain('#2563eb');
        expect(html).toContain('#f59e0b');
        // The values column reads the same catalogue: official for the end of
        // day kind, indicative for the intraday one.
        expect(html).toContain('#475569');
        expect(html).toContain('#eab308');
    });

    it('names a family the interface has no words for as the catalogue names it', () => {
        const SCD: CutKindRow = {
            ...KIND,
            code: 'SCD',
            name: 'Settlement cycle',
            familyCode: 'settlement_cycle',
            familyName: 'Settlement cycle',
        };
        const html = render('/refdata/cut-types', (client) => {
            seedTypes(client, [SCD]);
            client.setQueryData(['labels'], {
                ...LABELS,
                labels: [
                    ...LABELS.labels,
                    badge('cut_family_settlement_cycle', 'Settlement cycle', '#0ea5e9'),
                ],
                mapped: {
                    ...LABELS.mapped,
                    cut_family: {
                        ...LABELS.mapped.cut_family,
                        settlement_cycle: 'cut_family_settlement_cycle',
                    },
                },
            });
            client.setQueryData(['my-access'], access(['reporting::cut_kinds:read']));
        });
        expect(textOf(html)).toContain('Settlement cycle');
        expect(html).toContain('#0ea5e9');
    });

    it('says that a type is read here and that a new one is not offered', () => {
        const html = render('/refdata/cut-types', (client) => {
            seedTypes(client, [KIND]);
            client.setQueryData(['my-access'], access(['reporting::cut_kinds:read']));
        });
        expect(textOf(html)).toContain('Creating a new one is not offered yet');
    });

    it('pages the types on the server, with the server’s total under the table', () => {
        const html = render('/refdata/cut-types', (client) => {
            // The page is what the server answered: rows and the server's total,
            // which the list draws in the shared pager.
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
        // The lead line is the key and the version the record holds.
        expect(page).toContain('EOD · version 3');
        expect(page).toContain('The close of the day the report covers');
        expect(page).toContain('25 months');
        expect(page).toContain('Not held');
        expect(page).toContain('A cut copies its type’s authorisation and retention');
        // The frequency is derived from the resolution rule.
        expect(page).toContain('Daily');
        // Approvals are the inbox kind's fact and not a column of the type.
        expect(page).not.toContain('Approvals required');
    });

    it('groups its cuts under the period one step above its frequency', () => {
        const second = cut({
            id: '99999999-9999-4999-8999-999999999999',
            label: 'EOD.ACME-LONDON.2026-10-05',
            timepoint: '2026-10-05 16:00:00.000000Z',
        });
        const html = render('/refdata/cut-types/EOD?tab=cuts', (client) => {
            client.setQueryData(['cuts', 'type', 'EOD'], typeRecord(KIND, [AUTHORISED, second]));
            client.setQueryData(['my-access'], access(['reporting::cut_kinds:read']));
        });
        const page = textOf(html);
        // A daily kind reads by month, and the heading names the month.
        expect(page).toContain('October 2026');
        expect(page).toContain('2 cuts');
        expect(page).toContain('EOD.ACME-LONDON.2026-10-06');
        expect(page).toContain('EOD.ACME-LONDON.2026-10-05');
    });

    it('marks a cut whose kind fixes indicative values', () => {
        const html = render('/refdata/cut-types/FLASH?tab=cuts', (client) => {
            client.setQueryData(['cuts', 'type', 'FLASH'], typeRecord(FLASH, [FLASH_CUT]));
            client.setQueryData(['my-access'], access(['reporting::cut_kinds:read']));
        });
        const page = textOf(html);
        expect(page).toContain('Indicative');
        expect(page).toContain('Its kind requires no authorisation');
    });

    it('says a type has no cuts rather than drawing an empty table', () => {
        const html = render('/refdata/cut-types/FLASH?tab=cuts', (client) => {
            client.setQueryData(['cuts', 'type', 'FLASH'], typeRecord(FLASH, []));
            client.setQueryData(['my-access'], access(['reporting::cut_kinds:read']));
        });
        expect(textOf(html)).toContain('No cut of this type has been taken under this party');
    });
});

describe('the type’s form', () => {
    it('offers the row’s own values, and keeps the retention destination out of it', () => {
        const specs = specsOf(CHOICES);
        const written = writeOf(specs, valuesOf(specs, formRowOf(KIND)));
        // The form states what the record holds, so a save that changes nothing
        // writes nothing new.
        expect(written).toMatchObject({
            code: 'EOD',
            name: 'End of day',
            family_code: 'end_of_day',
            approval_kind_code: 'reporting.eod_cut_authorisation',
            output_is_indicative: false,
            resolution_rule_code: 'same_day_close',
            retention_months: 25,
            legal_hold_default: false,
            display_order: 10,
        });
        // No lookup states the tiers, so the form does not ask for one: the BFF
        // keeps the destination the kind holds.
        expect(specs.map((spec) => spec.field)).not.toContain('retention_destination_code');
    });

    it('asks for a kind’s signature as a choice, and writes null for none', () => {
        const specs = specsOf(CHOICES);
        const approval = specs.find((spec) => spec.field === 'approval_kind_code');
        expect(approval?.optional).toBe(true);
        expect(approval?.kind.kind).toBe('pick');
        const written = writeOf(specs, {
            ...valuesOf(specs, formRowOf(FLASH)),
            approval_kind_code: '',
        });
        // A kind that names no approval kind writes null, which is the whole of
        // the rule that it needs no authorisation.
        expect(written['approval_kind_code']).toBeNull();
    });
});

describe('the period a type’s cuts are grouped by', () => {
    it('reads the frequency a kind is taken at from the rule it resolves by', () => {
        expect(frequencyOf('same_day_close')).toBe('daily');
        expect(frequencyOf('same_day_open')).toBe('daily');
        expect(frequencyOf('last_business_day_of_month_close')).toBe('monthly');
        expect(frequencyOf('last_business_day_of_year_close')).toBe('yearly');
        expect(frequencyOf('run_instant')).toBe('onDemand');
        expect(frequencyOf('the_cut_named')).toBe('onDemand');
    });

    it('groups a daily kind by month, a monthly one by year, a yearly one as one group, an on-demand one by day', () => {
        const rows = [AUTHORISED, SUPERSEDED];
        expect(groupsOf(rows, 'daily').map((group) => group.key)).toEqual(['2026-10']);
        expect(groupsOf(rows, 'monthly').map((group) => group.key)).toEqual(['2026']);
        expect(groupsOf(rows, 'yearly').map((group) => group.key)).toEqual(['all']);
        expect(groupsOf(rows, 'onDemand').map((group) => group.key)).toEqual([
            '2026-10-06',
            '2026-10-05',
        ]);
    });

    it('names a group’s period in the language the person reads', () => {
        // The probe asserts the four headings the hook states; rendering it is
        // what lets the hook read the language.
        renderToStaticMarkup(
            <TranslationProvider>
                <HeadingProbe />
            </TranslationProvider>,
        );
    });
});

/** The headings a group reads, drawn through the hook the type's page uses. */
function HeadingProbe(): null {
    const heading = useGroupHeading();
    expect(heading('2026-10', 'daily')).toBe('October 2026');
    expect(heading('2026', 'monthly')).toBe('Year 2026');
    expect(heading('all', 'yearly')).toBe('Every year');
    expect(heading('2026-10-06', 'onDemand')).toBe('6 October 2026');
    return null;
}

describe('the cuts list', () => {
    function seedCuts(client: QueryClient, rows: readonly CutRow[]): void {
        client.setQueryData(['cuts', 'types'], { rows: [KIND, FLASH], total: 2, partial: false });
        client.setQueryData(pageKey({ key: 'cuts' }, FIRST_PAGE), {
            rows,
            total: rows.length,
            summary: <PageSummary cuts={rows} />,
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

    it('says a superseded cut was authorised, or in use, before a later cut replaced it', () => {
        const html = render('/reporting/cuts', (client) => {
            seedCuts(client, [SUPERSEDED, UNSIGNED_SUPERSEDED]);
        });
        const page = textOf(html);
        expect(page).toContain('Authorised before a later cut replaced it');
        expect(page).toContain('In use until a later cut replaced it');
    });

    it('counts the bodies a run may not price from, and not a draft', () => {
        const draft = cut({
            id: '66666666-6666-6666-6666-666666666666',
            label: 'EOD.ACME-LONDON.2026-10-08',
            state: 'draft',
            takenBy: '',
            takenAt: null,
            approvedBy: '',
            approvedAt: null,
        });
        const html = render('/reporting/cuts', (client) => {
            seedCuts(client, [TAKEN, draft]);
        });
        const page = textOf(html);
        // One body is not consumable: the taken one. A draft is not a cut yet, so
        // it is not counted among the bodies a run refuses.
        expect(page).toContain('1 cuts of this scope may not be priced from');
        expect(page).toContain('and their states are: Taken');
    });

    it('offers the kind and the state as the list’s filters', () => {
        const html = render('/reporting/cuts', (client) => {
            seedCuts(client, [AUTHORISED]);
        });
        const page = textOf(html);
        expect(page).toContain('Every kind');
        expect(page).toContain('Every state');
        expect(html).toContain('placeholder="Search…"');
    });

    it('names the two things it still does not offer', () => {
        const html = render('/reporting/cuts', (client) => {
            seedCuts(client, [AUTHORISED]);
        });
        const page = textOf(html);
        // The take is here now, so the note names what is left and not it.
        expect(page).toContain('it is taken from here');
        expect(page).toContain('the report’s selection of this cut');
        expect(page).toContain('the LIVE mint');
    });

    it('offers the take only to a person who may take a cut', () => {
        const offered = render('/reporting/cuts', (client) => {
            seedCuts(client, [AUTHORISED]);
            client.setQueryData(
                ['my-access'],
                access(['reporting::cuts:read', 'reporting::cuts:take']),
            );
        });
        expect(textOf(offered)).toContain('Take a cut');

        const refused = render('/reporting/cuts', (client) => {
            seedCuts(client, [AUTHORISED]);
            client.setQueryData(['my-access'], access(['reporting::cuts:read']));
        });
        expect(textOf(refused)).not.toContain('Take a cut');
    });
});

describe('the take flow', () => {
    /** The kinds and their choices, as the flow's first step reads them. */
    function seedTake(client: QueryClient): void {
        client.setQueryData(['cuts', 'types', 'take'], {
            rows: [KIND, { ...KIND, code: 'LIVE', name: 'Live cut', familyCode: 'consumer' }],
            total: 2,
            partial: false,
        });
        client.setQueryData(['cuts', 'take-choices'], {
            families: [{ code: 'end_of_day', name: 'End of day' }],
            resolutionRules: [{ code: 'same_day_close', name: 'The close of the day' }],
            approvalKinds: [
                { code: 'reporting.eod_cut_authorisation', name: 'End of day cut authorisation' },
            ],
            retentionDestinations: ['cold'],
            businessCentres: [
                { code: 'GBLO', name: 'London' },
                { code: 'USNY', name: 'New York' },
            ],
        });
        client.setQueryData(
            ['my-access'],
            access(['reporting::cuts:read', 'reporting::cuts:take']),
        );
    }

    it('draws no take for a person who may not take a cut', () => {
        const html = render('/reporting/cuts/take', (client) => {
            seedTake(client);
            client.setQueryData(['my-access'], access(['reporting::cuts:read']));
        });
        const page = textOf(html);
        // A saved link to the flow is the door the server would refuse, so the
        // screen states the refusal before it draws a button.
        expect(page).toContain('You cannot open Take a cut yet');
        expect(page).not.toContain('Assemble the body');
    });

    it('offers the kinds a person takes, and says why LIVE is not one', () => {
        const html = render('/reporting/cuts/take', seedTake);
        const page = textOf(html);
        expect(page).toContain('The kind, the instant and the scope');
        expect(page).toContain('End of day');
        expect(page).toContain('inbox::approvals:decide_finance');
        // LIVE's instant comes from the run that consumes it, so the step says
        // so instead of offering a take nobody presses.
        expect(page).toContain('LIVE is not offered here');
        expect(page).toContain('Rates centre');
        expect(page).not.toContain('Live cut');
    });

    it('draws every part with the state it reached, and the gaps beneath them', () => {
        const answer: CutTake = {
            cutId: '',
            cutLabel: 'EOD.ACME-LONDON.2026-10-06',
            cutState: '',
            requiresAuthorisation: true,
            outputIsIndicative: false,
            retentionMonths: 25,
            retentionDestinationCode: 'cold',
            approvalRequestId: '',
            approvalKindCode: 'reporting.eod_cut_authorisation',
            approvalKindName: 'End of day cut authorisation',
            decidePermissionCode: 'inbox::approvals:decide_finance',
            parts: [
                {
                    partCode: 'kind',
                    state: 'closed',
                    summary: 'EOD · End of day cut',
                    reference: '',
                    item: '',
                    reason: '',
                    remedy: '',
                    snapshotNote: 'kept 25 months in the cold tier',
                },
                {
                    partCode: 'deal_population',
                    state: 'gapped',
                    summary: '',
                    reference: '',
                    item: 'the deal population',
                    reason: 'The store holds no deal at the instant.',
                    remedy: 'Correct the deal population.',
                    snapshotNote: '',
                },
            ],
            gaps: [
                {
                    id: '8f2a4c19-0000-4000-8000-000000000005',
                    partCode: 'deal_population',
                    item: 'the deal population',
                    reason: 'The store holds no deal at the instant.',
                },
            ],
            dealCount: 0,
            marketDataSetCount: 7,
            referenceDataVersion: '',
            engineVersion: 'ORE 1.8.15',
            codeVersion: '',
        };
        const page = textOf(
            renderToStaticMarkup(
                <TranslationProvider>
                    <BodyTable answer={answer} />
                    <GapTable answer={answer} />
                </TranslationProvider>,
            ),
        );
        expect(page).toContain('kept 25 months in the cold tier');
        expect(page).toContain('Closed');
        expect(page).toContain('Gap');
        expect(page).toContain('the deal population');
        // The remedy is the flow's own: the gap row says what closes it.
        expect(page).toContain('Correct the deal population.');
    });

    it('says what the press left, whether it raised a request or not', () => {
        const raised = textOf(
            renderToStaticMarkup(
                <TranslationProvider>
                    <MemoryRouter>
                        <PressResult
                            answer={
                                {
                                    cutId: AUTHORISED.id,
                                    cutLabel: 'EOD.ACME-LONDON.2026-10-06',
                                    cutState: 'taken',
                                    requiresAuthorisation: true,
                                    outputIsIndicative: false,
                                    retentionMonths: 25,
                                    retentionDestinationCode: 'cold',
                                    approvalRequestId: '8f2a4c19-0000-4000-8000-000000000003',
                                    approvalKindCode: 'reporting.eod_cut_authorisation',
                                    approvalKindName: 'End of day cut authorisation',
                                    decidePermissionCode: 'inbox::approvals:decide_finance',
                                    parts: [],
                                    gaps: [],
                                    dealCount: 1284,
                                    marketDataSetCount: 7,
                                    referenceDataVersion: '',
                                    engineVersion: '',
                                    codeVersion: '',
                                } satisfies CutTake
                            }
                        />
                    </MemoryRouter>
                </TranslationProvider>,
            ),
        );
        expect(raised).toContain('is frozen, and the request is with the deciders');
        expect(raised).toContain('inbox::approvals:decide_finance');
        expect(raised).toContain(
            'the inbox refuses a decision by the person who raised the request',
        );

        const draft = textOf(
            renderToStaticMarkup(
                <TranslationProvider>
                    <MemoryRouter>
                        <PressResult
                            answer={
                                {
                                    cutId: AUTHORISED.id,
                                    cutLabel: 'EOD.ACME-LONDON.2026-10-06',
                                    cutState: 'incomplete',
                                    requiresAuthorisation: true,
                                    outputIsIndicative: false,
                                    retentionMonths: 25,
                                    retentionDestinationCode: 'cold',
                                    approvalRequestId: '',
                                    approvalKindCode: 'reporting.eod_cut_authorisation',
                                    approvalKindName: 'End of day cut authorisation',
                                    decidePermissionCode: 'inbox::approvals:decide_finance',
                                    parts: [],
                                    gaps: [
                                        {
                                            id: '8f2a4c19-0000-4000-8000-000000000005',
                                            partCode: 'deal_population',
                                            item: 'the deal population',
                                            reason: 'The store holds no deal at the instant.',
                                        },
                                    ],
                                    dealCount: 0,
                                    marketDataSetCount: 7,
                                    referenceDataVersion: '',
                                    engineVersion: '',
                                    codeVersion: '',
                                } satisfies CutTake
                            }
                        />
                    </MemoryRouter>
                </TranslationProvider>,
            ),
        );
        expect(draft).toContain('saved as a draft with 1 gap(s)');
        expect(draft).toContain('No approval request was raised');
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
        // The lead line is the record's key and version.
        expect(page).toContain('· version 2');
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

    it('states that the label is missing because the kind is, and not the body', () => {
        const html = render(`/reporting/cuts/${AUTHORISED.id}`, (client) => {
            client.setQueryData(
                ['cuts', 'cut', AUTHORISED.id],
                cutRecord({ label: '', kind: { ...KIND, name: '' } }),
            );
            client.setQueryData(['my-access'], access(['reporting::cuts:read']));
        });
        expect(textOf(html)).toContain(
            'does not hold the kind EOD this cut was taken under, so it has no label',
        );
    });

    it('links the request that authorised the body rather than printing its id', () => {
        const html = render(`/reporting/cuts/${AUTHORISED.id}?tab=authorisation`, (client) => {
            client.setQueryData(['cuts', 'cut', AUTHORISED.id], cutRecord());
            client.setQueryData(['my-access'], access(['reporting::cuts:read']));
        });
        const page = textOf(html);
        expect(page).toContain('Open the request');
        expect(html).toContain('/requests/8f2a4c19-0000-4000-8000-000000000003');
        expect(page).not.toContain('8f2a4c19-0000-4000-8000-000000000003');
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
        // The subject the request is about, read from the detail row the take
        // wrote, so a decider reads the counts of the body it signs.
        expect(page).toContain('The body the request names');
        expect(page).toContain('1284');
        expect(page).toContain('Published sets in use');
        expect(page).not.toContain('Raising and deciding an authorisation');
    });

    it('says a cut whose kind requires no authorisation was usable when taken', () => {
        const html = render(`/reporting/cuts/${AUTHORISED.id}?tab=authorisation`, (client) => {
            client.setQueryData(
                ['cuts', 'cut', AUTHORISED.id],
                cutRecord({
                    state: 'in_use',
                    kind: {
                        ...KIND,
                        signed: false,
                        approvalKindCode: null,
                        outputIsIndicative: true,
                    },
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

    it('offers a new take of the same occasion for a refused body', () => {
        const base = cutRecord();
        const refused = cutRecord({
            state: 'refused',
            cut: {
                ...base.cut,
                refusedAt: '2026-10-06 16:30:00.000000Z',
                refusalComment: 'The population was not the one the day closed on.',
            },
        });
        const html = render(`/reporting/cuts/${AUTHORISED.id}?tab=authorisation`, (client) => {
            client.setQueryData(['cuts', 'cut', AUTHORISED.id], refused);
            client.setQueryData(
                ['my-access'],
                access(['reporting::cuts:read', 'reporting::cuts:take']),
            );
        });
        const page = textOf(html);
        expect(page).toContain('A refusal is terminal for this body');
        expect(page).toContain('Take it again');
        // The new take opens on the occasion that failed, so the flow fixes the
        // same kind, scope and centre without the person typing them again.
        expect(html).toContain(
            '/reporting/cuts/take?kind=EOD&amp;scope=ACME-LONDON&amp;centre=LONDON',
        );

        const notPermitted = render(
            `/reporting/cuts/${AUTHORISED.id}?tab=authorisation`,
            (client) => {
                client.setQueryData(['cuts', 'cut', AUTHORISED.id], refused);
                client.setQueryData(['my-access'], access(['reporting::cuts:read']));
            },
        );
        expect(textOf(notPermitted)).not.toContain('Take it again');
    });

    it('lists the runs that priced from it, with the state each reached', () => {
        const html = render(`/reporting/cuts/${AUTHORISED.id}?tab=runs`, (client) => {
            client.setQueryData(['cuts', 'cut', AUTHORISED.id], cutRecord());
            client.setQueryData(['cuts', 'cut', AUTHORISED.id, 'runs'], {
                runs: [
                    {
                        id: '77777777-7777-4777-8777-777777777777',
                        name: 'NPV',
                        stateName: 'completed',
                        health: 'completed clean',
                        startedAt: '2026-10-06 18:05:00.000000Z',
                        completedAt: '2026-10-06 18:09:00.000000Z',
                        pricedAt: '2026-10-06 18:08:00.000000Z',
                    },
                ],
            });
            client.setQueryData(['my-access'], access(['reporting::cuts:read']));
        });
        const page = textOf(html);
        expect(page).toContain('The runs that priced from this cut');
        expect(page).toContain('NPV');
        expect(page).toContain('Completed clean');
        // The comparison belongs to the audit journey, and the tab says so.
        expect(page).toContain('is the audit journey’s work');
    });

    it('says a body no run may price from has no runs', () => {
        const html = render(`/reporting/cuts/${AUTHORISED.id}?tab=runs`, (client) => {
            client.setQueryData(['cuts', 'cut', AUTHORISED.id], cutRecord({ state: 'taken' }));
            client.setQueryData(['cuts', 'cut', AUTHORISED.id, 'runs'], { runs: [] });
            client.setQueryData(['my-access'], access(['reporting::cuts:read']));
        });
        expect(textOf(html)).toContain('No run may price from a body that is not fixed');
    });
});
