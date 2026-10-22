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
import type { QueryClient } from '@tanstack/react-query';
import { Route } from 'react-router';
import { wireTimestamp } from '@ores/wire-protocol/browser';
import type {
    ConfigurationDetail,
    ReportConfigurationBinding,
    ReportDetail,
    ReportsPage,
} from '@ores/wire-protocol/browser';
import type { ReportOpen } from '../api/client.js';
import { ApiFailure } from '../api/transport.js';
import { textOf } from '../textOf.js';
import { createTranslator } from '../i18n/translate.js';
import { enFlat } from '../i18n/locales/en.js';
import { FIRST_PAGE, pageKey } from '../refdata/RecordList.js';
import { accessOf, renderRecords } from '../refdata/testSupport.js';
import { reportKey } from './ReportPage.js';
import { editOf } from './store.js';
import { ConfigurationPage, bindingsKey, configurationKey } from './ConfigurationPage.js';
import { MY_PARTIES_QUERY_KEY, ReportsPage as ReportsScreen, unreadNotes } from './ReportsPage.js';
import { HealthSummary } from './shared.js';
import { ReportPage } from './ReportPage.js';

/**
 * The reporting screens, rendered from a seeded query cache.
 *
 * They read what a person reads: the cells of a row, the words a state states,
 * the address a row opens, and the fields the page states about one record.
 */

const HOME = '77777777-7777-4777-8777-777777777777';
const ELSEWHERE = '88888888-8888-4888-8888-888888888888';
const DAILY = '55555555-5555-4555-8555-555555555555';
const AUDIT_AT_HOME = '66666666-6666-4666-8666-666666666666';
const AUDIT_ELSEWHERE = 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa';
const RUN = '99999999-9999-4999-8999-999999999999';
const AUDIT_NAME = 'Audit Trail Summary';

const DAILY_ROW = {
    id: DAILY,
    name: 'ACME daily PnL',
    reportType: 'pnl',
    partyId: HOME,
    partyName: 'ACME Corporation',
    partyCode: 'ACMCOR',
    scheduleExpression: '0 0 9 * * ?',
    health: 'completed with errors' as const,
    stateName: 'completed',
    latestRun: {
        id: RUN,
        startedAt: wireTimestamp('2026-10-17 08:00:00.000000Z'),
        completedAt: wireTimestamp('2026-10-17 08:00:12.500000Z'),
        errors: 3,
        warnings: 2,
    },
    nextFireAt: wireTimestamp('2026-10-18 09:00:00.000000Z'),
};

const AUDIT_ROW = {
    id: AUDIT_ELSEWHERE,
    name: AUDIT_NAME,
    reportType: 'risk',
    partyId: ELSEWHERE,
    partyName: 'ACME Hong Kong',
    partyCode: 'ACCOHK',
    scheduleExpression: '0 0 9 L * ?',
    health: 'never run' as const,
    stateName: '',
    latestRun: null,
    nextFireAt: null,
};

const PAGE: ReportsPage = {
    rows: [DAILY_ROW, AUDIT_ROW],
    total: 2,
    unread: [],
};

const PARTIES = {
    defaultPartyId: HOME,
    parties: [
        {
            partyId: HOME,
            name: 'ACME Corporation',
            shortCode: 'ACMCOR',
            partyCategory: 'Operational',
            businessCenterCode: 'GBLO',
        },
        {
            partyId: ELSEWHERE,
            name: 'ACME Hong Kong',
            shortCode: 'ACCOHK',
            partyCategory: 'Operational',
            businessCenterCode: 'HKHH',
        },
    ],
};

const DETAIL: ReportDetail = {
    report: AUDIT_ROW,
    unread: [],
    definition: {
        id: AUDIT_ELSEWHERE,
        name: AUDIT_NAME,
        description: 'Audit trail for the Hong Kong party',
        report_type: 'risk',
        party_id: ELSEWHERE,
        party_name: 'ACME Hong Kong',
        party_code: 'ACCOHK',
        schedule_expression: '0 0 9 L * ?',
        concurrency_policy: 'forbid',
        is_official: true,
        regulatory_book_type: null,
        pre_processing: 'none',
        prepared_input_key: 'acme/audit',
        post_processing: 'none',
        version: 3,
        modified_by: 'priya',
        performed_by: 'priya',
        change_reason_code: 'common.non_material_update',
        change_commentary: '',
        recorded_at: '2026-10-17 08:00:00.000000Z',
    },
};

/** The list drawn at its own address, from a cache the test seeds. */
function list(
    seed: (client: QueryClient) => void = () => undefined,
    path = '/reporting/reports',
): string {
    return renderRecords(
        path,
        [],
        <Route path="/reporting/reports" element={<ReportsScreen />} />,
        (client) => {
            client.setQueryData(MY_PARTIES_QUERY_KEY, PARTIES);
            client.setQueryData(pageKey({ key: 'reports' }, FIRST_PAGE), PAGE);
            seed(client);
        },
    );
}

/** The words of the trail, so a test reads the path a person took to the page. */
function trailOf(html: string): string {
    const nav = html.match(/<nav aria-label="[^"]*"[^>]*>(.*?)<\/nav>/s);
    if (nav === null) {
        return '(no breadcrumb)';
    }
    return textOf(nav[1] ?? '')
        .replace(/›/g, ' › ')
        .replace(/\s+/g, ' ')
        .trim();
}

/** The text of the table row that names one definition, and no other row. */
function rowText(html: string, name: string): string {
    const start = html.split('<tr').find((part) => part.includes(name)) ?? '';
    return textOf(start.split('</tr>')[0] ?? '');
}

/** The detail page of one definition, drawn from a cache the test seeds. */
function detail(codes: string[] = ['reporting::report_definitions:write']): string {
    return renderRecords(
        `/reporting/reports/${AUDIT_ELSEWHERE}`,
        [],
        <Route path="/reporting/reports/:id" element={<ReportPage />} />,
        (client) => {
            client.setQueryData(['my-access'], accessOf(codes));
            client.setQueryData(reportKey(AUDIT_ELSEWHERE), {
                kind: 'record',
                detail: DETAIL,
            } satisfies ReportOpen);
        },
    );
}

/** A report page opened by an address segment, from a cache the test seeds. */
function openedBy(
    segment: string,
    seed: (client: QueryClient) => void,
    codes: string[] = ['reporting::report_definitions:write'],
): string {
    return renderRecords(
        `/reporting/reports/${encodeURIComponent(segment)}`,
        [],
        <Route path="/reporting/reports/:id" element={<ReportPage />} />,
        (client) => {
            client.setQueryData(['my-access'], accessOf(codes));
            seed(client);
        },
    );
}

/** The record a unique name resolves to. */
const UNIQUE_NAME = 'ACME daily PnL';
const UNIQUE_DETAIL: ReportDetail = {
    report: DAILY_ROW,
    unread: [],
    definition: {
        ...DETAIL.definition,
        id: DAILY,
        name: UNIQUE_NAME,
        party_id: HOME,
        party_name: 'ACME Corporation',
        party_code: 'ACMCOR',
        schedule_expression: '0 0 9 * * ?',
    },
};

/** The choices a shared name offers, as the BFF answers them. */
const SHARED_CHOICES = [
    {
        id: AUDIT_AT_HOME,
        name: AUDIT_NAME,
        partyName: 'ACME Corporation',
        scheduleExpression: '0 0 9 * * ?',
        health: 'completed with errors' as const,
        stateName: 'completed',
        hasRun: true,
    },
    {
        id: AUDIT_ELSEWHERE,
        name: AUDIT_NAME,
        partyName: 'ACME Hong Kong',
        scheduleExpression: '0 0 9 L * ?',
        health: 'never run' as const,
        stateName: '',
        hasRun: false,
    },
];

describe('the list of reports the parties run', () => {
    it('renders the trail Home, Reporting, Reports', () => {
        expect(trailOf(list())).toBe('Home › Reporting › Reports');
    });

    it('opens each row by the id that names one definition', () => {
        const html = list();

        // The name is shared by the same report in other parties, so the address
        // is the id and not the name.
        expect(html).toContain(`href="/reporting/reports/${AUDIT_ELSEWHERE}"`);
        expect(html).toContain(`href="/reporting/reports/${DAILY}"`);
        expect(html).not.toContain('href="/reporting/reports/Audit%20Trail%20Summary"');
    });

    it('states what it holds, and the parties it spans, in its lead sentence', () => {
        expect(textOf(list())).toContain(
            'The report definitions the parties you work in own, with the health of each latest run.',
        );
    });

    it('tags each party by its name and never by its code', () => {
        const row = rowText(list(), AUDIT_NAME);

        expect(row).toContain('ACME Hong Kong');
        expect(row).not.toContain('ACCOHK');
    });

    it('states the page the server returned, with the total it counted', () => {
        const page = textOf(
            list((client) =>
                client.setQueryData(pageKey({ key: 'reports' }, FIRST_PAGE), {
                    ...PAGE,
                    total: 78,
                }),
            ),
        );

        // The pager is the request's own numbers, and the size is the standard's
        // default; Load all is not offered for a total this size at this page.
        expect(page).toContain('1–2 of 78');
        expect(page).toContain('Page size');
        expect(page).toContain('15');
        expect(page).toContain('First');
        expect(page).toContain('Next');
    });

    it('draws the cells the server composed', () => {
        const row = rowText(list(), 'ACME daily PnL');

        expect(row).toContain('0 0 9 * * ?');
        expect(row).toContain('Completed with errors');
        expect(row).toContain('3');
        expect(row).toContain('2');
        // The run started at 08:00:00 and completed at 08:00:12.5, stated as a length.
        expect(row).toContain('12.5 s');
    });

    it('states a definition that never ran as never run, and not as clean', () => {
        const row = rowText(list(), AUDIT_NAME);

        expect(row).toContain('Never run');
        expect(row).not.toContain('completed');
        expect(row).toContain('—');
    });

    it('says it is loading before the first page arrives', () => {
        const page = textOf(
            list((client) => client.removeQueries({ queryKey: ['records', 'reports'] })),
        );

        expect(page).toContain('Loading...');
    });

    it('says the list holds nothing yet, in words', () => {
        const page = textOf(
            list((client) =>
                client.setQueryData(pageKey({ key: 'reports' }, FIRST_PAGE), {
                    rows: [],
                    total: 0,
                    unread: [],
                }),
            ),
        );

        expect(page).toContain('There are no reports yet.');
    });

    it('says which cell could not be read, in the words of the column that holds it', () => {
        expect(
            unreadNotes(
                [{ cell: 'latestRun', message: 'The latest run read was refused.' }],
                createTranslator('en', enFlat, enFlat).t,
            ),
        ).toEqual([
            {
                tone: 'warn',
                text: 'Latest run started could not be read: The latest run read was refused.',
            },
        ]);
    });

    it('draws the notes a read states above the table, and claims no word it did not read', () => {
        const html = list((client) =>
            client.setQueryData(pageKey({ key: 'reports' }, FIRST_PAGE), {
                rows: [{ ...AUDIT_ROW, health: null }],
                total: 1,
                notes: [{ tone: 'warn', text: 'The latest run read was refused.' }],
            }),
        );

        // Above the table, so a person reads why a cell is empty before the rows.
        expect(html.indexOf('The latest run read was refused.')).toBeLessThan(
            html.indexOf('<tbody'),
        );
        // No run was read, so the row claims no word at all.
        expect(rowText(html, AUDIT_NAME)).toContain('Unknown');
    });

    it('searches on the server, so the list has a search box and no sentence about a gap', () => {
        const html = list();

        expect(html).toContain('type="search"');
        expect(textOf(html)).not.toContain('The server does not search');
    });

    it('orders by the columns the server orders by, and by no other', () => {
        const html = list();
        const headers = html.match(/<th[^>]*>.*?<\/th>/gs) ?? [];
        const sortable = headers
            .filter((header) => header.includes('<button'))
            .map((header) => textOf(header));

        // The run columns belong to another read, so they carry no order of their own.
        expect(sortable).toEqual(['Report', 'Type', 'Schedule']);
        expect(textOf(html)).not.toContain('is declared sortable');
    });

    it('draws the health the server counted above the table, with the words in order', () => {
        const tally = {
            counts: [
                { health: 'failed', count: 2 },
                { health: 'completed with errors', count: 1 },
                { health: 'running', count: 0 },
                { health: 'never run', count: 1234 },
                { health: 'completed clean', count: 3 },
                { health: '', count: 0 },
            ],
            total: 1240,
        };
        const html = list((client) =>
            client.setQueryData(pageKey({ key: 'reports' }, FIRST_PAGE), {
                ...PAGE,
                summary: <HealthSummary tally={tally} />,
            }),
        );

        expect(html.indexOf('Reports by the health of their latest run')).toBeLessThan(
            html.indexOf('<tbody'),
        );
        const strip = textOf(
            html.match(/<ul aria-label="Reports by the health.*?<\/ul>/s)?.[0] ?? '',
        );
        expect(strip).toBe('Failed2Completed with errors1Running0Never run1,234Completed clean3');
        expect(textOf(html)).not.toContain('The server counts no definition');
    });

    it('counts a report whose run has no word yet apart, so no report goes missing', () => {
        const html = list((client) =>
            client.setQueryData(pageKey({ key: 'reports' }, FIRST_PAGE), {
                ...PAGE,
                summary: (
                    <HealthSummary
                        tally={{
                            counts: [
                                { health: 'failed', count: 0 },
                                { health: 'never run', count: 1 },
                                { health: '', count: 2 },
                            ],
                            total: 3,
                        }}
                    />
                ),
            }),
        );

        expect(textOf(html)).toContain('No word2');
    });

    it('offers the party filter, with the parties the session works in', () => {
        const html = list();

        expect(html).toContain('All parties');
        expect(html).toContain('ACME Hong Kong');
    });
});

describe('one report definition', () => {
    it('renders the trail Home, Reporting, Reports, then the name', () => {
        expect(trailOf(detail())).toBe(`Home › Reporting › Reports › ${AUDIT_NAME}`);
    });

    it('states the key and the version under the title', () => {
        // The id is shortened because it is an identifier; the version is the one read.
        expect(textOf(detail())).toContain('aaaaaaaa · version 3');
    });

    it('draws the party by its name, and links to the party it names', () => {
        const html = detail();

        expect(textOf(html)).toContain('ACME Hong Kong');
        expect(html).toContain(`href="/parties/${ELSEWHERE}"`);
        expect(textOf(html)).not.toContain('ACCOHK');
    });

    it('offers one Edit, on the card it changes, and none in the header', () => {
        const html = detail();

        // The specification puts Edit on the card, so the header carries none.
        expect(html.match(/>Edit</g)).toHaveLength(1);
        const header = html.slice(0, html.indexOf('Definition'));
        expect(header).not.toContain('>Edit<');
        expect(html).not.toContain('<textarea');
        expect(html).not.toContain('Schedule expression</label>');
    });

    it('explains what each value means in place of the generic fixed-field words', () => {
        const page = textOf(detail());

        expect(page).toContain('What the report is called.');
        expect(page).toContain('A full risk run on the compute grid');
        expect(page).toContain('The stored archive a run reads its input from');
        expect(page).not.toContain('never changed here');
    });

    it('states a known value by its word and explains it', () => {
        const page = textOf(
            openedBy(AUDIT_ELSEWHERE, (client) => {
                client.setQueryData(reportKey(AUDIT_ELSEWHERE), {
                    kind: 'record',
                    detail: {
                        ...DETAIL,
                        definition: {
                            ...DETAIL.definition,
                            pre_processing: 'execute',
                            post_processing: 'ignore',
                            schedule_expression: '30 18 * * 1-5',
                        },
                    },
                } satisfies ReportOpen);
            }),
        );

        expect(page).toContain('Execute');
        expect(page).toContain('At 18:30, Monday through Friday');
        expect(page).not.toContain('30 18 * * 1-5');
        expect(page).toContain('Each run builds the engine’s input itself');
        expect(page).toContain('A run records only that the engine produced results');
    });

    it('says a report with no book type reads books of every type', () => {
        const page = textOf(detail());

        expect(page).toContain('Book type');
        expect(page).toContain('Empty means the report reads books of every type.');
    });

    it('states a typed report by its book type and explains what it reads', () => {
        const page = textOf(
            openedBy(AUDIT_ELSEWHERE, (client) => {
                client.setQueryData(reportKey(AUDIT_ELSEWHERE), {
                    kind: 'record',
                    detail: {
                        ...DETAIL,
                        definition: { ...DETAIL.definition, regulatory_book_type: 'Trading' },
                    },
                } satisfies ReportOpen);
            }),
        );

        expect(page).toContain('Trading book');
        expect(page).toContain('The report reads trading books only.');
        expect(page).not.toContain('Empty means the report reads books of every type.');
    });

    it('states the fields the record holds', () => {
        const page = textOf(detail());

        expect(page).toContain('Definition');
        expect(page).toContain('Report type');
        expect(page).toContain('forbid');
    });

    it('closes the details with the provenance of the version it read', () => {
        const page = textOf(detail());

        expect(page).toContain('version 3');
        expect(page).toContain('priya');
        expect(page).toContain('common.non_material_update');
    });

    it('draws no History tab, because the versions cannot be read by id yet', () => {
        expect(detail()).not.toContain('role="tab"');
    });

    it('says a run it could not read was not checked, not that it never ran', () => {
        const page = textOf(
            renderRecords(
                `/reporting/reports/${AUDIT_ELSEWHERE}`,
                [],
                <Route path="/reporting/reports/:id" element={<ReportPage />} />,
                (client) => {
                    client.setQueryData(['my-access'], accessOf([]));
                    client.setQueryData(reportKey(AUDIT_ELSEWHERE), {
                        kind: 'record',
                        detail: {
                            report: { ...AUDIT_ROW, health: null },
                            definition: DETAIL.definition,
                            unread: [
                                {
                                    cell: 'latestRun',
                                    message: 'The latest run read was refused.',
                                },
                            ],
                        },
                    } satisfies ReportOpen);
                },
            ),
        );

        expect(page).toContain('could not be checked');
        expect(page).toContain('does not read as never run');
        expect(page).not.toContain('has never run');
    });

    it('offers no Edit to a person who may not change a definition', () => {
        const page = textOf(detail(['reporting::report_definitions:read']));

        expect(page).not.toContain('Edit');
        expect(page).toContain('Read only');
    });
});

describe('an address that names a definition', () => {
    it('opens the record when one definition carries the name', () => {
        const html = openedBy(UNIQUE_NAME, (client) => {
            client.setQueryData(reportKey(UNIQUE_NAME), {
                kind: 'record',
                detail: UNIQUE_DETAIL,
            } satisfies ReportOpen);
        });

        // The record is drawn at the name's address, so the page lands on it
        // while the address is replaced by the id's own.
        expect(trailOf(html)).toBe(`Home › Reporting › Reports › ${UNIQUE_NAME}`);
        expect(textOf(html)).toContain(UNIQUE_NAME);
        expect(textOf(html)).toContain('ACME Corporation');
        expect(textOf(html)).toContain('55555555 · version 3');
        expect(textOf(html)).not.toContain('No record matches');
    });

    it('offers one linked row per definition when several carry the name', () => {
        const html = openedBy(AUDIT_NAME, (client) => {
            client.setQueryData(reportKey(AUDIT_NAME), {
                kind: 'choice',
                name: AUDIT_NAME,
                choices: SHARED_CHOICES,
            } satisfies ReportOpen);
        });
        const page = textOf(html);

        expect(trailOf(html)).toBe(`Home › Reporting › Reports › ${AUDIT_NAME}`);
        expect(page).toContain('2 reports have this name. Choose the one you meant.');
        // Each row names its own party, schedule and latest run, so the rows are
        // told apart by what a person knows rather than by an identifier.
        expect(rowText(html, 'ACME Corporation')).toContain('0 0 9 * * ?');
        expect(rowText(html, 'ACME Corporation')).toContain('Completed with errors');
        expect(rowText(html, 'ACME Hong Kong')).toContain('0 0 9 L * ?');
        expect(rowText(html, 'ACME Hong Kong')).toContain('Never run');
        // One link each, to the address that names that definition exactly.
        expect(html).toContain(`href="/reporting/reports/${AUDIT_AT_HOME}"`);
        expect(html).toContain(`href="/reporting/reports/${AUDIT_ELSEWHERE}"`);
    });

    it('says the record is not there, and offers the list, for a name nobody has', () => {
        const html = openedBy('CVA/DVA/FVA', (client) => {
            client.setQueryDefaults(reportKey('CVA/DVA/FVA'), { retryOnMount: false });
            const query = client
                .getQueryCache()
                .build(client, { queryKey: reportKey('CVA/DVA/FVA') });
            query.setState({
                status: 'error',
                error: new ApiFailure(404, {
                    code: 'not-found',
                    message: 'The report_definition does not exist.',
                }),
                fetchStatus: 'idle',
            });
        });
        const page = textOf(html);

        expect(page).toContain('There is no such record, or you may not see it.');
        expect(page).toContain('Back to the list');
        expect(html).toContain('href="/reporting/reports"');
        // The entity's own name is not a word a person reads.
        expect(page).not.toContain('report_definition');
    });
});

const MARKET = 'c1000000-0000-4000-8000-000000000001';
const ENGINES = 'c2000000-0000-4000-8000-000000000002';

const BINDINGS: readonly ReportConfigurationBinding[] = [
    {
        id: 'b2',
        typeCode: 'pricing_engines',
        typeName: 'Pricing engines',
        configurationId: ENGINES,
        name: 'HK pricing engines',
    },
    {
        id: 'b1',
        typeCode: 'todays_market',
        typeName: "Today's market",
        configurationId: MARKET,
        name: '',
    },
];

function withBindings(bindings: readonly ReportConfigurationBinding[]): string {
    return renderRecords(
        `/reporting/reports/${AUDIT_ELSEWHERE}`,
        [],
        <Route path="/reporting/reports/:id" element={<ReportPage />} />,
        (client) => {
            client.setQueryData(['my-access'], accessOf([]));
            client.setQueryData(reportKey(AUDIT_ELSEWHERE), {
                kind: 'record',
                detail: DETAIL,
            } satisfies ReportOpen);
            client.setQueryData(bindingsKey(AUDIT_ELSEWHERE), bindings);
        },
    );
}

describe('the configurations of a report', () => {
    it('lists each by its kind, and links its name to the configuration page', () => {
        const html = withBindings(BINDINGS);
        const page = textOf(html);

        expect(page).toContain('Configurations');
        expect(page).toContain('Pricing engines');
        expect(page).toContain('HK pricing engines');
        expect(html).toContain(`href="/reporting/configurations/${ENGINES}"`);
        expect(html).toContain(`href="/reporting/configurations/${MARKET}"`);
    });

    it('shows the start of the id for a configuration whose name was not read', () => {
        expect(textOf(withBindings(BINDINGS))).toContain(MARKET.slice(0, 8));
    });

    it('says a report binds nothing in words', () => {
        expect(textOf(withBindings([]))).toContain('This report binds no configuration.');
    });
});

const CONFIGURATION: ConfigurationDetail = {
    configuration: {
        id: MARKET,
        name: "HK today's market",
        typeCode: 'todays_market',
        typeName: "Today's market",
        owningComponent: 'ores.analytics',
        partyId: ELSEWHERE,
        partyName: 'ACME Hong Kong',
        version: 2,
        modified_by: 'priya',
        performed_by: 'priya',
        change_reason_code: 'common.new_record',
        change_commentary: 'Seed',
        recorded_at: '2026-10-17 08:00:00.000000Z',
    },
    analytics: [
        { code: 'npv', name: 'Net present value', active: true },
        { code: 'sensitivity', name: 'Sensitivity analysis', active: false },
    ],
    reports: [
        {
            id: AUDIT_ELSEWHERE,
            name: AUDIT_NAME,
            partyName: 'ACME Hong Kong',
            regulatoryBookType: null,
        },
    ],
};

describe('the book type on a configuration page', () => {
    it('tags a report that reads one book type, and none that reads every type', () => {
        const typed = textOf(
            configurationPage({
                ...CONFIGURATION,
                reports: [
                    {
                        id: AUDIT_ELSEWHERE,
                        name: AUDIT_NAME,
                        partyName: 'ACME Hong Kong',
                        regulatoryBookType: 'Trading',
                    },
                ],
            }),
        );
        const untyped = textOf(configurationPage(CONFIGURATION));

        expect(typed).toContain('Trading book');
        expect(untyped).not.toContain('Trading book');
    });
});

function configurationPage(detail: ConfigurationDetail): string {
    return renderRecords(
        `/reporting/configurations/${MARKET}`,
        [],
        <Route path="/reporting/configurations/:id" element={<ConfigurationPage />} />,
        (client) => {
            client.setQueryData(['my-access'], accessOf([]));
            client.setQueryData(configurationKey(MARKET), detail);
        },
    );
}

describe('one configuration', () => {
    it('states its name, kind, owner and party, and offers no Edit', () => {
        const page = textOf(configurationPage(CONFIGURATION));

        expect(page).toContain("HK today's market");
        expect(page).toContain("Today's market");
        expect(page).toContain('ores.analytics');
        expect(page).toContain('ACME Hong Kong');
        expect(page).toContain('Read only');
        expect(page).not.toContain('Edit');
    });

    it('lists the analytics it holds and marks the ones that are off', () => {
        const page = textOf(configurationPage(CONFIGURATION));

        expect(page).toContain('Net present value');
        expect(page).toContain('Sensitivity analysis');
        expect(page.match(/Off/g)).toHaveLength(1);
    });

    it('links each report that binds it', () => {
        const html = configurationPage(CONFIGURATION);

        expect(textOf(html)).toContain('Reports that use it');
        expect(html).toContain(`href="/reporting/reports/${AUDIT_ELSEWHERE}"`);
    });

    it('says in words when no report that the reader may see uses it', () => {
        const page = textOf(configurationPage({ ...CONFIGURATION, reports: [] }));

        expect(page).toContain('No report that you may see uses this configuration.');
    });
});

describe('the write an edit of a report makes', () => {
    const row = { ...DETAIL.definition } as unknown as Record<string, unknown>;

    it('carries the book type the report holds', () => {
        expect(editOf({ ...row, regulatory_book_type: 'Trading' }).regulatory_book_type).toBe(
            'Trading',
        );
    });

    it('clears the book type when the row states none', () => {
        expect(editOf({ ...row, regulatory_book_type: null }).regulatory_book_type).toBeNull();
    });

    it('states nothing when the row was read before the field existed', () => {
        const { regulatory_book_type: _unread, ...older } = row;

        expect('regulatory_book_type' in editOf(older)).toBe(false);
    });
});
