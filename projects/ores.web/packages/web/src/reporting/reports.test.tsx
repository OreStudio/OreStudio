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
import type { ReportDetail, ReportsPage } from '@ores/wire-protocol/browser';
import { textOf } from '../textOf.js';
import { accessOf, renderRecords } from '../refdata/testSupport.js';
import { reportKey } from './ReportPage.js';
import { REPORTS_QUERY_KEY, ReportsPage as ReportsScreen } from './ReportsPage.js';
import { ReportPage } from './ReportPage.js';

/**
 * The reporting screens, rendered from a seeded query cache.
 *
 * They read what a person reads: the cells of a row, the word each one states,
 * and the fields the edit form is filled from. Nothing here depends on how the
 * table or the form is laid out.
 */

const PARTY = '77777777-7777-4777-8777-777777777777';
const RUN = '99999999-9999-4999-8999-999999999999';
const NAME = 'ACME daily PnL';
const MONTH_END = 'ACME month-end risk';

const DAILY = {
    name: NAME,
    reportType: 'pnl',
    partyId: PARTY,
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

const MONTH = {
    name: MONTH_END,
    reportType: 'risk',
    partyId: PARTY,
    partyCode: null,
    scheduleExpression: '0 0 9 L * ?',
    health: 'never run' as const,
    stateName: '',
    latestRun: null,
    nextFireAt: null,
};

const PAGE: ReportsPage = {
    rows: [DAILY, MONTH],
    tally: {
        failed: 0,
        completedWithErrors: 1,
        running: 0,
        neverRun: 1,
        completedClean: 0,
        unstated: 0,
    },
    unread: [],
};

const DETAIL: ReportDetail = {
    report: DAILY,
    definition: {
        name: NAME,
        description: 'ACME daily PnL definition',
        reportType: 'pnl',
        partyId: PARTY,
        scheduleExpression: '0 0 9 * * ?',
        concurrencyPolicy: 'forbid',
        isOfficial: true,
        preProcessing: 'none',
        preparedInputKey: 'acme/daily',
        postProcessing: 'none',
        schedulerJobId: null,
        runGrantId: null,
        fsmStateId: null,
        version: 3,
        modifiedBy: 'priya',
        performedBy: 'priya',
        changeReasonCode: 'common.new_record',
        changeCommentary: '',
        recordedAt: '2026-10-17 08:00:00.000000Z',
    },
};

function list(seed: (client: QueryClient) => void = () => undefined): string {
    return renderRecords(
        '/reporting',
        [],
        <Route path="/reporting" element={<ReportsScreen />} />,
        (client) => {
            client.setQueryData(REPORTS_QUERY_KEY, PAGE);
            seed(client);
        },
    );
}

function detail(codes: string[] = ['reporting::report_definitions:write']): string {
    return renderRecords(
        `/reporting/${encodeURIComponent(NAME)}`,
        [],
        <Route path="/reporting/:name" element={<ReportPage />} />,
        (client) => {
            client.setQueryData(['my-access'], accessOf(codes));
            client.setQueryData(reportKey(NAME), DETAIL);
        },
    );
}

/** The text of the table row that names one definition, and no other row. */
function rowText(html: string, name: string): string {
    const start = html.split('<tr').find((part) => part.includes(name)) ?? '';
    return textOf(start.split('</tr>')[0] ?? '');
}

describe('the list of reports a party runs', () => {
    it('draws one row per definition, with the cells the server composed', () => {
        const row = rowText(list(), NAME);

        expect(row).toContain('ACME daily PnL');
        expect(row).toContain('ACMCOR');
        expect(row).toContain('0 0 9 * * ?');
        expect(row).toContain('completed with errors');
        expect(row).toContain('3 errors');
        expect(row).toContain('2 warnings');
        // The run started at 08:00:00 and completed at 08:00:12.5, stated as a length.
        expect(row).toContain('12.5 s');
        expect(row).toContain('next');
    });

    it('states a definition that never ran as never run, and not as clean', () => {
        const row = rowText(list(), MONTH_END);

        expect(row).toContain('never run');
        expect(row).not.toContain('completed');
        expect(row).toContain('—');
        expect(row).toContain('not stated');
    });

    it('counts the rows by health word before any row is read', () => {
        const page = textOf(list());

        expect(page).toContain('Health of the latest run');
        expect(page).toContain('completed with errors');
        expect(page).toContain('never run');
    });

    it('states a party that owns no definition in words', () => {
        const page = textOf(
            renderRecords(
                '/reporting',
                [],
                <Route path="/reporting" element={<ReportsScreen />} />,
                (client) => {
                    client.setQueryData(REPORTS_QUERY_KEY, {
                        rows: [],
                        tally: PAGE.tally,
                        unread: [],
                    });
                },
            ),
        );

        expect(page).toContain('This party owns no report definition');
    });

    it('states which read failed when a cell is empty for want of it', () => {
        const page = textOf(
            renderRecords(
                '/reporting',
                [],
                <Route path="/reporting" element={<ReportsScreen />} />,
                (client) => {
                    client.setQueryData(REPORTS_QUERY_KEY, {
                        rows: PAGE.rows,
                        tally: PAGE.tally,
                        unread: [{ cell: 'parties', message: 'Read refused.' }],
                    });
                },
            ),
        );

        expect(page).toContain('Party could not be read: Read refused.');
    });
});

describe('one report definition', () => {
    it('fills the edit form from the record it read', () => {
        const html = detail();

        // A textarea states its value as its content, an input as an attribute.
        expect(html).toContain('ACME daily PnL definition</textarea>');
        expect(html).toContain('value="0 0 9 * * ?"');
        expect(html).toContain('value="acme/daily"');
        expect(html).toContain('value="none"');
        expect(html).toContain('checked=""');
        expect(textOf(html)).toContain('Save');
    });

    it('states the fields the form does not write as read only', () => {
        const page = textOf(detail());

        expect(page).toContain('forbid');
        expect(page).toContain('common.new_record');
        expect(page).toContain('read only');
    });

    it('offers no form to a person who may not change a definition', () => {
        const page = textOf(detail(['reporting::report_definitions:read']));

        expect(page).not.toContain('Save');
        expect(page).toContain('Editing needs permission to change report definitions');
    });
});
