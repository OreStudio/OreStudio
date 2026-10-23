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
 */

import { afterEach, describe, expect, it, vi } from 'vitest';
import type { QueryClient } from '@tanstack/react-query';
import { Route } from 'react-router';
import { renderToStaticMarkup } from 'react-dom/server';
import { TranslationProvider } from '../i18n/Provider.js';
import type { EngineDocument, EngineDocumentation, EngineSummary } from '../api/engineSettings.js';
import { MY_PARTIES_QUERY_KEY } from '../refdata/partyScope.js';
import { FIRST_PAGE, pageKey } from '../refdata/RecordList.js';
import { accessOf, renderRecords } from '../refdata/testSupport.js';
import { textOf } from '../textOf.js';
import { EngineSettingsPage, pageOf } from './EngineSettingsPage.js';
import {
    DOCUMENTATION_KEY,
    EngineSettingsRecordPage,
    SettingRow,
    engineSettingsKey,
} from './EngineSettingsRecordPage.js';

const acting = vi.hoisted(() => ({
    party: undefined as string | undefined,
    canSwitch: true,
}));

vi.mock('../session/SessionProvider.js', () => ({
    useActingPartyId: () => acting.party,
    useSwitchParty: () => (acting.canSwitch ? async () => undefined : undefined),
}));

/**
 * The engine settings screens, rendered from a seeded query cache.
 *
 * They read what a person reads: the cells of a row, the words a tab states and
 * the actions a person may take. What a change does is the model's, and the
 * model's own tests cover it.
 */

const HK = '55555555-5555-4555-8555-555555555555';
const TRIAL = '66666666-6666-4666-8666-666666666666';

const ROWS: readonly EngineSummary[] = [
    {
        id: HK,
        partyId: 'p-hk',
        partyName: 'ACME Corporation HK',
        partyCode: 'HK',
        version: 4,
        name: 'HK standard',
        description: 'What the Hong Kong reports share',
        configurationId: '99999999-9999-4999-8999-999999999999',
        groupsSet: 2,
        groups: 3,
        threads: 4,
        modifiedBy: 'analyst',
        recordedAt: '2026-10-20 09:00:00.000000Z',
    },
    {
        id: TRIAL,
        partyId: 'p-hk',
        partyName: 'ACME Corporation HK',
        partyCode: 'HK',
        version: 1,
        name: 'Trial',
        description: '',
        configurationId: '99999999-9999-4999-8999-999999999998',
        groupsSet: 0,
        groups: 3,
        threads: null,
        modifiedBy: 'analyst',
        recordedAt: '2026-10-18 09:00:00.000000Z',
    },
];

const part = (
    id: string,
    version: number,
    values: Record<string, boolean | number | string | null>,
) => ({
    id,
    version,
    values,
    modifiedBy: 'analyst',
    recordedAt: '2026-10-20 09:00:00.000000Z',
    changeReason: 'common.correction',
    changeCommentary: '',
});

const DOCUMENT: EngineDocument = {
    id: HK,
    partyId: 'p-hk',
    partyName: 'ACME Corporation HK',
    partyCode: 'HK',
    version: 4,
    name: 'HK standard',
    description: 'What the Hong Kong reports share',
    configurationId: '99999999-9999-4999-8999-999999999999',
    modifiedBy: 'analyst',
    recordedAt: '2026-10-20 09:00:00.000000Z',
    changeReason: 'common.correction',
    changeCommentary: '',
    groups: {
        fixings: part('a', 2, { imply_todays_fixings: true, ignore_fixing_lag: null }),
        building: null,
        execution: part('b', 1, { n_threads: 4, log_mask: null, gzip_compression_level: null }),
    },
    reports: [{ id: '77777777-7777-4777-8777-777777777777', name: 'HK daily VaR' }],
};

const setting = (
    column: string,
    ore: string,
    label: string,
    overrides: Partial<EngineDocumentation['groups'][number]['settings'][number]> = {},
): EngineDocumentation['groups'][number]['settings'][number] => ({
    column,
    ore,
    kind: 'flag',
    changesNumbers: true,
    options: [],
    min: null,
    max: null,
    documented: true,
    label,
    help: 'A flag.',
    description: `What ${label} decides.`,
    defaultText: 'No. The engine default.',
    format: 'A flag, written Y or N.',
    introducedIn: '1.8.0.7 or earlier',
    removedIn: '',
    xpath: `/ORE/Setup/Parameter[@name='${ore}']`,
    ...overrides,
});

const DOCUMENTATION: EngineDocumentation = {
    groups: [
        {
            id: 'fixings',
            changesNumbers: true,
            settings: [
                setting('imply_todays_fixings', 'implyTodaysFixings', 'Imply today’s fixings'),
                setting('ignore_fixing_lag', 'ignoreFixingLag', 'Ignore fixing lag', {
                    kind: 'whole',
                    min: 0,
                }),
            ],
        },
        {
            id: 'building',
            changesNumbers: true,
            settings: [setting('build_failed_trades', 'buildFailedTrades', 'Build failed trades')],
        },
        {
            id: 'execution',
            changesNumbers: false,
            settings: [
                setting('n_threads', 'nThreads', 'Threads', {
                    kind: 'whole',
                    changesNumbers: false,
                    min: 1,
                    defaultText: '1.',
                }),
                setting('log_mask', 'logMask', 'Log mask', {
                    kind: 'whole',
                    changesNumbers: false,
                    documented: false,
                    label: '',
                }),
            ],
        },
    ],
};

const WRITE = ['reporting::engine_settings:read', 'reporting::engine_settings:write'];
const READ = ['reporting::engine_settings:read'];

const party = (partyId: string, name: string) => ({
    partyId,
    name,
    shortCode: name.slice(0, 3).toUpperCase(),
    partyCategory: 'Operational',
    businessCenterCode: 'GBLO',
});

const ONE_PARTY = { defaultPartyId: 'p-hk', parties: [party('p-hk', 'ACME Corporation HK')] };
const TWO_PARTIES = {
    defaultPartyId: 'p-hk',
    parties: [party('p-hk', 'ACME Corporation HK'), party('p-uk', 'ACME Corporation UK')],
};

const ONE_PARTY_UK = { defaultPartyId: 'p-uk', parties: [party('p-uk', 'ACME Corporation UK')] };

afterEach(() => {
    acting.party = undefined;
    acting.canSwitch = true;
});

function list(codes: string[], parties = TWO_PARTIES): string {
    return renderRecords(
        '/reporting/engine-settings',
        [],
        <Route path="/reporting/engine-settings" element={<EngineSettingsPage />} />,
        (client: QueryClient) => {
            client.setQueryData(['my-access'], accessOf(codes));
            client.setQueryData(MY_PARTIES_QUERY_KEY, parties);
            client.setQueryData(pageKey({ key: 'engine-settings' }, FIRST_PAGE), {
                rows: ROWS,
                total: ROWS.length,
            });
        },
    );
}

function record(
    codes: string[],
    tab = '',
    document: EngineDocument = DOCUMENT,
    parties = TWO_PARTIES,
): string {
    return renderRecords(
        `/reporting/engine-settings/${HK}${tab === '' ? '' : `?tab=${tab}`}`,
        [],
        <Route path="/reporting/engine-settings/:id" element={<EngineSettingsRecordPage />} />,
        (client: QueryClient) => {
            client.setQueryData(['my-access'], accessOf(codes));
            client.setQueryData(MY_PARTIES_QUERY_KEY, parties);
            client.setQueryData(engineSettingsKey(HK), document);
            client.setQueryData(DOCUMENTATION_KEY, DOCUMENTATION);
        },
    );
}

describe('pageOf', () => {
    const page = { offset: 0, limit: 15, sort: 'name', descending: false, search: '', filters: {} };

    it('orders by name and pages the rows', () => {
        expect(pageOf(ROWS, page).rows.map((row) => row.name)).toEqual(['HK standard', 'Trial']);
        expect(pageOf(ROWS, { ...page, limit: 1, offset: 1 }).rows.map((row) => row.name)).toEqual([
            'Trial',
        ]);
    });

    it('orders a count as a number, and a missing thread count before any number', () => {
        expect(pageOf(ROWS, { ...page, sort: 'threads' }).rows.map((row) => row.name)).toEqual([
            'Trial',
            'HK standard',
        ]);
        expect(pageOf(ROWS, { ...page, sort: 'groupsSet', descending: true }).rows[0]?.name).toBe(
            'HK standard',
        );
    });

    it('keeps the rows of the party the filter names', () => {
        const mixed = [...ROWS, { ...ROWS[0]!, id: 'x', name: 'UK standard', partyId: 'p-uk' }];
        expect(
            pageOf(mixed, { ...page, filters: { party: 'p-uk' } }).rows.map((row) => row.name),
        ).toEqual(['UK standard']);
        expect(pageOf(mixed, { ...page, filters: {} })).toMatchObject({ total: 3 });
    });

    it('searches the name, the description and the party, and counts what it found', () => {
        expect(pageOf(ROWS, { ...page, search: 'hong kong' })).toMatchObject({ total: 1 });
        expect(pageOf(ROWS, { ...page, search: 'corporation hk' })).toMatchObject({ total: 2 });
        expect(pageOf(ROWS, { ...page, search: 'nothing' })).toMatchObject({ total: 0, rows: [] });
    });
});

describe('the list of engine settings', () => {
    it('states each configuration with its groups and its threads', () => {
        const text = textOf(list(READ));
        expect(text).toContain('HK standard');
        expect(text).toContain('2 of 3');
        expect(text).toContain('0 of 3');
        expect(text).toContain('default');
        expect(text).toContain('ACME Corporation HK');
    });

    it('names the owning party and offers a party filter when the account works in several', () => {
        const text = textOf(list(READ));
        expect(text).toContain('Party');
        expect(text).toContain('All parties');
    });

    it('shows neither the party column nor the filter when the account works in one party', () => {
        const text = textOf(list(READ, ONE_PARTY));
        expect(text).toContain('HK standard');
        expect(text).not.toContain('All parties');
        expect(text).not.toContain('ACME Corporation HK');
    });

    it('opens each row by the id of the configuration', () => {
        expect(list(READ)).toContain(`href="/reporting/engine-settings/${HK}"`);
    });

    it('offers Add only to a person who may write', () => {
        expect(textOf(list(WRITE))).toContain('Add');
        expect(textOf(list(READ))).not.toContain('Add');
    });
});

describe('one configuration of engine settings', () => {
    it('opens on its details and says how much of it is set and who binds it', () => {
        const text = textOf(record(READ));
        expect(text).toContain('HK standard');
        expect(text).toContain('2 of 3');
        expect(text).toContain('1 reports');
        expect(text).toContain('What the Hong Kong reports share');
    });

    it('names the three groups, the reports and the history as tabs', () => {
        const text = textOf(record(READ));
        expect(text).toContain('Fixings and cash flows');
        expect(text).toContain('Market and trade building');
        expect(text).toContain('Execution');
        expect(text).toContain('Used by');
        expect(text).toContain('History');
    });

    it('shows a group with the value of each setting, and the default of one it does not set', () => {
        const text = textOf(record(READ, 'fixings'));
        expect(text).toContain('Imply today’s fixings');
        expect(text).toContain('Yes');
        expect(text).toContain('Ignore fixing lag');
        expect(text).toContain('Default: No');
        expect(text).toContain('version 2');
        expect(text).toContain('Changes the numbers');
    });

    it('says a group with no row is not set and that the engine uses its defaults', () => {
        const text = textOf(record(READ, 'building'));
        expect(text).toContain('not set');
        expect(text).toContain('holds no row for Market and trade building');
    });

    it('marks an operational group, and a setting with no documentation, by its ORE name', () => {
        const text = textOf(record(READ, 'execution'));
        expect(text).toContain('Operational');
        expect(text).toContain('logMask');
    });

    it('offers Edit only to a person who may write', () => {
        expect(textOf(record(WRITE, 'fixings'))).toContain('Edit');
        expect(textOf(record(READ, 'fixings'))).not.toContain('Edit');
    });

    it('offers the two filters and the Simple and Advanced switch', () => {
        const text = textOf(record(READ, 'fixings'));
        expect(text).toContain('Set in this configuration');
        expect(text).toContain('Changes the numbers');
        expect(text).toContain('Simple');
        expect(text).toContain('Advanced');
        expect(text).toContain('2 of 2 settings shown');
    });

    it('lists the reports that bind it, each by its address', () => {
        const html = record(READ, 'reports');
        expect(textOf(html)).toContain('HK daily VaR');
        expect(html).toContain('href="/reporting/reports/77777777-7777-4777-8777-777777777777"');
    });

    it('says when the reports that bind it could not be read', () => {
        expect(textOf(record(READ, 'reports', { ...DOCUMENT, reports: null }))).toContain(
            'could not be read',
        );
    });

    it('says plainly when no report binds it', () => {
        expect(textOf(record(READ, 'reports', { ...DOCUMENT, reports: [] }))).toContain(
            'No report binds these engine settings',
        );
    });
});

describe('a configuration another party owns', () => {
    it('says whose it is, offers the switch, and offers no edit', () => {
        acting.party = 'p-uk';
        const text = textOf(record(WRITE));
        expect(text).toContain('belong to ACME Corporation HK');
        expect(text).toContain('Switch to ACME Corporation HK');
        expect(text).not.toContain('Edit');
    });

    it('offers no switch to a party the account does not work in', () => {
        acting.party = 'p-uk';
        const text = textOf(record(WRITE, '', DOCUMENT, ONE_PARTY_UK));
        expect(text).toContain('belong to ACME Corporation HK');
        expect(text).not.toContain('Switch to');
    });

    it('says the history is in the owning party instead of reading it', () => {
        acting.party = 'p-uk';
        const text = textOf(record(WRITE, 'history'));
        expect(text).toContain('Switch to ACME Corporation HK to read it');
    });

    it('draws no notice and keeps Edit for the owning party', () => {
        acting.party = 'p-hk';
        const text = textOf(record(WRITE));
        expect(text).not.toContain('Switch to');
        expect(text).toContain('Edit');
    });
});

describe('a setting being changed', () => {
    const threads = DOCUMENTATION.groups[2]!.settings[0]!;
    const row = (problem: { key: string; params: Record<string, string> } | null): string =>
        renderToStaticMarkup(
            <TranslationProvider>
                <ul>
                    <SettingRow
                        setting={threads}
                        saved={1}
                        text="0"
                        changed
                        problem={problem}
                        open={false}
                        mode="simple"
                        onToggle={() => undefined}
                        onText={() => undefined}
                        onFocus={() => undefined}
                    />
                </ul>
            </TranslationProvider>,
        );

    it('marks the field and states the problem beside it', () => {
        const html = row({
            key: 'engineSettings.problem.min',
            params: { setting: 'Threads', min: '1' },
        });
        expect(html).toContain('aria-invalid="true"');
        expect(html).toContain('role="alert"');
        expect(textOf(html)).toContain('Threads must be at least 1.');
    });

    it('marks nothing when the text is a value the setting takes', () => {
        const html = row(null);
        expect(html).toContain('aria-invalid="false"');
        expect(html).not.toContain('role="alert"');
    });
});
