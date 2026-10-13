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
import type { RecordRow } from '../api/client.js';
import { ApiFailure } from '../api/transport.js';
import { textOf } from '../textOf.js';
import { PartiesPage, PartyPage, TabPane } from './parties.js';
import { renderToStaticMarkup } from 'react-dom/server';
import { FIRST_PAGE, pageKey } from './RecordList.js';
import { accessOf, renderRecords, resourceView } from './testSupport.js';

/**
 * The party screens, rendered from a seeded query cache. They read what a
 * person reads: the names on the list and on the page, who may change what, and
 * where each thing leads. Nothing here depends on how the page is laid out.
 */

const ID = '11111111-1111-4111-8111-111111111111';
const PARENT = '22222222-2222-4222-8222-222222222222';

const REGISTRY = [
    resourceView('parties', ['id'], {
        search: true,
        sortable: ['short_code', 'full_name', 'party_type', 'status'],
    }),
    resourceView('party-identifiers', ['id']),
    resourceView('party-contacts', ['id'], { name: 'party_contact_informations' }),
    resourceView('party-countries', ['party_id', 'country_alpha2_code'], { versioned: false }),
    resourceView('party-currencies', ['party_id', 'currency_iso_code'], { versioned: false }),
    resourceView('party-counterparties', ['party_id', 'counterparty_id'], { versioned: false }),
    resourceView('business-units', ['id']),
];

/** The reads of the parts a party page draws a tab for. */
const PART_READS = [
    'party_identifiers',
    'party_contact_informations',
    'party_countries',
    'party_currencies',
    'business_units',
    'party_counterparties',
].map((name) => `refdata::${name}:read`);

const party: RecordRow = {
    version: 7,
    id: ID,
    short_code: 'ACME_UK',
    full_name: 'Acme UK Limited',
    codename: 'ores_brave_hopper',
    transliterated_name: null,
    party_category: 'Operational',
    party_type: 'Corporate',
    parent_party_id: PARENT,
    business_center_code: 'GBLO',
    status: 'Active',
    image_id: null,
    is_registration_default: false,
    modified_by: 'priya',
    performed_by: 'priya',
    change_reason_code: 'common.rectification',
    change_commentary: 'Fixed the name',
    recorded_at: '2026-10-04 22:10:00Z',
};

const parent: RecordRow = { ...party, id: PARENT, short_code: 'ACME', full_name: 'Acme Group' };

function seed(codes: string[], more: (client: QueryClient) => void = () => undefined) {
    return (client: QueryClient): void => {
        client.setQueryData(['my-access'], accessOf(codes));
        client.setQueryData(['records', 'parties', 'key', ID], party);
        client.setQueryData(['records', 'parties'], [party, parent]);
        more(client);
    };
}

function page(path: string, codes: string[], more?: (client: QueryClient) => void): string {
    return renderRecords(
        path,
        REGISTRY,
        <>
            <Route path="/refdata/parties" element={<PartiesPage />} />
            <Route path="/refdata/parties/:id" element={<PartyPage />} />
        </>,
        seed(codes, more),
    );
}

describe('the parties list', () => {
    const list = (codes: string[]): string =>
        page('/refdata/parties', codes, (client) =>
            client.setQueryData(pageKey({ key: 'parties' }, FIRST_PAGE), {
                rows: [party, parent],
                total: 30,
            }),
        );

    it('lists the parties by code and name, with their type and status', () => {
        const words = textOf(list(['refdata::parties:read']));

        expect(words).toContain('ACME_UK');
        expect(words).toContain('Acme UK Limited');
        expect(words).toContain('Corporate');
        expect(words).toContain('Active');
    });

    it('is reached from Home through Reference data', () => {
        const html = list(['refdata::parties:read']);

        expect(html.indexOf('href="/refdata"')).toBeGreaterThan(html.indexOf('href="/"'));
    });

    it('has no Add, because a party is provisioned by a workflow, and lists that workflow', () => {
        const html = list(['refdata::parties:read', 'refdata::parties:write']);
        const words = textOf(html);

        expect(words).not.toMatch(/\bAdd\b/);
        expect(words).toContain('Onboard a party');
    });

    it('pages with the server total', () => {
        expect(textOf(list(['refdata::parties:read']))).toContain('30');
    });
});

describe('a tab of the party page', () => {
    const draw = (tab: string, seen: string[]): string =>
        renderToStaticMarkup(
            <TabPane name="contacts" tab={tab} seen={new Set(seen)}>
                <p>The contacts</p>
            </TabPane>,
        );

    it('is drawn while it is the open tab', () => {
        expect(draw('contacts', [])).not.toContain('hidden');
        expect(draw('contacts', [])).toContain('The contacts');
    });

    it('stays drawn but hidden once the person has opened it and gone to another', () => {
        const html = draw('history', ['contacts', 'history']);

        expect(html).toContain('hidden');
        expect(html).toContain('The contacts');
    });

    it('is not drawn, and so reads nothing, until the person opens it', () => {
        expect(draw('history', ['history'])).toBe('');
    });
});

describe('a party page', () => {
    const open = (
        codes: string[],
        tab = 'details',
        more: (client: QueryClient) => void = () => undefined,
    ): string =>
        page(`/refdata/parties/${ID}?tab=${tab}`, [...codes, ...PART_READS], (client) => {
            more(client);
            client.setQueryData(
                ['records', 'party-identifiers', ID],
                [
                    {
                        version: 3,
                        id: '33333333-3333-4333-8333-333333333333',
                        party_id: ID,
                        id_scheme: 'LEI',
                        id_value: '5493001KJTIIGC8Y1R12',
                        description: '',
                    },
                ],
            );
            client.setQueryData(
                ['records', 'party-contacts', ID],
                [
                    {
                        version: 1,
                        id: '44444444-4444-4444-8444-444444444444',
                        party_id: ID,
                        contact_type: 'Legal',
                        street_line_1: '1 Gresham Street',
                        street_line_2: '',
                        city: 'London',
                        state: '',
                        country_code: 'GB',
                        postal_code: 'EC2V 7HN',
                        phone: '+44 20 7000 0000',
                        email: 'legal@acme.example',
                        web_page: 'https://acme.example',
                        is_primary: true,
                    },
                ],
            );
        });

    it('names the party and offers the aspects as tabs', () => {
        const words = textOf(open(['refdata::parties:read']));

        expect(words).toContain('Acme UK Limited');
        for (const tab of [
            'Details',
            'Identifiers',
            'Contacts',
            'Memberships',
            'Business centres',
            'Counterparties',
            'History',
        ]) {
            expect(words, tab).toContain(tab);
        }
        expect(words).not.toContain('Structure');
    });

    it('draws only the tabs the reader may read, and always History', () => {
        const words = textOf(
            page(`/refdata/parties/${ID}`, ['refdata::parties:read'], () => undefined),
        );

        expect(words).toContain('Details');
        expect(words).toContain('History');
        for (const tab of [
            'Identifiers',
            'Contacts',
            'Memberships',
            'Business centres',
            'Counterparties',
        ]) {
            expect(words, tab).not.toContain(tab);
        }
    });

    it('draws the tab of a part for a reader of that part and for no one else', () => {
        const words = textOf(
            page(
                `/refdata/parties/${ID}`,
                ['refdata::parties:read', 'refdata::party_identifiers:read'],
                () => undefined,
            ),
        );

        expect(words).toContain('Identifiers');
        expect(words).not.toContain('Contacts');
    });

    it('draws the memberships for a reader of either kind of link', () => {
        const words = textOf(
            page(
                `/refdata/parties/${ID}?tab=memberships`,
                ['refdata::parties:read', 'refdata::party_currencies:read'],
                () => undefined,
            ),
        );

        expect(words).toContain('Currencies it operates in');
        expect(words).not.toContain('Countries it operates in');
    });

    it('offers Refresh, because the page reloads only when the person asks', () => {
        const words = textOf(open(['refdata::parties:read']));

        expect(words).toContain('Refresh');
    });

    it('shows a string on every tab and never the key of one that is missing', () => {
        for (const tab of [
            'details',
            'identifiers',
            'contacts',
            'memberships',
            'centres',
            'counterparties',
            'history',
        ]) {
            const words = textOf(open(['refdata::parties:read', 'refdata::parties:write'], tab));

            expect(words.match(/\b(refdata|parties|access)\.[a-zA-Z_.-]+/g) ?? [], tab).toEqual([]);
        }
    });

    it('opens the memberships with one panel for countries and one for currencies', () => {
        const words = textOf(open(['refdata::parties:read'], 'memberships'));

        expect(words.match(/Countries it operates in/g)).toHaveLength(1);
        expect(words.match(/Currencies it operates in/g)).toHaveLength(1);
    });

    it('lists the counterparties it may trade with on their own tab', () => {
        const words = textOf(open(['refdata::parties:read'], 'counterparties'));

        expect(words).toContain('Counterparties it may trade with');
    });

    it('says under a fixed field why it never changes', () => {
        const words = textOf(open(['refdata::parties:read'], 'details'));

        expect(words).toContain('Set when the record was made, and never changed here.');
    });

    it('names a linked counterparty and never draws its identifier', () => {
        const CP = '55555555-5555-4555-8555-555555555555';
        const words = textOf(
            open(['refdata::parties:read'], 'counterparties', (client) => {
                client.setQueryData(
                    ['records', 'party-counterparties', ID],
                    [{ version: 1, party_id: ID, counterparty_id: CP }],
                );
                client.setQueryData(
                    ['records', 'counterparties'],
                    [{ version: 1, id: CP, full_name: 'Globex Corporation' }],
                );
            }),
        );

        expect(words).toContain('Globex Corporation');
        expect(words).not.toContain(CP);
    });

    it('marks a panel read only for a person who may not change its links', () => {
        const words = textOf(open(['refdata::parties:read'], 'counterparties'));

        expect(words).toContain('Read only');
    });

    it('says that a party does not exist and offers the list, and does not leave the page', () => {
        const html = renderRecords(
            '/refdata/parties/missing',
            REGISTRY,
            <Route path="/refdata/parties/:id" element={<PartyPage />} />,
            (client) => {
                client.setQueryData(['my-access'], accessOf(['refdata::parties:read']));
                // An errored read is tried again when its screen mounts, which would
                // hide the error from this render.
                client.setQueryDefaults(['records', 'parties', 'key', 'missing'], {
                    retryOnMount: false,
                });
                client
                    .getQueryCache()
                    .build(client, { queryKey: ['records', 'parties', 'key', 'missing'] })
                    .setState({
                        status: 'error',
                        fetchStatus: 'idle',
                        error: new ApiFailure(404, { code: 'not_found', message: 'gone' } as never),
                        errorUpdateCount: 1,
                        errorUpdatedAt: Date.now(),
                    });
            },
        );

        expect(textOf(html)).toContain('There is no such record, or you may not see it.');
        expect(html).toContain('href="/refdata/parties"');
    });

    it('calls the picture of a party its logo', () => {
        const words = textOf(open(['refdata::parties:read'], 'details'));

        expect(words).toContain('Logo');
        expect(words).not.toContain('Flag');
    });

    describe('on the History tab', () => {
        const story = {
            subject: 'party',
            id: ID,
            events: [
                {
                    entityType: 'ores.refdata.party',
                    entityId: ID,
                    kind: 'changed',
                    at: '2026-10-05 10:00:00Z',
                    actor: 'priya',
                    version: 7,
                    reasonCode: 'common.rectification',
                    commentary: 'Fixed the name',
                    fields: [
                        { name: 'Short Code', value: 'ACME_UK' },
                        { name: 'Full Name', value: 'Acme UK Limited' },
                    ],
                },
                {
                    entityType: 'ores.refdata.party_currency',
                    entityId: '44444444-4444-4444-8444-444444444444/GBP',
                    kind: 'closed',
                    at: '2026-10-04 12:00:00Z',
                    actor: '',
                    version: 1,
                    reasonCode: '',
                    commentary: '',
                    fields: [
                        { name: 'ID', value: '44444444-4444-4444-8444-444444444444/GBP' },
                        { name: 'Currency', value: 'GBP' },
                    ],
                },
                {
                    entityType: 'ores.refdata.party_currency',
                    entityId: '44444444-4444-4444-8444-444444444444/GBP',
                    kind: 'raised',
                    at: '2026-10-04 11:00:00Z',
                    actor: 'priya',
                    version: 1,
                    reasonCode: 'system.new_record',
                    commentary: '',
                    fields: [
                        { name: 'ID', value: '44444444-4444-4444-8444-444444444444/GBP' },
                        { name: 'Currency', value: 'GBP' },
                    ],
                },
                {
                    entityType: 'ores.refdata.party_identifier',
                    entityId: '33333333-3333-4333-8333-333333333333',
                    kind: 'raised',
                    at: '2026-10-04 10:00:00Z',
                    actor: 'ores_refdata_service',
                    version: 1,
                    reasonCode: 'system.new_record',
                    commentary: '',
                    fields: [{ name: 'ID Value', value: '5493001KJTIIGC8Y1R12' }],
                },
                {
                    entityType: 'ores.refdata.party',
                    entityId: ID,
                    kind: 'raised',
                    at: '2026-10-03 10:00:00Z',
                    actor: 'ores_refdata_service',
                    version: 6,
                    reasonCode: 'system.new_record',
                    commentary: '',
                    fields: [
                        { name: 'Short Code', value: 'ACME_UK' },
                        { name: 'Full Name', value: 'Acme UK' },
                    ],
                },
            ],
            gaps: [
                {
                    entity: 'ores.refdata.party_country',
                    reason: 'The countries it works in could not be read.',
                },
                {
                    entity: 'ores.refdata.party_identifier',
                    reason: 'The server refused this caller the identifier read.',
                },
            ],
        };
        const history = (codes: string[]): string =>
            page(`/refdata/parties/${ID}?tab=history`, codes, (client) =>
                client.setQueryData(['timeline', 'party', ID], story),
            );

        it('draws one rail for the party and its identifiers, by name, with the author', () => {
            const words = textOf(history(['refdata::parties:read']));

            expect(words).toContain('Party ACME_UK');
            expect(words).toContain('Identifier 5493001KJTIIGC8Y1R12');
            expect(words).toContain('priya');
            expect(words).toContain('Fixed the name');
        });

        it('draws a link the party held by its currency, with its opening and its closing', () => {
            const words = textOf(history(['refdata::parties:read']));

            expect(words).toContain('Currency GBP');
            expect(words).toContain('Closed');
            expect(words).toContain('Created');
        });

        it('states what the history cannot show, in words', () => {
            const words = textOf(history(['refdata::parties:read']));

            expect(words).toContain('What this history cannot show');
            expect(words).toContain(
                'The countries it works in could not be read, so adding or removing one is not shown.',
            );
            expect(words).toContain('Part of this history could not be read, so it is not shown.');
            expect(words).not.toContain('junction');
            expect(words).toContain(
                'party_identifier: The server refused this caller the identifier read.',
            );
        });

        it('offers to write an older version of the party back, and not the newest', () => {
            const words = textOf(history(['refdata::parties:read', 'refdata::parties:write']));

            expect(words).toContain('Revert to v6');
            expect(words).not.toContain('Revert to v7');
        });

        it('offers no revert to a person who may not write the party', () => {
            expect(textOf(history(['refdata::parties:read']))).not.toContain('Revert');
        });
    });

    it('is reached from Home through Reference data and the list', () => {
        const html = open(['refdata::parties:read']);

        expect(html.indexOf('href="/refdata"')).toBeGreaterThan(html.indexOf('href="/"'));
        expect(html).toContain('href="/refdata/parties"');
    });

    it('shows the parent by name, as a link to its page, and never an identifier', () => {
        const html = open(['refdata::parties:read']);

        expect(textOf(html)).toContain('Acme Group');
        expect(html).toContain(`href="/refdata/parties/${PARENT}"`);
        expect(textOf(html)).not.toContain(PARENT);
    });

    it('offers Edit on the card to a person who may write, and no Delete', () => {
        const words = textOf(open(['refdata::parties:read', 'refdata::parties:write']));

        expect(words).toContain('Edit');
        expect(words).not.toContain('Delete');
    });

    it('marks the card read only for a person who may not write', () => {
        const words = textOf(open(['refdata::parties:read']));

        expect(words).toContain('Read only');
        expect(words).not.toContain('Edit');
    });

    it('lists the identifiers with their scheme and value', () => {
        const words = textOf(open(['refdata::parties:read'], 'identifiers'));

        expect(words).toContain('LEI');
        expect(words).toContain('5493001KJTIIGC8Y1R12');
    });

    it('draws an email as a mail link and a web address as a link that leaves the page', () => {
        const html = open(['refdata::parties:read'], 'contacts');

        expect(html).toContain('href="mailto:legal@acme.example"');
        expect(html).toContain('href="https://acme.example/"');
        expect(html).toContain('rel="noopener noreferrer"');
    });

    it('offers Add and Edit on the identifiers to a person who may write', () => {
        const words = textOf(
            open(['refdata::parties:read', 'refdata::party_identifiers:write'], 'identifiers'),
        );

        expect(words).toContain('Add');
        expect(words).toContain('Edit');
    });
});
