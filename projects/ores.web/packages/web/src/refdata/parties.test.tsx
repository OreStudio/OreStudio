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
import { textOf } from '../textOf.js';
import { PartiesPage, PartyPage } from './parties.js';
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
    resourceView('party-contacts', ['id']),
    resourceView('party-countries', ['party_id', 'country_alpha2_code'], { versioned: false }),
    resourceView('party-currencies', ['party_id', 'currency_iso_code'], { versioned: false }),
    resourceView('party-counterparties', ['party_id', 'counterparty_id'], { versioned: false }),
    resourceView('business-units', ['id']),
];

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

describe('a party page', () => {
    const open = (codes: string[], tab = 'details'): string =>
        page(`/refdata/parties/${ID}?tab=${tab}`, codes, (client) => {
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

    it('calls the picture of a party its logo', () => {
        const words = textOf(open(['refdata::parties:read'], 'details'));

        expect(words).toContain('Logo');
        expect(words).not.toContain('Flag');
    });

    it('asks for the versions of a party under its short code', () => {
        const words = textOf(
            page(`/refdata/parties/${ID}?tab=history`, ['refdata::parties:read'], (client) =>
                client.setQueryData(
                    ['history', 'ores.refdata.party', 'ACME_UK'],
                    [
                        {
                            version: 7,
                            modifiedBy: 'priya',
                            recordedAt: '2026-10-04 22:10:00Z',
                            fields: [{ name: 'Full Name', value: 'Acme UK Limited' }],
                        },
                    ],
                ),
            ),
        );

        expect(words).toContain('v7');
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
