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
import type { AuthenticatedCaller } from './account-operations.js';
import { orderTimeline, readPartyTimeline, type TimelineEvent } from './timeline.js';

function event(over: Partial<TimelineEvent>): TimelineEvent {
    return {
        entityType: 'ores.iam.account',
        entityId: 'a',
        kind: 'changed',
        at: '2026-10-04 10:00:00Z',
        actor: '',
        version: 1,
        reasonCode: '',
        commentary: '',
        fields: [],
        ...over,
    };
}

/**
 * The order of a stream is the whole of what it says, so it is asserted
 * directly rather than through a screen: two entries in the wrong order read as
 * a different story.
 */
describe('the order of a timeline', () => {
    it('reads newest first', () => {
        const older = event({ at: '2026-10-01 09:00:00Z', version: 1 });
        const newer = event({ at: '2026-10-02 09:00:00Z', version: 2 });

        expect(orderTimeline([older, newer])).toEqual([newer, older]);
    });

    it('reads an act that lands later in the same second first', () => {
        const raised = event({ kind: 'raised', version: 1 });
        const granted = event({ kind: 'granted', version: 2 });

        expect(orderTimeline([granted, raised])).toEqual([granted, raised]);
        expect(orderTimeline([raised, granted])).toEqual([granted, raised]);
    });

    it('reads the sign-in above the change it was made under', () => {
        const signIn = event({
            kind: 'signed_in',
            entityType: 'ores.iam.auth_event',
            entityId: 'e',
        });
        const change = event({ kind: 'changed' });

        expect(orderTimeline([change, signIn])).toEqual([signIn, change]);
    });

    it('reads the higher version of one row first when a second holds both', () => {
        const first = event({ version: 1 });
        const second = event({ version: 2 });

        expect(orderTimeline([first, second])).toEqual([second, first]);
    });
});

describe('the story of a party', () => {
    /** The parts whose removal no read can find, so the story always says so. */
    const REMOVED_PARTS = [
        'ores.refdata.party_identifier_removed',
        'ores.refdata.party_contact_information_removed',
        'ores.refdata.business_unit_removed',
    ];
    const PARTY = '11111111-1111-4111-8111-111111111111';
    const OTHER = '22222222-2222-4222-8222-222222222222';
    const ok = { outcome: 'ok', code: '', message: '' };

    function version(
        number: number,
        id: string,
        fields: Record<string, string>,
        modifiedBy = 'priya',
    ) {
        return {
            version: number,
            modified_by: modifiedBy,
            recorded_at: `2026-10-0${String(number)} 10:00:00Z`,
            fields: [
                { name: 'ID', value: id },
                ...Object.entries(fields).map(([name, value]) => ({ name, value })),
                {
                    name: 'Change Reason Code',
                    value: number === 1 ? 'system.new_record' : 'common.rectification',
                },
            ],
            changes: { entries: [] },
        };
    }

    /** A reply, or a function of the request when the same subject answers by entity type. */
    type Reply = unknown | ((body: { entity_type?: string }) => unknown);

    function caller(replies: Record<string, Reply>): AuthenticatedCaller {
        return {
            async callAuthenticated(
                subject: string,
                body: { entity_type?: string },
                schema: { parse: (value: unknown) => unknown },
            ): Promise<never> {
                if (!(subject in replies)) throw new Error(`No canned reply for ${subject}`);
                const reply = replies[subject];
                return schema.parse(typeof reply === 'function' ? reply(body) : reply) as never;
            },
        } as unknown as AuthenticatedCaller;
    }

    /** A history answer: the party's own versions, and the link versions by entity type. */
    function history(
        partyVersions: readonly unknown[],
        links: Record<string, readonly unknown[]> = {},
    ) {
        return (body: { entity_type?: string }) => ({
            success: true,
            message: '',
            versions:
                body.entity_type === 'ores.refdata.party'
                    ? partyVersions
                    : (links[body.entity_type ?? ''] ?? []),
        });
    }

    const base = {
        'refdata.v1.parties.list': {
            result: ok,
            parties: [{ version: 3, id: PARTY, short_code: 'ACME_UK' }],
            total: 1,
        },
        'refdata.v1.history.get': history([
            version(1, PARTY, { 'Short Code': 'ACME_UK', 'Full Name': 'Acme UK' }),
            version(2, PARTY, { 'Short Code': 'ACME_UK', 'Full Name': 'Acme UK Ltd' }),
        ]),
        'refdata.v1.party_identifiers.list_by_party_id': {
            result: ok,
            party_identifiers: [],
        },
        'refdata.v1.party_contact_informations.list_by_party_id': {
            result: ok,
            party_contact_informations: [],
        },
        'refdata.v1.business_units.list_by_party_id': { result: ok, business_units: [] },
    };

    it('is the versions of the party, newest first, as raised and changed', async () => {
        const story = await readPartyTimeline(caller(base), PARTY);

        expect(story.subject).toBe('party');
        expect(story.events.map((e) => [e.kind, e.version])).toEqual([
            ['changed', 2],
            ['raised', 1],
        ]);
        expect(story.events[0]?.actor).toBe('priya');
    });

    it('reads the history of a contact by the contact’s own id and states no gap', async () => {
        const CONTACT = '99999999-9999-4999-8999-999999999999';
        const asked: string[] = [];
        const story = await readPartyTimeline(
            caller({
                ...base,
                'refdata.v1.party_contact_informations.list_by_party_id': {
                    result: ok,
                    party_contact_informations: [
                        { version: 1, id: CONTACT, party_id: PARTY, contact_type: 'Legal' },
                    ],
                },
                'refdata.v1.history.get': (body: { entity_type?: string; entity_id?: string }) => {
                    if (body.entity_type === 'ores.refdata.party_contact_information') {
                        asked.push(body.entity_id ?? '');
                    }
                    return history([version(1, PARTY, { 'Short Code': 'ACME_UK' })])(body);
                },
            }),
            PARTY,
        );

        expect(asked).toEqual([CONTACT]);
        expect(story.gaps.map((g) => g.entity)).toEqual(REMOVED_PARTS);
    });

    /** One version of a link, which names the pair as its ID and says when it ended. */
    function linkVersion(number: number, link: string, closedAt = '', closedBy = '') {
        const [, country = ''] = link.split('/');
        return {
            version: number,
            modified_by: 'priya',
            recorded_at: `2026-10-0${String(number)} 10:00:00.000000Z`,
            fields: [
                { name: 'ID', value: link },
                { name: 'Party', value: PARTY },
                { name: 'Country', value: country },
                { name: 'Closed At', value: closedAt },
                { name: 'Closed By', value: closedBy },
                { name: 'Change Reason Code', value: 'system.new_record' },
            ],
            changes: { entries: [] },
        };
    }

    it('draws the opening, the change and the closing of a link the party held', async () => {
        const link = `${PARTY}/GB`;
        const story = await readPartyTimeline(
            caller({
                ...base,
                'refdata.v1.history.get': history(
                    [version(1, PARTY, { 'Short Code': 'ACME_UK' })],
                    {
                        'ores.refdata.party_country': [
                            linkVersion(1, link),
                            linkVersion(2, link, '2026-10-05T10:00:00Z', 'priya'),
                        ],
                    },
                ),
            }),
            PARTY,
        );

        const own = story.events.filter((e) => e.entityType === 'ores.refdata.party_country');
        expect(own.map((e) => [e.kind, e.version, e.entityId])).toEqual([
            ['closed', 2, link],
            ['changed', 2, link],
            ['raised', 1, link],
        ]);
        // The store records when a link ended and who ended it.
        expect(own[0]?.actor).toBe('priya');
        expect(own[0]?.at).toBe('2026-10-05T10:00:00Z');
        expect(own[0]?.fields.map((f) => f.name)).toEqual(['ID', 'Party', 'Country']);
    });

    it('keeps two links of one party apart, and states no gap for a link read that worked', async () => {
        const story = await readPartyTimeline(
            caller({
                ...base,
                'refdata.v1.history.get': history(
                    [version(1, PARTY, { 'Short Code': 'ACME_UK' })],
                    {
                        'ores.refdata.party_country': [
                            linkVersion(1, `${PARTY}/GB`),
                            linkVersion(1, `${PARTY}/FR`),
                        ],
                    },
                ),
            }),
            PARTY,
        );

        const ids = story.events
            .filter((e) => e.entityType === 'ores.refdata.party_country')
            .map((e) => e.entityId)
            .sort();
        expect(ids).toEqual([`${PARTY}/FR`, `${PARTY}/GB`]);
        expect(story.gaps.map((g) => g.entity)).not.toContain('ores.refdata.party_country');
    });

    it('says the counterparty of a link by its name and not by its id', async () => {
        const counterparty = '55555555-5555-4555-8555-555555555555';
        const link = `${PARTY}/${counterparty}`;
        const story = await readPartyTimeline(
            caller({
                ...base,
                'refdata.v1.counterparties.list': {
                    result: ok,
                    counterparties: [{ id: counterparty, full_name: 'Globex Corporation' }],
                    total: 1,
                },
                'refdata.v1.history.get': history(
                    [version(1, PARTY, { 'Short Code': 'ACME_UK' })],
                    {
                        'ores.refdata.party_counterparty': [
                            {
                                ...linkVersion(1, link),
                                fields: [
                                    { name: 'ID', value: link },
                                    { name: 'Party', value: PARTY },
                                    { name: 'Counterparty', value: counterparty },
                                    { name: 'Closed At', value: '' },
                                ],
                            },
                        ],
                    },
                ),
            }),
            PARTY,
        );

        const own = story.events.find((e) => e.entityType === 'ores.refdata.party_counterparty');
        expect(own?.fields.find((f) => f.name === 'Counterparty')?.value).toBe(
            'Globex Corporation',
        );
    });

    it('states a gap for a link kind whose history the server refuses', async () => {
        const story = await readPartyTimeline(
            caller({
                ...base,
                'refdata.v1.history.get': (body: { entity_type?: string }) =>
                    body.entity_type === 'ores.refdata.party_currency'
                        ? { success: false, message: 'refused', versions: [] }
                        : { success: true, message: '', versions: [] },
            }),
            PARTY,
        );

        expect(story.gaps.map((g) => g.entity)).toEqual([
            'ores.refdata.party_currency',
            ...REMOVED_PARTS,
        ]);
    });

    it('keeps only the versions that belong to the party asked for', async () => {
        const story = await readPartyTimeline(
            caller({
                ...base,
                'refdata.v1.history.get': history([
                    version(1, PARTY, { 'Short Code': 'ACME_UK' }),
                    version(2, OTHER, { 'Short Code': 'ACME_UK' }),
                ]),
            }),
            PARTY,
        );

        expect(story.events.map((e) => e.entityId)).toEqual([PARTY]);
    });

    it('answers an empty story for a party nothing holds', async () => {
        const story = await readPartyTimeline(
            caller({ ...base, 'refdata.v1.parties.list': { result: ok, parties: [], total: 0 } }),
            PARTY,
        );

        expect(story.events).toEqual([]);
    });
});
