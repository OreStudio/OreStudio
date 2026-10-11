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

    function caller(replies: Record<string, unknown>): AuthenticatedCaller {
        return {
            async callAuthenticated(
                subject: string,
                _body: unknown,
                schema: { parse: (value: unknown) => unknown },
            ): Promise<never> {
                if (!(subject in replies)) throw new Error(`No canned reply for ${subject}`);
                return schema.parse(replies[subject]) as never;
            },
        } as unknown as AuthenticatedCaller;
    }

    const base = {
        'refdata.v1.parties.list': {
            result: ok,
            parties: [{ version: 3, id: PARTY, short_code: 'ACME_UK' }],
            total: 1,
        },
        'refdata.v1.history.get': {
            success: true,
            message: '',
            versions: [
                version(1, PARTY, { 'Short Code': 'ACME_UK', 'Full Name': 'Acme UK' }),
                version(2, PARTY, { 'Short Code': 'ACME_UK', 'Full Name': 'Acme UK Ltd' }),
            ],
        },
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

    it('says what the junctions cannot answer, and never leaves them out as empty', async () => {
        const story = await readPartyTimeline(caller(base), PARTY);

        expect(story.gaps.map((gap) => gap.entity)).toEqual(
            expect.arrayContaining([
                'ores.refdata.party_country',
                'ores.refdata.party_currency',
                'ores.refdata.party_counterparty',
            ]),
        );
    });

    it('keeps only the versions that belong to the party asked for', async () => {
        const story = await readPartyTimeline(
            caller({
                ...base,
                'refdata.v1.history.get': {
                    success: true,
                    message: '',
                    versions: [
                        version(1, PARTY, { 'Short Code': 'ACME_UK' }),
                        version(2, OTHER, { 'Short Code': 'ACME_UK' }),
                    ],
                },
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
