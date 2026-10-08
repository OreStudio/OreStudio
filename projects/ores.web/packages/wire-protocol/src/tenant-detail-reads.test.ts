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
import { readPartiesPage } from './party-page.js';
import { readTenant, readTenantSetups } from './tenants.js';

/**
 * The reads behind one tenant's screen, against a caller that records what it
 * was sent and answers what it is given.
 */

const ACME = '44444444-4444-4444-4444-444444444444';
const SYSTEM_PARTY = '11111111-1111-1111-1111-111111111111';
const GROUP = '22222222-2222-2222-2222-222222222222';
const LONDON = '33333333-3333-3333-3333-333333333333';
const PARIS = '66666666-6666-6666-6666-666666666666';

interface Recorded {
    subject: string;
    body: unknown;
}

function callerAnswering(answer: unknown, sent: Recorded[] = []): AuthenticatedCaller {
    return {
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            sent.push({ subject, body });
            return schema.parse(answer);
        },
    } as unknown as AuthenticatedCaller;
}

/** A caller that answers each call with the next answer in turn. */
function callerAnsweringInTurn(answers: unknown[], sent: Recorded[] = []): AuthenticatedCaller {
    let next = 0;
    return {
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            sent.push({ subject, body });
            return schema.parse(answers[next++]);
        },
    } as unknown as AuthenticatedCaller;
}

function party(id: string, code: string, name: string, category: string, parent: string | null) {
    return {
        id,
        short_code: code,
        full_name: name,
        party_category: category,
        party_type: 'Corporate',
        status: 'Active',
        parent_party_id: parent,
    };
}

const acmeRow = {
    version: 3,
    tenant_id: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
    id: ACME,
    code: 'acme',
    name: 'Acme Corporation',
    type: 'evaluation',
    description: '',
    hostname: 'acme',
    status: 'active',
    is_registration_default: false,
    modified_by: 'admin',
    performed_by: 'ores.iam.service',
    change_reason_code: 'system.initial_load',
    change_commentary: '',
    recorded_at: '2026-10-02 21:39:00Z',
};

describe('the tenant read', () => {
    it('asks for the tenant by its code and keeps the provenance', async () => {
        const sent: Recorded[] = [];
        const tenant = await readTenant(
            callerAnswering({ result: { outcome: 'ok' }, tenant: acmeRow }, sent),
            'acme',
        );

        expect(sent[0]?.subject).toBe('iam.v1.tenants.get');
        expect(sent[0]?.body).toEqual({ key: { code: 'acme' } });
        expect(tenant?.version).toBe(3);
        expect(tenant?.changeReasonCode).toBe('system.initial_load');
    });

    it('answers null for a code no tenant holds', async () => {
        const tenant = await readTenant(
            callerAnswering({ result: { outcome: 'missing' }, tenant: null }),
            'nobody',
        );

        expect(tenant).toBeNull();
    });
});

describe('the run read for one tenant', () => {
    it('filters the runs on the tenant as target', async () => {
        const sent: Recorded[] = [];
        await readTenantSetups(
            callerAnswering({ success: true, instances: [] }, sent),
            [ACME],
            async () => 0,
        );

        expect(sent[0]?.body).toMatchObject({ target_id_filter: '', target_ids_filter: [ACME] });
    });
});

/*
 * The parties are the session's own tenant's, so the read names no tenant:
 * row-level security scopes it from the token. Each read is one page.
 */
describe('the party page', () => {
    function reply(parties: unknown[], total: number) {
        return { result: { outcome: 'ok' }, parties, total };
    }

    it('reads one page with the server total and names parents on the page', async () => {
        const sent: Recorded[] = [];
        const page = await readPartiesPage(
            callerAnswering(
                reply(
                    [
                        party(SYSTEM_PARTY, 'system', 'Acme System', 'System', null),
                        party(GROUP, 'acme_group', 'Acme Group', 'Operational', SYSTEM_PARTY),
                    ],
                    40,
                ),
                sent,
            ),
            { offset: 20, limit: 2 },
        );

        expect(sent).toHaveLength(1);
        expect(sent[0]?.subject).toBe('refdata.v1.parties.list');
        expect(sent[0]?.body).toEqual({
            offset: 20,
            limit: 2,
            order: { field: '', descending: false },
            as_of: null,
            filter: null,
        });
        expect(page.totalCount).toBe(40);
        expect(page.parties.map((p) => p.parentName)).toEqual([null, 'Acme System']);
    });

    /*
     * The search and the order are the caller's, and reach the server as the
     * filter and the order of the page request.
     */
    it('sends the search and the order the caller asks for', async () => {
        const sent: Recorded[] = [];
        await readPartiesPage(callerAnswering(reply([], 0), sent), {
            offset: 0,
            limit: 20,
            search: 'acme',
            sort: 'full_name',
            descending: true,
        });

        expect(sent[0]?.body).toEqual({
            offset: 0,
            limit: 20,
            order: { field: 'full_name', descending: true },
            as_of: null,
            filter: { id_one_of: null, search: 'acme' },
        });
    });

    /*
     * A parent on another page is read by its id, once for every such parent,
     * so a page costs two reads at most however its parents fall.
     */
    it('reads the parents on other pages in one read by id', async () => {
        const sent: Recorded[] = [];
        const page = await readPartiesPage(
            callerAnsweringInTurn(
                [
                    reply(
                        [
                            party(GROUP, 'acme_group', 'Acme Group', 'Operational', SYSTEM_PARTY),
                            party(LONDON, 'acme_london', 'Acme London', 'Operational', ACME),
                            party(PARIS, 'acme_paris', 'Acme Paris', 'Operational', ACME),
                        ],
                        40,
                    ),
                    reply([party(ACME, 'acme', 'Acme Corporation', 'Operational', null)], 1),
                ],
                sent,
            ),
            { offset: 20, limit: 3 },
        );

        expect(sent).toHaveLength(2);
        expect(sent[1]?.body).toEqual({
            offset: 0,
            limit: 2,
            order: { field: '', descending: false },
            as_of: null,
            filter: { id_one_of: [SYSTEM_PARTY, ACME], search: null },
        });
        // The system party is not visible to this session, so it is not answered.
        expect(page.parties.map((p) => p.parentName)).toEqual([
            null,
            'Acme Corporation',
            'Acme Corporation',
        ]);
        expect(page.parties[0]?.parentId).toBe(SYSTEM_PARTY);
    });

    /*
     * A centre has no flag of its own: its country has. The page's centres are
     * read once by code and their countries once by code, so the flags cost
     * two reads however many parties share a centre.
     */
    it('carries the flag of each party business centre country', async () => {
        const GB_FLAG = '99999999-9999-9999-9999-999999999991';
        const sent: Recorded[] = [];
        const page = await readPartiesPage(
            callerAnsweringInTurn(
                [
                    reply(
                        [
                            {
                                ...party(ACME, 'acme', 'Acme', 'Operational', null),
                                business_center_code: 'GBLO',
                            },
                            {
                                ...party(LONDON, 'acme_london', 'Acme London', 'Operational', ACME),
                                business_center_code: 'GBLO',
                            },
                            {
                                ...party(PARIS, 'acme_paris', 'Acme Paris', 'Operational', ACME),
                                business_center_code: 'FRPA',
                            },
                            party(GROUP, 'acme_group', 'Acme Group', 'Operational', ACME),
                        ],
                        4,
                    ),
                    {
                        result: { outcome: 'ok' },
                        centres: [
                            { code: 'GBLO', country_alpha2_code: 'GB' },
                            { code: 'FRPA', country_alpha2_code: 'FR' },
                        ],
                    },
                    {
                        result: { outcome: 'ok' },
                        countries: [
                            { alpha2_code: 'GB', image_id: GB_FLAG },
                            { alpha2_code: 'FR', image_id: null },
                        ],
                    },
                ],
                sent,
            ),
            { offset: 0, limit: 4 },
        );

        expect(sent.map((r) => r.subject)).toEqual([
            'refdata.v1.parties.list',
            'refdata.v1.business_centres.list',
            'refdata.v1.countries.list',
        ]);
        expect(sent[1]?.body).toMatchObject({ filter: { code_one_of: ['GBLO', 'FRPA'] } });
        expect(sent[2]?.body).toMatchObject({ filter: { alpha2_code_one_of: ['GB', 'FR'] } });
        expect(page.parties.map((p) => [p.businessCentreCode, p.flagImageId])).toEqual([
            ['GBLO', GB_FLAG],
            ['GBLO', GB_FLAG],
            ['FRPA', null],
            ['', null],
        ]);
    });

    it('answers the page without flags when the centres cannot be read', async () => {
        const page = await readPartiesPage(
            callerAnsweringInTurn([
                reply(
                    [
                        {
                            ...party(ACME, 'acme', 'Acme', 'Operational', null),
                            business_center_code: 'GBLO',
                        },
                    ],
                    1,
                ),
                { result: { outcome: 'denied', message: 'Refused.' }, centres: [] },
            ]),
            { offset: 0, limit: 1 },
        );

        expect(page.parties.map((p) => [p.name, p.businessCentreCode, p.flagImageId])).toEqual([
            ['Acme', 'GBLO', null],
        ]);
    });

    it('fails with the server words when the read is refused', async () => {
        await expect(
            readPartiesPage(
                callerAnswering({
                    result: { outcome: 'denied', message: 'Refused.' },
                    parties: [],
                    total: 0,
                }),
                { offset: 0, limit: 20 },
            ),
        ).rejects.toThrow('Refused.');
    });
});
