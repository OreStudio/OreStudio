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
import { readTenantParties } from './tenant-parties.js';
import { readTenant, readTenantSetups } from './tenants.js';

/**
 * The reads behind one tenant's screen, against a caller that records what it
 * was sent and answers what it is given.
 */

const ACME = '44444444-4444-4444-4444-444444444444';
const SYSTEM_PARTY = '11111111-1111-1111-1111-111111111111';
const GROUP = '22222222-2222-2222-2222-222222222222';
const LONDON = '33333333-3333-3333-3333-333333333333';

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
        await readTenantSetups(callerAnswering({ success: true, instances: [] }, sent), ACME);

        expect(sent[0]?.body).toMatchObject({ target_id_filter: ACME });
    });
});

describe('the party read for one tenant', () => {
    it('names the tenant, lists the system party first and resolves each parent', async () => {
        const sent: Recorded[] = [];
        const read = await readTenantParties(
            callerAnswering(
                {
                    success: true,
                    parties: [
                        party(LONDON, 'acme_london', 'Acme London', 'Operational', GROUP),
                        party(GROUP, 'acme_group', 'Acme Group', 'Operational', SYSTEM_PARTY),
                        party(SYSTEM_PARTY, 'system', 'Acme System', 'System', null),
                    ],
                    total: 3,
                },
                sent,
            ),
            ACME,
        );

        expect(sent[0]?.subject).toBe('refdata.v1.parties.list-of-tenant');
        expect(sent[0]?.body).toMatchObject({ tenant_id: ACME });
        expect(read.parties.map((p) => p.code)).toEqual(['system', 'acme_group', 'acme_london']);
        expect(read.parties[2]?.parentName).toBe('Acme Group');
        expect(read.parties[0]?.parentName).toBeNull();
        expect(read.total).toBe(3);
    });

    it('fails with the server words when the read is refused', async () => {
        await expect(
            readTenantParties(
                callerAnswering({ success: false, message: 'Refused.', parties: [] }),
                ACME,
            ),
        ).rejects.toThrow('Refused.');
    });
});
