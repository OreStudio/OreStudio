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
import { OresClient } from './client.js';
import { WireCodec } from './codec.js';
import type { Reply, RequestHeaders, Transport } from './transport.js';

interface RecordedCall {
    readonly subject: string;
    readonly body: Uint8Array;
}

/** A transport that answers from a script and records what it received. */
class ScriptedTransport implements Transport {
    readonly calls: RecordedCall[] = [];
    readonly #script: Map<string, unknown[]>;
    readonly #codec = new WireCodec('msgpack');

    constructor(script: Record<string, unknown[]>) {
        this.#script = new Map(Object.entries(script));
    }

    async request(
        subject: string,
        body: Uint8Array,
        _headers: RequestHeaders,
        _timeoutMs: number,
    ): Promise<Reply> {
        this.calls.push({ subject, body });
        const queue = this.#script.get(subject);
        const next = queue?.shift();
        if (next === undefined) {
            throw new Error(`no scripted reply left for ${subject}`);
        }
        return { subject, body: this.#codec.encode(next), headers: {} };
    }

    async close(): Promise<void> {
        return Promise.resolve();
    }

    decodeCall(index: number): unknown {
        const call = this.calls[index];
        if (call === undefined) {
            throw new Error(`no call at index ${index}`);
        }
        return this.#codec.decode(call.body);
    }
}

function loginReply(): Record<string, unknown> {
    return {
        success: true,
        account_id: '11111111-1111-1111-1111-111111111111',
        tenant_id: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
        tenant_name: 'System',
        username: 'probe',
        email: 'probe@ores.web.test',
        password_reset_required: false,
        tenant_bootstrap_mode: false,
        party_setup_required: false,
        party_setup_warning: '',
        token: 'token-one',
        error_message: '',
        message: '',
        selected_party_id: '22222222-2222-2222-2222-222222222222',
        available_parties: [],
        default_party_id: '',
        access_lifetime_s: 1800,
        session_id: '33333333-3333-3333-3333-333333333333',
    };
}

function partyRow(shortCode: string, parentPartyId: string | null): Record<string, unknown> {
    return {
        id: '0d0f9f24-6f3a-4c2b-9c2e-1a2b3c4d5e6f',
        short_code: shortCode,
        full_name: 'BARCLAYS PLC',
        party_category: 'Operational',
        party_type: 'Corporate',
        parent_party_id: parentPartyId,
        business_center_code: 'WRLD',
        status: 'Active',
    };
}

describe('OresClient list parties', () => {
    it('reads every page the tenant holds and carries the parent through', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.auth.login': [loginReply()],
            'refdata.v1.parties.list': [
                {
                    result: { outcome: 'ok', code: '', message: '' },
                    parties: [partyRow('BRCLYS', null)],
                    total: 2,
                },
                {
                    result: { outcome: 'ok', code: '', message: '' },
                    parties: [partyRow('BABAUK', '0d0f9f24-6f3a-4c2b-9c2e-1a2b3c4d5e6f')],
                    total: 2,
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        const parties = await client.listParties();

        expect(parties.map((party) => party.short_code)).toEqual(['BRCLYS', 'BABAUK']);
        expect(parties[0]?.parent_party_id).toBeNull();
        expect(parties[1]?.parent_party_id).toBe('0d0f9f24-6f3a-4c2b-9c2e-1a2b3c4d5e6f');
        expect(transport.decodeCall(1)).toEqual({
            offset: 0,
            limit: 500,
            order: { field: '', descending: false },
        });
    });

    it('stops at one page when the tenant holds no more', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.auth.login': [loginReply()],
            'refdata.v1.parties.list': [
                {
                    result: { outcome: 'ok', code: '', message: '' },
                    parties: [partyRow('BRCLYS', null)],
                    total: 1,
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await client.listParties();

        expect(transport.calls).toHaveLength(2);
    });

    it('reports a refused read rather than answering with no parties', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.auth.login': [loginReply()],
            'refdata.v1.parties.list': [
                { result: { outcome: 'denied', code: 'forbidden', message: 'Not permitted' } },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await expect(client.listParties()).rejects.toThrow('Not permitted');
    });
});

describe('OresClient create party', () => {
    const created = {
        result: { outcome: 'ok', code: '', message: '' },
        party: partyRow('NWCAP', null),
    };

    it('writes a party that exists only once, born inactive', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.auth.login': [loginReply()],
            'refdata.v1.parties.put': [created],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        const result = await client.createParty({
            shortCode: 'NWCAP',
            fullName: 'Northwind Capital',
            parentPartyId: null,
        });

        expect(result.success).toBe(true);
        expect(result.partyId).toMatch(/^[0-9a-f-]{36}$/);
        const write = transport.decodeCall(1) as {
            change: { write: Record<string, unknown>; precondition: Record<string, unknown> };
            intent: Record<string, unknown>;
        };
        expect(write.change.write).toEqual({
            id: result.partyId,
            short_code: 'NWCAP',
            full_name: 'Northwind Capital',
            codename: '',
            transliterated_name: null,
            party_category: 'Operational',
            party_type: 'Corporate',
            parent_party_id: null,
            business_center_code: 'WRLD',
            status: 'Inactive',
            image_id: null,
            is_registration_default: false,
        });
        expect(write.change.precondition).toEqual({ kind: 'must_not_exist', version: null });
        // The reason is left to the server, which states the new-record one.
        expect(write.intent).toEqual({ reason_code: '', commentary: '' });
    });

    it('hangs the party under the one it was given', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.auth.login': [loginReply()],
            'refdata.v1.parties.put': [created],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await client.createParty({
            shortCode: 'NWCAP',
            fullName: 'Northwind Capital',
            parentPartyId: '0d0f9f24-6f3a-4c2b-9c2e-1a2b3c4d5e6f',
        });

        const write = transport.decodeCall(1) as {
            change: { write: { parent_party_id: string | null } };
        };
        expect(write.change.write.parent_party_id).toBe('0d0f9f24-6f3a-4c2b-9c2e-1a2b3c4d5e6f');
    });

    it('answers with the refusal the server stated rather than throwing', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.auth.login': [loginReply()],
            'refdata.v1.parties.put': [
                {
                    result: {
                        outcome: 'conflict',
                        code: 'already_exists',
                        message: 'A party with this code already exists',
                    },
                    party: null,
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        const result = await client.createParty({
            shortCode: 'NWCAP',
            fullName: 'Northwind Capital',
            parentPartyId: null,
        });

        expect(result).toEqual({
            success: false,
            message: 'A party with this code already exists',
            partyId: '',
        });
    });
});

describe('OresClient provision party', () => {
    it('names the party and the starting point and reads the run it started', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.auth.login': [loginReply()],
            'iam.v1.parties.provision': [
                {
                    success: true,
                    message: '',
                    instance_id: '9c1f0f5a-6bd2-4f2a-9a4a-6f1a3a2b4c5d',
                    party_id: '0d0f9f24-6f3a-4c2b-9c2e-1a2b3c4d5e6f',
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        const result = await client.provisionParty({
            party: '0d0f9f24-6f3a-4c2b-9c2e-1a2b3c4d5e6f',
            profileCode: 'empty_operational',
            lei: '213800LBQA1Y9L22JB70',
        });

        expect(transport.decodeCall(1)).toEqual({
            party: '0d0f9f24-6f3a-4c2b-9c2e-1a2b3c4d5e6f',
            profile_code: 'empty_operational',
            lei: '213800LBQA1Y9L22JB70',
        });
        expect(result).toEqual({
            success: true,
            message: '',
            instanceId: '9c1f0f5a-6bd2-4f2a-9a4a-6f1a3a2b4c5d',
            partyId: '0d0f9f24-6f3a-4c2b-9c2e-1a2b3c4d5e6f',
        });
    });

    it('answers with the refusal the server stated rather than throwing', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.auth.login': [loginReply()],
            'iam.v1.parties.provision': [
                {
                    success: false,
                    message:
                        "The seed profile 'nope' orders no party step, so it has no party " +
                        'stage to run.',
                    instance_id: '',
                    party_id: '',
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        const result = await client.provisionParty({
            party: '0d0f9f24-6f3a-4c2b-9c2e-1a2b3c4d5e6f',
            profileCode: 'nope',
            lei: '',
        });

        expect(result.success).toBe(false);
        expect(result.message).toContain('orders no party step');
        expect(result.instanceId).toBe('');
    });
});
