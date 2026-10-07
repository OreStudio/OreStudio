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

interface ScriptedReply {
    readonly body?: unknown;
}

/** A transport that answers from a script and records what it received. */
class ScriptedTransport implements Transport {
    readonly calls: RecordedCall[] = [];
    readonly #script: Map<string, ScriptedReply[]>;
    readonly #codec = new WireCodec('msgpack');

    constructor(script: Record<string, ScriptedReply[]>) {
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
        const encoded = next.body === undefined ? new Uint8Array() : this.#codec.encode(next.body);
        return { subject, body: encoded, headers: {} };
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

const request = {
    profileCode: 'empty_operational',
    tenantCode: 'northwind',
    tenantName: 'Northwind Capital',
    tenantHostname: 'northwind.example.com',
    adminUsername: 'tenant_admin',
    adminEmail: 'admin@northwind.example.com',
    adminPassword: 'Secure-Password-123',
    parameters: { root_lei: '9695ACMEGROUP0000030', counterparty_size: 'large' },
} as const;

const provisionedReply = {
    success: true,
    message: '',
    instance_id: '9c1f0f5a-6bd2-4f2a-9a4a-6f1a3a2b4c5d',
    tenant_id: '0d0f9f24-6f3a-4c2b-9c2e-1a2b3c4d5e6f',
    account_id: '7a6b5c4d-3e2f-4a1b-8c9d-0e1f2a3b4c5d',
};

describe('OresClient provision tenant', () => {
    it('sends every field the request declares and reads the answer as a result', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'iam.v1.ops.provision_tenant': [{ body: provisionedReply }],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        const result = await client.provisionTenant(request);

        expect(transport.calls.map((call) => call.subject)).toEqual([
            'iam.v1.ops.login',
            'iam.v1.ops.provision_tenant',
        ]);
        // The parameters travel as one "name=value" entry each, which is the
        // shape the shell fills from a command line.
        expect(transport.decodeCall(1)).toEqual({
            profile_code: 'empty_operational',
            tenant_code: 'northwind',
            tenant_name: 'Northwind Capital',
            tenant_hostname: 'northwind.example.com',
            tenant_description: '',
            admin_username: 'tenant_admin',
            admin_email: 'admin@northwind.example.com',
            admin_password: 'Secure-Password-123',
            parameters: ['root_lei=9695ACMEGROUP0000030', 'counterparty_size=large'],
        });
        expect(result).toEqual({
            success: true,
            message: '',
            instanceId: '9c1f0f5a-6bd2-4f2a-9a4a-6f1a3a2b4c5d',
            tenantId: '0d0f9f24-6f3a-4c2b-9c2e-1a2b3c4d5e6f',
            accountId: '7a6b5c4d-3e2f-4a1b-8c9d-0e1f2a3b4c5d',
        });
    });

    it('answers with the refusal the server stated rather than throwing', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'iam.v1.ops.provision_tenant': [
                {
                    body: {
                        success: false,
                        message: "The seed profile 'nope' does not exist.",
                        instance_id: '',
                        tenant_id: '',
                        account_id: '',
                    },
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        const result = await client.provisionTenant({ ...request, profileCode: 'nope' });

        expect(result).toEqual({
            success: false,
            message: "The seed profile 'nope' does not exist.",
            instanceId: '',
            tenantId: '',
            accountId: '',
        });
    });

    it('refuses a request the browser shape does not accept', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await expect(client.provisionTenant({ ...request, tenantCode: '' })).rejects.toThrow();
        expect(transport.calls).toHaveLength(1);
    });
});
