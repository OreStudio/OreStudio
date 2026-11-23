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

import { z } from 'zod';
import { request } from './transport.js';

/*
 * The person's own LLM agent grants and keys, read and written through the
 * operation routes ores.http generates. The key comes back once, from create;
 * nothing here keeps it.
 */

const answer = { success: z.boolean(), message: z.string() };

const grantSchema = z.looseObject({
    id: z.string(),
    grantor_account_id: z.string(),
    agent_name: z.string(),
    role_id: z.string(),
    not_after: z.string(),
    revoked_at: z.string(),
});

const keySchema = z.looseObject({
    id: z.string(),
    grant_id: z.string(),
    revoked_at: z.string(),
    recorded_at: z.string(),
});

export type AgentGrant = z.infer<typeof grantSchema>;
export type AgentKey = z.infer<typeof keySchema>;

/** An instant the server leaves at the epoch has not happened. */
export function isStamped(instant: string): boolean {
    return instant !== '' && !instant.startsWith('1970-01-01');
}

async function post<T extends z.ZodType>(
    path: string,
    body: unknown,
    schema: T,
): Promise<z.infer<T>> {
    const reply = z
        .looseObject(answer)
        .parse(await request(path, { method: 'POST', body: JSON.stringify(body) }));
    if (!reply.success) throw new Error(reply.message);
    return schema.parse(reply);
}

export const agentGrants = {
    async listOwn(): Promise<{ grants: AgentGrant[]; keys: AgentKey[] }> {
        return post(
            '/api/v1/iam/ops/list_own_agent_grants',
            {},
            z.object({ ...answer, grants: z.array(grantSchema), keys: z.array(keySchema) }),
        );
    },
    /** Every grant and key in the reader's parties; needs iam::agent_grants:read. */
    async listAll(): Promise<{ grants: AgentGrant[]; keys: AgentKey[] }> {
        return post(
            '/api/v1/iam/ops/list_every_agent_grant',
            {},
            z.object({ ...answer, grants: z.array(grantSchema), keys: z.array(keySchema) }),
        );
    },
    async create(agentName: string, role: string, validDays: number): Promise<string> {
        const reply = await post(
            '/api/v1/iam/agent_grants/put',
            { agent_name: agentName, role, valid_days: validDays },
            z.object({ ...answer, grant_id: z.string(), key: z.string() }),
        );
        return reply.key;
    },
    async createKey(grantId: string): Promise<string> {
        const reply = await post(
            '/api/v1/iam/ops/create_agent_key',
            { grant_id: grantId },
            z.object({ ...answer, key_id: z.string(), key: z.string() }),
        );
        return reply.key;
    },
    async revokeGrant(grantId: string, reason: string): Promise<void> {
        await post(
            '/api/v1/iam/ops/revoke_agent_grant',
            { grant_id: grantId, reason },
            z.object(answer),
        );
    },
    async revokeKey(keyId: string, reason: string): Promise<void> {
        await post('/api/v1/iam/ops/revoke_agent_key', { key_id: keyId, reason }, z.object(answer));
    },
};
