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

import { z } from 'zod';
import { request } from './transport.js';

/**
 * The vault screens' reads and writes.
 *
 * The four doors are the gateway's own and none of them is generated: a write
 * hands the gateway a value it seals to the vault's transport key before the
 * value leaves it, and a reveal is an ask the vault records against the person
 * who made it. The list and the record carry no value at all.
 *
 * A row is typed from the wire's own field names, because they are the keys the
 * codec writes; the only row this client drops is the stored ciphertext, which
 * no screen has any use for.
 */

/** The outcome a read states when it has nothing else to say. */
export interface VaultResult {
    readonly outcome:
        'ok' | 'invalid' | 'denied' | 'missing' | 'conflict' | 'unavailable' | 'failed';
    readonly code: string;
    readonly message: string;
}

/** One secret as the list carries it: what the vault holds, never a value. */
export interface VaultSecret {
    readonly id: string;
    readonly name: string;
    readonly owner: string;
    readonly key_version: number;
    readonly description: string;
    readonly modified_by: string;
    readonly recorded_at: string;
}

/** The record of one secret: the metadata a person reads before asking for the value. */
export interface SecretDescription {
    readonly name: string;
    readonly owner: string;
    readonly key_version: number;
    readonly written_at: string;
}

/** One consumer: a service the vault may seal a value to. */
export interface VaultConsumer {
    readonly id: string;
    readonly name: string;
    readonly actor: string;
    readonly public_key: string;
    readonly is_enabled: boolean;
    readonly description: string;
    readonly recorded_at: string;
}

/** The answer to a reveal, and the version it came from. */
export interface SecretReveal {
    readonly name: string;
    readonly key_version: number;
    readonly value: string;
}

/** What a write states once the vault has stored the version. */
export interface SecretWrite {
    readonly name: string;
    readonly key_version: number;
}

const resultSchema = z.object({
    outcome: z.enum(['ok', 'invalid', 'denied', 'missing', 'conflict', 'unavailable', 'failed']),
    code: z.string(),
    message: z.string(),
});

const secretSchema = z.looseObject({
    id: z.string(),
    name: z.string(),
    owner: z.string(),
    key_version: z.int(),
    description: z.string(),
    modified_by: z.string(),
    recorded_at: z.string(),
});

const consumerSchema = z.looseObject({
    id: z.string(),
    name: z.string(),
    actor: z.string(),
    public_key: z.string(),
    is_enabled: z.boolean(),
    description: z.string(),
    recorded_at: z.string(),
});

/** Throws the vault's own sentence when a read states something other than ok. */
function unlessOk(answer: { result: VaultResult }): void {
    if (answer.result.outcome !== 'ok') {
        throw new Error(answer.result.message);
    }
}

/** The secrets this tenant keeps, without their values. */
export async function listSecrets(
    offset = 0,
    limit = 100,
): Promise<{ secrets: readonly VaultSecret[]; total: number }> {
    const page = await request(`/api/v1/vault/secrets?offset=${offset}&limit=${limit}`, {
        method: 'GET',
    });
    const answer = z
        .object({
            result: resultSchema,
            secrets: z.array(secretSchema),
            total: z.number(),
        })
        .parse(page);
    unlessOk(answer);
    return { secrets: answer.secrets, total: answer.total };
}

/**
 * The record of one secret.
 *
 * The door answers the metadata and never the value: a screen that shows a
 * secret is showing what the vault holds, and the value is a separate ask.
 */
export async function describeSecret(name: string): Promise<SecretDescription> {
    const page = await request(`/api/v1/vault/secrets/${encodeURIComponent(name)}`, {
        method: 'GET',
    });
    const answer = z
        .object({
            success: z.boolean(),
            message: z.string(),
            name: z.string(),
            owner: z.string(),
            key_version: z.int(),
            written_at: z.string(),
        })
        .parse(page);
    if (!answer.success) {
        throw new Error(answer.message);
    }
    return {
        name: answer.name,
        owner: answer.owner,
        key_version: answer.key_version,
        written_at: answer.written_at,
    };
}

/**
 * Ask the vault for one value.
 *
 * The ask is the act the audit records: the gateway resolves as itself because
 * it holds the key the answer is sealed to, and the vault records the person
 * whose permission let the ask happen.
 */
export async function revealSecret(name: string): Promise<SecretReveal> {
    const page = await request(`/api/v1/vault/secrets/${encodeURIComponent(name)}/value`, {
        method: 'GET',
    });
    return z.object({ name: z.string(), key_version: z.int(), value: z.string() }).parse(page);
}

/**
 * Write one version of a secret.
 *
 * The owner is the consumer the vault seals to and the only one that may resolve
 * the value. The value travels to the gateway, which seals it to the vault's
 * transport key, so the bus carries ciphertext and the vault never accepts a
 * plain value.
 */
export async function writeSecret(write: {
    readonly name: string;
    readonly owner: string;
    readonly value: string;
}): Promise<SecretWrite> {
    const page = await request('/api/v1/vault/secrets', {
        method: 'POST',
        body: JSON.stringify(write),
    });
    const answer = z
        .object({
            success: z.boolean(),
            message: z.string(),
            name: z.string(),
            key_version: z.int(),
        })
        .parse(page);
    if (!answer.success) {
        throw new Error(answer.message);
    }
    return { name: answer.name, key_version: answer.key_version };
}

/** Remove one secret, and the use record it leaves behind outlives it. */
export async function removeSecret(name: string): Promise<void> {
    const page = await request(`/api/v1/vault/secrets/${encodeURIComponent(name)}`, {
        method: 'DELETE',
    });
    const answer = z.object({ success: z.boolean(), message: z.string() }).parse(page);
    if (!answer.success) {
        throw new Error(answer.message);
    }
}

/** The services that may receive a secret, and whether each is enabled. */
export async function listConsumers(
    offset = 0,
    limit = 100,
): Promise<{ consumers: readonly VaultConsumer[]; total: number }> {
    const page = await request(`/api/v1/vault/consumers?offset=${offset}&limit=${limit}`, {
        method: 'GET',
    });
    const answer = z
        .object({
            result: resultSchema,
            consumers: z.array(consumerSchema),
            total: z.number(),
        })
        .parse(page);
    unlessOk(answer);
    return { consumers: answer.consumers, total: answer.total };
}

/** The part of a consumer's key that tells two of them apart, for a table cell. */
export function keyFingerprint(publicKey: string): string {
    return publicKey.length <= 12 ? publicKey : `${publicKey.slice(0, 12)}…`;
}
