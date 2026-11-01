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
import { randomUUID } from 'node:crypto';
import type { FastifyInstance, FastifyRequest } from 'fastify';
import { z } from 'zod';
import { SYSTEM_TENANT_ID } from '@ores/wire-protocol';
import { subjects as feedSubjects } from '@ores/wire-protocol/generated/synthetic/protocol/feed_config_protocol';
import { subjects as policySubjects } from '@ores/wire-protocol/generated/synthetic/protocol/calibration_policy_protocol';
import { subjects as orderSubjects } from '@ores/wire-protocol/generated/synthetic/protocol/calibration_provider_order_protocol';
import { subjects as itemSubjects } from '@ores/wire-protocol/generated/synthetic/protocol/fx_spot_fixing_item_protocol';
import { HttpFailure, notPermitted } from './errors.js';
import { input, intentSchema, paramId, readAll, refusal, write } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';
import { feedStartSchema, operationReplySchema, upstreamFailure } from './synthetic.js';

/** An asset class as its refdata code names it. */
const assetClassSchema = z
    .string()
    .trim()
    .regex(/^[a-z0-9_]{1,40}$/);

/** The policy a class calibrates under, as the service states it. */
const calibrationPolicySchema = z.object({
    from_policy_row: z.boolean().default(false),
    window_days: z.int().default(0),
    allow_inverse: z.boolean().default(false),
    fallback_code: z.string().default(''),
    provider_order: z.array(z.string()).default([]),
});

/** One candidate a feed read, and what became of it. */
const candidateOutcomeSchema = z.object({
    item: z.string(),
    provider_code: z.string(),
    inverted: z.boolean().default(false),
    rank: z.int().default(0),
    state: z.enum(['chosen', 'skipped', 'not_reached']),
    reason: z.string().default(''),
});

/** How one feed of the class resolved under the policy. */
const calibratedFeedSchema = z.object({
    config_id: z.string(),
    source_name: z.string(),
    base_currency_code: z.string().default(''),
    quote_currency_code: z.string().default(''),
    explicit_items: z.boolean().default(false),
    window_days_override: z.int().nullish(),
    refused: z.boolean().default(false),
    start: feedStartSchema,
    candidates: z.array(candidateOutcomeSchema).default([]),
});

export type CalibratedFeed = z.infer<typeof calibratedFeedSchema>;

/**
 * The counts a class's calibration covers: how many feeds start from a fixing, keep their
 * fixed level, are refused, and override the policy.
 */
export function calibrationCoverage(feeds: readonly CalibratedFeed[]) {
    return {
        total: feeds.length,
        fromFixing: feeds.filter((f) => f.start.from_fixing).length,
        keptFixed: feeds.filter((f) => !f.start.from_fixing && !f.refused).length,
        refused: feeds.filter((f) => f.refused).length,
        overridden: feeds.filter((f) => f.explicit_items || f.window_days_override != null).length,
    };
}

/** A class's policy row, as stored, when it has one. */
const policyRowSchema = z.looseObject({
    id: z.string(),
    version: z.int(),
    asset_class_code: z.string(),
});

const orderRowSchema = z.looseObject({
    id: z.string(),
    version: z.int(),
    policy_id: z.string(),
    provider_code: z.string(),
    rank: z.int(),
});

/** The policy a person states for a class: the window, the inverse rule, the order, the fallback. */
const policyBodySchema = z.object({
    intent: intentSchema,
    version: z.int().nonnegative().nullable(),
    window_days: z.int().min(2).max(180),
    allow_inverse: z.boolean(),
    fallback_code: z.enum(['keep_fixed', 'refuse']),
    provider_order: z.array(z.string().trim().min(1).max(100)).max(50),
});

const removeBodySchema = z.object({ intent: intentSchema });

async function policyRowOf(session: LiveSession, assetClass: string) {
    const rows = await readAll(
        session,
        policySubjects.list_calibration_policies_request,
        'calibration_policies',
        {
            filter: null,
            as_of: null,
        },
    );
    const found = rows.find((r) => r['asset_class_code'] === assetClass);
    return found === undefined ? undefined : policyRowSchema.parse(found);
}

async function orderRowsOf(session: LiveSession, policyId: string) {
    const rows = await readAll(
        session,
        orderSubjects.list_calibration_provider_orders_request,
        'calibration_provider_orders',
        {
            filter: {
                policy_id: policyId,
                provider_code: null,
                id_one_of: null,
                policy_id_one_of: null,
                provider_code_one_of: null,
            },
            as_of: null,
        },
    );
    return rows.map((r) => orderRowSchema.parse(r));
}

/** A write that did not succeed, as the HTTP failure it means. */
function ensure(result: Awaited<ReturnType<typeof write>>): void {
    if (result.outcome !== 'ok') {
        throw refusal(result);
    }
}

/** The policy is the installation's: only the system tenant changes it. */
function requireSystem(session: LiveSession): void {
    if (session.tenantId !== SYSTEM_TENANT_ID) {
        throw notPermitted(
            'A calibration policy belongs to the installation, so only the system tenant changes it.',
        );
    }
}

function assetClassOf(request: FastifyRequest): string {
    return input(assetClassSchema, (request.params as { assetClass: string }).assetClass);
}

/** The items a feed names, when it names its own instead of deriving them. */
async function itemRowsOf(session: LiveSession, configId: string) {
    const rows = await readAll(
        session,
        itemSubjects.list_fx_spot_fixing_items_request,
        'fx_spot_fixing_items',
        {
            filter: {
                fx_spot_config_id: configId,
                id_one_of: null,
                fx_spot_config_id_one_of: null,
            },
            as_of: null,
        },
    );
    return rows.map((r) => z.looseObject({ id: z.string(), version: z.int() }).parse(r));
}

async function removeItems(
    session: LiveSession,
    intent: z.infer<typeof intentSchema>,
    rows: readonly { readonly id: string }[],
): Promise<void> {
    if (rows.length === 0) {
        return;
    }
    ensure(
        await write(session, itemSubjects.delete_many_fx_spot_fixing_items_request, {
            intent,
            removals: rows.map((r) => ({
                key: { id: r.id },
                precondition: { kind: 'any', version: null },
            })),
        }),
    );
}

/** One candidate a person pins on a feed, as the exception to its policy. */
const pinBodySchema = z.object({
    intent: intentSchema,
    item: z
        .string()
        .trim()
        .max(500)
        .regex(/^oresmd:\/\/fx\/[A-Z]{3}\?.*type=fixing/),
    provider_code: z
        .string()
        .trim()
        .regex(/^[a-z0-9_]{1,100}$/),
    window_days: z.int().min(2).max(180),
});

export function registerCalibrationRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /**
     * Calibrate an asset class: the policy it calibrates under, how every feed of it
     * resolves with its candidates, and the coverage counts. Nothing starts; every feed is
     * resolved.
     */
    server.get('/api/synthetic/calibration/:assetClass', async (request) => {
        const session = requireSession(request);
        const assetClass = assetClassOf(request);
        const reply = await session.client.callAuthenticated(
            feedSubjects.calibrate_request,
            { asset_class_code: assetClass },
            operationReplySchema.extend({
                policy: calibrationPolicySchema,
                feeds: z.array(calibratedFeedSchema).default([]),
            }),
        );
        if (!reply.success) {
            throw upstreamFailure(reply.message);
        }
        const row = reply.policy.from_policy_row
            ? await policyRowOf(session, assetClass)
            : undefined;
        return {
            assetClass,
            policy: { ...reply.policy, version: row?.version ?? null },
            policyWritable: session.tenantId === SYSTEM_TENANT_ID,
            feeds: reply.feeds,
            coverage: calibrationCoverage(reply.feeds),
        };
    });

    /**
     * States a class's policy. The policy row is written against the version read. When the
     * provider order differs from the stored one, the stored rungs are removed and the list
     * is written from rank 1: a rank is unique within a policy, so rungs are not moved in
     * place. Each write converges, so a save that fails part way is completed by saving again.
     */
    server.put('/api/synthetic/calibration/:assetClass/policy', async (request, reply) => {
        const session = requireSession(request);
        requireSystem(session);
        const assetClass = assetClassOf(request);
        const body = input(policyBodySchema, request.body);
        if (new Set(body.provider_order).size !== body.provider_order.length) {
            throw new HttpFailure(400, {
                code: 'invalid-request',
                message: 'The provider order names a provider twice.',
            });
        }

        const existing = await policyRowOf(session, assetClass);
        const policyId = existing?.id ?? randomUUID();
        ensure(
            await write(session, policySubjects.put_many_calibration_policies_request, {
                intent: body.intent,
                changes: [
                    {
                        write: {
                            id: policyId,
                            asset_class_code: assetClass,
                            window_days: body.window_days,
                            allow_inverse: body.allow_inverse,
                            fallback_code: body.fallback_code,
                        },
                        precondition:
                            body.version === null
                                ? { kind: 'must_not_exist', version: null }
                                : { kind: 'must_match_version', version: body.version },
                    },
                ],
            }),
        );

        const orders = existing === undefined ? [] : await orderRowsOf(session, policyId);
        const current = [...orders].sort((x, y) => x.rank - y.rank).map((o) => o.provider_code);
        const unchanged =
            current.length === body.provider_order.length &&
            current.every((code, index) => code === body.provider_order[index]);
        if (!unchanged) {
            if (orders.length > 0) {
                ensure(
                    await write(
                        session,
                        orderSubjects.delete_many_calibration_provider_orders_request,
                        {
                            intent: body.intent,
                            removals: orders.map((o) => ({
                                key: { id: o.id },
                                precondition: { kind: 'any', version: null },
                            })),
                        },
                    ),
                );
            }
            if (body.provider_order.length > 0) {
                ensure(
                    await write(
                        session,
                        orderSubjects.put_many_calibration_provider_orders_request,
                        {
                            intent: body.intent,
                            changes: body.provider_order.map((provider, index) => ({
                                write: {
                                    id: randomUUID(),
                                    policy_id: policyId,
                                    provider_code: provider,
                                    rank: index + 1,
                                },
                                precondition: { kind: 'must_not_exist', version: null },
                            })),
                        },
                    ),
                );
            }
        }
        return reply.code(204).send();
    });

    /** Returns a class to automatic calibration: its provider order and its policy row go. */
    server.delete('/api/synthetic/calibration/:assetClass/policy', async (request, reply) => {
        const session = requireSession(request);
        requireSystem(session);
        const assetClass = assetClassOf(request);
        const body = input(removeBodySchema, request.body);
        const existing = await policyRowOf(session, assetClass);
        if (existing === undefined) {
            return reply.code(204).send();
        }
        const orders = await orderRowsOf(session, existing.id);
        if (orders.length > 0) {
            ensure(
                await write(
                    session,
                    orderSubjects.delete_many_calibration_provider_orders_request,
                    {
                        intent: body.intent,
                        removals: orders.map((o) => ({
                            key: { id: o.id },
                            precondition: { kind: 'any', version: null },
                        })),
                    },
                ),
            );
        }
        ensure(
            await write(session, policySubjects.delete_calibration_policy_request, {
                intent: body.intent,
                removal: {
                    key: { id: existing.id },
                    precondition: { kind: 'must_match_version', version: existing.version },
                },
            }),
        );
        return reply.code(204).send();
    });

    /**
     * Pins one candidate on a feed: the feed's own items become that one item, read on or
     * before the clock over the window given. The feed then overrides its class policy. A
     * feed belongs to its tenant, so any tenant pins its own feeds. The old items go first:
     * a write that then fails leaves the feed following its policy, and pinning again
     * completes it.
     */
    server.put('/api/synthetic/calibration/feeds/:id/pin', async (request, reply) => {
        const session = requireSession(request);
        const configId = paramId(request);
        const body = input(pinBodySchema, request.body);
        await removeItems(session, body.intent, await itemRowsOf(session, configId));
        ensure(
            await write(session, itemSubjects.put_many_fx_spot_fixing_items_request, {
                intent: body.intent,
                changes: [
                    {
                        write: {
                            id: randomUUID(),
                            fx_spot_config_id: configId,
                            rank: 1,
                            item: body.item,
                            provider_code: body.provider_code,
                            policy: 'latest_on_or_before_clock',
                            window_days: body.window_days,
                        },
                        precondition: { kind: 'must_not_exist', version: null },
                    },
                ],
            }),
        );
        return reply.code(204).send();
    });

    /** Clears a feed's own items, so it derives its candidates from the policy again. */
    server.delete('/api/synthetic/calibration/feeds/:id/pin', async (request, reply) => {
        const session = requireSession(request);
        const configId = paramId(request);
        const body = input(removeBodySchema, request.body);
        await removeItems(session, body.intent, await itemRowsOf(session, configId));
        return reply.code(204).send();
    });
}
