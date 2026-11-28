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
import type { AuthenticatedCaller } from './account-operations.js';
import { OperationFailedError } from './errors.js';
import {
    type ListStoreBoundsResponse,
    type ListStoreSamplesResponse,
    type StoreBound,
    type StoreBoundAction,
    type StoreLastRun,
    type StoreSample,
    type StoreSize,
    subjects as schedulingSubjects,
} from './generated/scheduler/protocol/scheduling_operations_protocol.js';
import type { RetentionPolicyWrite } from './generated/scheduler/protocol/retention_policy_protocol.js';

// The rows the read answers with are the service's own generated types, re-stated
// here so a screen imports a store from the module that reads it.
export type { StoreBound, StoreSample };
import { subjects as policySubjects } from './generated/scheduler/protocol/retention_policy_protocol.js';
import { resultEnvelopeSchema } from './operations.js';
import { outcomeOf, preconditionFor, type WriteIntent, type WriteOutcome } from './records.js';
import { nullableWireTimestampSchema, wireTimestampSchema } from './time.js';

/**
 * The stores the installation keeps, as the housekeeping screen reads them.
 *
 * The read is the bounds view: the database states whether a store is bounded,
 * what bounds it and what its last run did, and nothing here infers a bound from
 * a policy's presence. That matters because a policy whose action the sweep
 * cannot perform reads as a bound and is not one, and a screen that decided
 * boundedness itself would have to hold the same rule about which actions run —
 * a rule the view already owns.
 *
 * The read answers every store at once and pages nothing: an installation keeps
 * a dozen stores and the screen shows them together, because comparing them is
 * the question it exists to answer.
 */

const text = z.string().default('');

/** What bounds one store, and whether the mechanism it names applies today. */
const storeBoundActionSchema = z.object({
    action: z.string().nullable().default(null),
    retention_days: z.int().positive().nullable().default(null),
    cadence_hours: z.int().positive().nullable().default(null),
    policy_code: z.string().nullable().default(null),
    policy_enabled: z.boolean().default(false),
    policy_version: z.int().nonnegative().default(0),
    policy_description: z.string().nullable().default(null),
    /*
     * Kept as the read's own word rather than a union: which mechanisms exist is
     * the server's answer, and a word it gains reaches the screen instead of
     * failing the parse.
     */
    mechanism: text,
    bounded_now: z.boolean().default(false),
}) satisfies z.ZodType<StoreBoundAction>;

/** What the bound last did, in the run's own words. */
const storeLastRunSchema = z.object({
    outcome: z.string().nullable().default(null),
    ran_at: nullableWireTimestampSchema.default(null),
    removed: z.int().nonnegative().default(0),
    detail: z.string().nullable().default(null),
}) satisfies z.ZodType<StoreLastRun>;

/** What the store held when it was last measured, or no measurement at all. */
const storeSizeSchema = z.object({
    size_bytes: z.int().nonnegative().default(0),
    measure_count: z.int().nonnegative().default(0),
    sampled_at: nullableWireTimestampSchema.default(null),
}) satisfies z.ZodType<StoreSize>;

/** One store, with what bounds it, what it last did and how large it is. */
export const storeBoundSchema = z.object({
    code: z.string(),
    display_name: text,
    description: text,
    owner_component: text,
    target_kind: text,
    target: text,
    time_column: z.string().nullable().default(null),
    measure_kind: z.string().nullable().default(null),
    bound_kind: text,
    bound: storeBoundActionSchema,
    last_run: storeLastRunSchema,
    size: storeSizeSchema,
}) satisfies z.ZodType<StoreBound>;

const listStoreBoundsReplySchema = z.object({
    success: z.boolean().default(false),
    message: text,
    stores: z.array(storeBoundSchema).default([]),
}) satisfies z.ZodType<ListStoreBoundsResponse>;

/**
 * Every store the installation keeps, in catalogue order.
 *
 * A refusal is an error rather than an empty catalogue: an empty answer reads as
 * an installation that keeps nothing, which is the opposite of what a refused
 * read means.
 */
export async function readStoreBounds(caller: AuthenticatedCaller): Promise<readonly StoreBound[]> {
    const reply = await caller.callAuthenticated(
        schedulingSubjects.list_store_bounds_request,
        {},
        listStoreBoundsReplySchema,
    );
    if (!reply.success) {
        throw new OperationFailedError(schedulingSubjects.list_store_bounds_request, reply.message);
    }
    return reply.stores;
}

/** One measurement of one store, at the instant it was taken. */
export const storeSampleSchema = z.object({
    sampled_at: wireTimestampSchema,
    size_bytes: z.int().nonnegative().default(0),
    measure_count: z.int().nonnegative().default(0),
});

/**
 * The horizons a store's series is read over.
 *
 * The sampler writes one point an hour, so a day is 24 points, a week 168 and a
 * month 720. The screen offers these rather than a count of points, because a
 * count would move the window whenever the sampler's cadence changed.
 */
export const STORE_RANGES = ['24h', '7d', '30d'] as const;

export type StoreRange = (typeof STORE_RANGES)[number];

const listStoreSamplesReplySchema = z.object({
    success: z.boolean().default(false),
    message: text,
    samples: z.array(storeSampleSchema).default([]),
    from_at: wireTimestampSchema,
    to_at: wireTimestampSchema,
}) satisfies z.ZodType<ListStoreSamplesResponse>;

/** One store's measurements over a window, and the window the read covered. */
export interface StoreSampleSeries {
    /** The points, oldest first, so a plot reads left to right. */
    readonly samples: readonly StoreSample[];
    /** Where the window opens, in the deployment's clock. */
    readonly fromAt: string;
    /** The instant the read ran, which is where the window closes. */
    readonly toAt: string;
}

/** The measurements of one store over a range, oldest first. */
export async function readStoreSamples(
    caller: AuthenticatedCaller,
    storeCode: string,
    range: StoreRange = '7d',
): Promise<StoreSampleSeries> {
    const reply = await caller.callAuthenticated(
        schedulingSubjects.list_store_samples_request,
        { store_code: storeCode, range },
        listStoreSamplesReplySchema,
    );
    if (!reply.success) {
        throw new OperationFailedError(
            schedulingSubjects.list_store_samples_request,
            reply.message,
        );
    }
    return { samples: reply.samples, fromAt: reply.from_at, toAt: reply.to_at };
}

/**
 * The two fields of a policy a person changes, and nothing else.
 *
 * A policy states the store it bounds, the shape of the store's target and the
 * action to take, and none of those is a decision a form makes: the catalogue
 * states them and the publish keeps them. A screen that sent them back would be
 * able to retarget a bound, so the write names only what a person decided.
 */
export interface StoreBoundChange {
    readonly enabled: boolean;
    readonly retentionDays: number;
    readonly cadenceHours: number;
}

/**
 * Writes one store's bound, taking the contract fields from the store itself.
 *
 * The version is the one the read answered: zero creates a policy where none
 * exists, and any other value is refused when the policy moved in the meantime.
 * A store the catalogue says no period can bound has no action to send, so it
 * cannot be written at all, and the screen says so instead of offering a form.
 *
 * The policy's code is the store's code when the screen creates it: a policy is
 * named once and its name is what the runs ledger records, so the name comes
 * from the store rather than from the attempt.
 */
export async function saveStoreBound(
    caller: AuthenticatedCaller,
    store: StoreBound,
    change: StoreBoundChange,
    intent: WriteIntent,
): Promise<WriteOutcome> {
    const action = store.bound.action;
    if (action === null || action === '') {
        throw new OperationFailedError(
            policySubjects.put_retention_policy_request,
            `The catalogue states no action that bounds ${store.code}.`,
        );
    }
    const write: RetentionPolicyWrite = {
        code: store.bound.policy_code ?? store.code,
        target_kind: store.target_kind,
        target: store.target,
        store_code: store.code,
        action,
        retention_days: change.retentionDays,
        cadence_hours: change.cadenceHours,
        enabled: change.enabled,
        description: store.bound.policy_description ?? '',
    };
    const reply = await caller.callAuthenticated(
        policySubjects.put_retention_policy_request,
        {
            change: {
                write,
                precondition: preconditionFor(
                    store.bound.policy_version === 0 ? null : store.bound.policy_version,
                ),
            },
            intent: { reason_code: intent.reasonCode, commentary: intent.commentary },
        },
        z.object({ result: resultEnvelopeSchema }),
    );
    return outcomeOf(reply.result);
}

/**
 * What the browser reads from the housekeeping list: every store as the read
 * answered it, with the moment the BFF asked.
 *
 * The moment is the BFF's clock, not the browser's, so a screen that says how
 * long ago a store was measured is stating the deployment's age.
 */
export const storeBoundsViewSchema = z.object({
    read_at: nullableWireTimestampSchema.default(null),
    rows: z.array(storeBoundSchema).default([]),
});

export type StoreBoundsView = z.infer<typeof storeBoundsViewSchema>;

/**
 * One store's series, as the store's own page draws it.
 *
 * The window is the read's own answer, so the plot's axis states the horizon the
 * installation measured and not the one the browser's clock would guess.
 */
export const storeSamplesViewSchema = z.object({
    store_code: z.string().default(''),
    samples: z.array(storeSampleSchema).default([]),
    from_at: nullableWireTimestampSchema.default(null),
    to_at: nullableWireTimestampSchema.default(null),
});

export type StoreSamplesView = z.infer<typeof storeSamplesViewSchema>;

/**
 * What a person sends to change a store's bound, beside the reason for it.
 *
 * The three numbers are the decision; the reason is the audit trail. The store,
 * the target and the action are not here because they are not the form's to say.
 */
export const storeBoundWriteSchema = z
    .object({
        enabled: z.boolean(),
        retention_days: z.int().positive().max(3650),
        cadence_hours: z.int().positive().max(8760),
        reason_code: z.string().trim().min(1).max(100),
        commentary: z.string().max(2000).default(''),
    })
    .strict();

export type StoreBoundWrite = z.infer<typeof storeBoundWriteSchema>;

/** What a write answers: whether it happened, and what the server said when it did not. */
export const storeBoundResultSchema = z.object({
    success: z.boolean().default(false),
    message: text,
    outcome: z.string().default(''),
});

export type StoreBoundResult = z.infer<typeof storeBoundResultSchema>;
