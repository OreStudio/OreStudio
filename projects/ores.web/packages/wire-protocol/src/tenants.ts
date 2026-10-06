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
import {
    uuidSchema,
    wireTimestampSchema,
    type TenantDetail,
    type TenantSetup,
    type TenantSummary,
} from './domain.js';
import {
    subjects as tenantSubjects,
    type DeleteTenantRequest,
    type GetTenantRequest,
    type ListTenantsRequest,
} from './generated/iam/protocol/tenant_protocol.js';
import type { Tenant } from './generated/iam/domain/tenant.js';
import {
    subjects as workflowSubjects,
    type ListWorkflowInstanceSummariesRequest,
    type ListWorkflowInstanceSummariesResponse,
    type WorkflowInstanceSummary,
} from './generated/workflow/protocol/workflow_protocol.js';

/**
 * The tenants a deployment holds, as the registry answers them.
 *
 * A tenant row is system-scoped: the registry's own check requires every row to
 * carry the system tenant in `tenant_id`, so that column names the owner of the
 * registry and never the tenant the row describes. A row's identity is its
 * `id`, and the deployment's own bookkeeping is the row whose id is the system
 * id. A reader that filters on the wrong column answers "no tenants" for a
 * deployment full of them.
 */

/** Subjects for the tenant reads, kept beside the operations that use them. */
export const TENANT_SUBJECTS = {
    list: tenantSubjects.list_tenants_request,
    get: tenantSubjects.get_tenant_request,
    delete: tenantSubjects.delete_tenant_request,
} as const;

/** The row as the registry writes it. */
const wireTenantSchema = z.object({
    version: z.int().nonnegative().default(0),
    tenant_id: z.string().default(''),
    id: uuidSchema,
    code: z.string(),
    name: z.string(),
    type: z.string(),
    description: z.string().default(''),
    hostname: z.string().default(''),
    status: z.string(),
    is_registration_default: z.boolean().default(false),
    modified_by: z.string().default(''),
    performed_by: z.string().default(''),
    change_reason_code: z.string().default(''),
    change_commentary: z.string().default(''),
    recorded_at: wireTimestampSchema,
});

/**
 * One tenant, as a screen reads it.
 *
 * The registry's audit columns are dropped rather than forwarded: a roster
 * names each tenant and says what state it is in, and nothing on it is about
 * who last edited the row.
 */
function summaryOf(row: z.infer<typeof wireTenantSchema>): TenantSummary {
    return {
        id: row.id,
        code: row.code,
        name: row.name,
        type: row.type,
        description: row.description,
        hostname: row.hostname,
        status: row.status,
        registrationDefault: row.is_registration_default,
        setup: null,
    };
}

/*
 * The schema reads every field the generated tenant declares. A field the C++
 * registry gains and this schema does not read fails the typecheck here.
 */
const wireTenantReadsEveryField: [
    Exclude<keyof Tenant, keyof z.input<typeof wireTenantSchema>>,
] extends [never]
    ? true
    : false = true;
void wireTenantReadsEveryField;

/** One tenant with the row's provenance, for the tenant's own screen. */
function detailOf(row: z.infer<typeof wireTenantSchema>): TenantDetail {
    return {
        ...summaryOf(row),
        version: row.version,
        modifiedBy: row.modified_by,
        performedBy: row.performed_by,
        changeReasonCode: row.change_reason_code,
        changeCommentary: row.change_commentary,
        recordedAt: row.recorded_at,
    };
}

/** The get's answer: the outcome, and the tenant when it was found. */
const wireTenantGetSchema = z.object({
    result: z.object({
        outcome: z.string(),
        message: z.string().default(''),
    }),
    tenant: wireTenantSchema.nullable().default(null),
});

/**
 * One tenant by its code, or `null` when no tenant holds the code.
 *
 * The get keys a tenant by its code, the tenant's stable name in a request.
 */
export async function readTenant(
    caller: AuthenticatedCaller,
    code: string,
): Promise<TenantDetail | null> {
    const request: GetTenantRequest = { key: { code } };
    const answer = await caller.callAuthenticated(
        TENANT_SUBJECTS.get,
        request,
        wireTenantGetSchema,
    );
    if (answer.result.outcome === 'missing') {
        return null;
    }
    if (answer.result.outcome !== 'ok' || answer.tenant === null) {
        throw new Error(
            answer.result.message === '' ? 'The tenant was not read.' : answer.result.message,
        );
    }
    return detailOf(answer.tenant);
}

const wireDeleteTenantSchema = z.object({
    result: z.object({
        outcome: z.string(),
        message: z.string().default(''),
    }),
});

/** How a removal ended: removed, or the server's outcome and its words. */
export type TenantRemovalOutcome =
    | { readonly removed: true }
    | { readonly removed: false; readonly outcome: string; readonly message: string };

/**
 * Removes the tenant with this code.
 *
 * The server closes the tenant's row and marks it terminated, and keeps its
 * data. Sign-in and token refresh then refuse it. A removal that is refused
 * is an answer, not a failed call.
 */
export async function removeTenant(
    caller: AuthenticatedCaller,
    code: string,
): Promise<TenantRemovalOutcome> {
    const request: DeleteTenantRequest = {
        removal: { key: { code }, precondition: { kind: 'any', version: null } },
        intent: { reason_code: '', commentary: '' },
    };
    const answer = await caller.callAuthenticated(
        TENANT_SUBJECTS.delete,
        request,
        wireDeleteTenantSchema,
    );
    return answer.result.outcome === 'ok'
        ? { removed: true }
        : { removed: false, outcome: answer.result.outcome, message: answer.result.message };
}

/**
 * What a roster read narrows by. Every member is optional; the ones set must
 * all hold.
 */
export interface TenantListQuery {
    /** Text the code, the name or the hostname contains, ignoring case. */
    readonly search?: string;
    /** The one type asked for. */
    readonly type?: string;
    /** The one status asked for. */
    readonly status?: string;
    /** The types a row may have; a row of any other type is left out. */
    readonly types?: readonly string[];
    /** The tenants asked for by id; any other tenant is left out. */
    readonly ids?: readonly string[];
    readonly offset?: number;
    readonly limit?: number;
}

/** The page of tenants, translated from the wire's names. */
export const wireTenantPageSchema = z
    .object({
        result: z.object({
            outcome: z.string(),
            message: z.string().default(''),
        }),
        tenants: z.array(wireTenantSchema).default([]),
        total: z.int().nonnegative().default(0),
    })
    .transform((row) => ({
        outcome: row.result.outcome,
        message: row.result.message,
        tenants: row.tenants.map(summaryOf),
        totalCount: row.total,
    }));
export interface WireTenantPage {
    readonly tenants: TenantSummary[];
    readonly totalCount: number;
}

/**
 * One page of the tenants that match, in code order, with how many match in
 * all.
 *
 * The request is the generated type, so every field the server's decoder
 * requires is present or the typecheck fails. A member left unset is sent as
 * null, which sets no condition.
 */
export async function listTenantsPage(
    caller: AuthenticatedCaller,
    input: TenantListQuery = {},
): Promise<WireTenantPage> {
    const request: ListTenantsRequest = {
        offset: input.offset ?? 0,
        limit: input.limit ?? 100,
        order: { field: 'code', descending: false },
        as_of: null,
        filter: {
            type: input.type === undefined || input.type === '' ? null : input.type,
            status: input.status === undefined || input.status === '' ? null : input.status,
            id_one_of: input.ids === undefined ? null : [...input.ids],
            type_one_of: input.types === undefined ? null : [...input.types],
            status_one_of: null,
            search: input.search === undefined || input.search === '' ? null : input.search,
        },
    };
    const answer = await caller.callAuthenticated(
        TENANT_SUBJECTS.list,
        request,
        wireTenantPageSchema,
    );
    if (answer.outcome !== 'ok') {
        throw new Error(answer.message === '' ? 'The tenants were not read.' : answer.message);
    }
    return { tenants: answer.tenants, totalCount: answer.totalCount };
}

/**
 * The run type and the target kind ores.iam names on a tenant's provisioning
 * run. They mirror `provision_tenant_workflow_type` and
 * `provision_tenant_target_kind` in C++, and a rename there must be mirrored
 * here or the roster stops finding any run.
 */
export const PROVISION_TENANT_WORKFLOW_TYPE = 'provision_tenant_workflow';
export const PROVISION_TENANT_TARGET_KIND = 'tenant';

/**
 * One run, as the instances list answers it.
 *
 * The shape is the generated `WorkflowInstanceSummary`, and `satisfies` holds
 * the schema to it: a field the C++ protocol renames or adds fails the
 * typecheck here instead of decoding to a default at run time.
 */
const wireWorkflowInstanceSummarySchema = z.object({
    id: z.string(),
    type: z.string().default(''),
    status: z.string().default(''),
    current_step_index: z.int().nonnegative().default(0),
    step_count: z.int().nonnegative().default(0),
    correlation_id: z.string().default(''),
    created_by: z.string().default(''),
    created_at: z.string().default(''),
    completed_at: z.string().nullable().default(null),
    error: z.string().default(''),
    target_kind: z.string().default(''),
    target_id: z.string().default(''),
}) satisfies z.ZodType<WorkflowInstanceSummary>;

/** The list's answer. `success` defaults to false, so a bare answer reads as a failure. */
export const wireWorkflowInstancesSchema = z.object({
    success: z.boolean().default(false),
    message: z.string().default(''),
    instances: z.array(wireWorkflowInstanceSummarySchema).default([]),
}) satisfies z.ZodType<ListWorkflowInstanceSummariesResponse>;

/**
 * The most provisioning runs one read answers, and the most tenants it may
 * name: the protocol's bound on one reply.
 */
export const TENANT_SETUP_READ_LIMIT = 1000;

/**
 * The runs a read found, and whether it saw all of them.
 *
 * `complete` is false when the answer reached the limit, which takes more than
 * one run for each named tenant on average. The engine answers the run that
 * changed last first, so the runs cut off are the oldest.
 */
export interface TenantSetups {
    readonly setups: ReadonlyMap<string, TenantSetup>;
    readonly complete: boolean;
}

/**
 * The latest provisioning run for each named tenant, keyed by tenant id.
 *
 * The read names the tenants, so it answers the runs that act on them and
 * nothing else the engine holds; a roster names the tenants on its page. No
 * tenant named is no read, because an empty list would ask for every run.
 *
 * A tenant may have more than one run, because nothing stops a second attempt.
 * The engine answers the run that changed last first, so the first run seen for
 * a tenant is the one that moved most recently, and that is the one reported.
 */
export async function readTenantSetups(
    caller: AuthenticatedCaller,
    tenantIds: readonly string[],
): Promise<TenantSetups> {
    if (tenantIds.length === 0) {
        return { setups: new Map(), complete: true };
    }
    /*
     * The request is the generated type, so every field the server's decoder
     * requires is present or the typecheck fails. An empty filter is no filter.
     */
    const request: ListWorkflowInstanceSummariesRequest = {
        limit: TENANT_SETUP_READ_LIMIT,
        status_filter: '',
        type_filter: PROVISION_TENANT_WORKFLOW_TYPE,
        target_kind_filter: PROVISION_TENANT_TARGET_KIND,
        target_id_filter: '',
        target_ids_filter: [...tenantIds],
    };
    const answer = await caller.callAuthenticated(
        workflowSubjects.list_workflow_instance_summaries_request,
        request,
        wireWorkflowInstancesSchema,
    );
    if (!answer.success) {
        throw new Error(
            answer.message === '' ? 'The provisioning runs were not read.' : answer.message,
        );
    }
    const setups = new Map<string, TenantSetup>();
    for (const run of answer.instances) {
        if (run.target_id === '' || setups.has(run.target_id)) {
            continue;
        }
        setups.set(run.target_id, {
            instanceId: run.id,
            status: run.status,
            currentStepIndex: run.current_step_index,
            stepCount: run.step_count,
            error: run.error,
        });
    }
    return { setups, complete: answer.instances.length < TENANT_SETUP_READ_LIMIT };
}

/** One provisioning run, as a screen that reports activity reads it. */
export interface ProvisioningRun {
    readonly instanceId: string;
    readonly tenantId: string;
    readonly status: string;
    readonly currentStepIndex: number;
    readonly stepCount: number;
    readonly error: string;
    /** When the run finished, or started when it has not. */
    readonly at: string;
}

/**
 * The newest provisioning runs, the one that changed last first.
 *
 * `status` narrows them to one engine state, such as `failed`. The read is one
 * page of the engine's answer, so `limit` is the most it returns.
 */
export async function readProvisioningRuns(
    caller: AuthenticatedCaller,
    input: { readonly status?: string; readonly limit: number },
): Promise<readonly ProvisioningRun[]> {
    const request: ListWorkflowInstanceSummariesRequest = {
        limit: input.limit,
        status_filter: input.status ?? '',
        type_filter: PROVISION_TENANT_WORKFLOW_TYPE,
        target_kind_filter: PROVISION_TENANT_TARGET_KIND,
        target_id_filter: '',
        target_ids_filter: [],
    };
    const answer = await caller.callAuthenticated(
        workflowSubjects.list_workflow_instance_summaries_request,
        request,
        wireWorkflowInstancesSchema,
    );
    if (!answer.success) {
        throw new Error(
            answer.message === '' ? 'The provisioning runs were not read.' : answer.message,
        );
    }
    return answer.instances
        .filter((run) => run.target_id !== '')
        .map((run) => ({
            instanceId: run.id,
            tenantId: run.target_id,
            status: run.status,
            currentStepIndex: run.current_step_index,
            stepCount: run.step_count,
            error: run.error,
            at: run.completed_at ?? run.created_at,
        }));
}
