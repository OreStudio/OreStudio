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
import { uuidSchema, wireTimestampSchema, type TenantSetup, type TenantSummary } from './domain.js';
import { subjects as tenantSubjects } from './generated/iam/protocol/tenant_protocol.js';
import {
    subjects as workflowSubjects,
    type ListWorkflowInstanceSummariesRequest,
    type ListWorkflowInstanceSummariesResponse,
    type WorkflowInstanceSummary,
} from './generated/workflow/protocol/workflow_protocol.js';
import { orderSchema } from './operations.js';

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
} as const;

/** The row as the registry writes it. */
const wireTenantSchema = z.object({
    version: z.int().nonnegative().default(0),
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

/** `list_tenants_request`, sent on `iam.v1.tenants.list`. */
export const listTenantsRequestSchema = z.object({
    offset: z.int().nonnegative().default(0),
    limit: z.int().positive().max(1000).default(100),
    order: orderSchema.default({ field: '', descending: false }),
});
export type ListTenantsRequest = z.infer<typeof listTenantsRequestSchema>;

/** The page of tenants, translated from the wire's names. */
export const wireTenantPageSchema = z
    .object({
        tenants: z.array(wireTenantSchema).default([]),
        total: z.int().nonnegative().default(0),
    })
    .transform((row) => ({
        tenants: row.tenants.map(summaryOf),
        totalCount: row.total,
    }));
export type WireTenantPage = z.infer<typeof wireTenantPageSchema>;

/** The page of tenants the caller may see. */
export async function readTenantsPage(
    caller: AuthenticatedCaller,
    input: { readonly offset?: number; readonly limit?: number } = {},
): Promise<WireTenantPage> {
    return caller.callAuthenticated(
        TENANT_SUBJECTS.list,
        listTenantsRequestSchema.parse(input),
        wireTenantPageSchema,
    );
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

/** The most provisioning runs one roster read asks for. */
export const TENANT_SETUP_READ_LIMIT = 1000;

/**
 * The runs a roster read found, and whether it saw all of them.
 *
 * `complete` is false when the answer reached the limit. The engine answers
 * the run that changed last first, so the runs cut off are the oldest, and a
 * tenant whose only run is among them shows no setup.
 */
export interface TenantSetups {
    readonly setups: ReadonlyMap<string, TenantSetup>;
    readonly complete: boolean;
}

/**
 * The latest provisioning run for each tenant it acts on, keyed by tenant id.
 *
 * A tenant may have more than one run, because nothing stops a second attempt.
 * The engine answers the run that changed last first, so the first run seen for
 * a tenant is the one that moved most recently, and that is the one reported.
 */
export async function readTenantSetups(caller: AuthenticatedCaller): Promise<TenantSetups> {
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
