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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
export interface CompleteTenantProvisioningCommand {}

export interface CompleteTenantProvisioningResponse {
    success: boolean;
    message: string;
}

/**
 * @brief Provisions a tenant from a seed profile.
 *
 * The tenant and its administrator are created before the answer; the steps
 * the profile orders run afterwards as a workflow instance, whose id the
 * answer carries. This is the one verb for every tenant, the first one
 * included, and it replaces iam.v1.bootstrap.provision-tenant and
 * iam.v1.tenants.provision-acme.
 */
export interface ProvisionTenantCommand {
    /**
     * @brief Code of the seed profile to provision from.
     *
     * The profile states the tenant's type, the steps to run and the parameters
     * this request must supply. An unknown code is refused.
     */
    profile_code: string;
    /**
     * @brief Unique code of the tenant to create.
     */
    tenant_code: string;
    /**
     * @brief Display name of the tenant.
     */
    tenant_name: string;
    /**
     * @brief Hostname of the tenant.
     *
     * A principal of the form username@hostname resolves its tenant through this
     * value.
     */
    tenant_hostname: string;
    /**
     * @brief Optional description for the tenant record.
     */
    tenant_description: string;
    /**
     * @brief Username of the tenant's first administrator, without a hostname
     * part.
     */
    admin_username: string;
    /**
     * @brief Email address of that administrator.
     */
    admin_email: string;
    /**
     * @brief Initial password of that administrator.
     *
     * A client that reuses the caller's own password, as a demonstration
     * profile's form does, sends it here: a server cannot read the password its
     * caller signed in with.
     */
    admin_password: string;
    /**
     * @brief Values for the profile's declared parameters, one "name=value" entry
     * each.
     *
     * A parameter the profile does not declare is refused, and so is a value the
     * parameter's data type or its choices refuse. A parameter this list omits
     * takes the profile's default; one that is required, omitted and declares no
     * default is refused, and so is a required parameter whose value is empty.
     */
    parameters: string[];
}

/**
 * @brief The answer to a provision request.
 *
 * The name carries the verb because @c iam.v1.bootstrap.provision-tenant still
 * declares a @c provision_tenant_response of its own, and both verbs answer
 * until the shell's provisioning moves to this one.
 */
export interface ProvisionTenantCommandResponse {
    success: boolean;
    message: string;
    /**
     * @brief Id of the workflow instance that runs the profile's steps.
     *
     * The caller follows the run, its progress and its retries by this id.
     */
    instance_id: string;
    /**
     * @brief Id of the tenant the request created.
     */
    tenant_id: string;
    /**
     * @brief Id of the administrator account it created.
     */
    account_id: string;
}

// --- Acme one-click tenant provisioning (--source acme) ---
//
// A single server-side orchestrated request: imports the four-party Acme
// Bank LEI hierarchy, publishes real GLEIF counterparties (small), then
// for each operating company publishes its business units, portfolios,
// books, accounts, and account contact informations. No repeated
// per-party logins, no orchestration logic client-side -- driven by
// internal actor impersonation through the real handler pipeline, see
// ores.iam.core/messaging/tenant_provisioning_handler.hpp's provision_acme.
//
// The answer takes minutes, not the seconds the transport allows by
// default: the handler waits up to 1500 seconds on the base bundle it
// publishes. The budget this message declares is 1800 seconds --
// deliberately larger than that wait, so the caller keeps waiting while
// the handler is still working. At the transport default the caller gives
// up first and reports a timeout for a run that had not finished.
export interface ProvisionAcmeTenantCommand {}

export interface ProvisionAcmeTenantStep {
    step: string;
    action: string;
    record_count: number;
}

export interface ProvisionAcmeTenantResponse {
    success: boolean;
    message: string;
    steps: ProvisionAcmeTenantStep[];
}

export const subjects = {
    complete_tenant_provisioning_command: 'iam.v1.tenants.complete-provisioning',
    provision_tenant_command: 'iam.v1.tenants.provision',
    provision_acme_tenant_command: 'iam.v1.tenants.provision-acme',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    complete_tenant_provisioning_command: true,
    provision_tenant_command: true,
    provision_acme_tenant_command: true,
} as const;
