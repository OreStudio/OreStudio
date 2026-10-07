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
/**
 * @brief Provisions a tenant from a seed profile.
 *
 * The tenant and its administrator are created before the answer; the steps
 * the profile orders run afterwards as a workflow instance, whose id the
 * answer carries. This is the one verb for every tenant, the first one
 * included, and it replaces iam.v1.bootstrap.provision-tenant and
 * iam.v1.ops.provision_tenant-acme.
 *
 * The answer takes longer than the transport's default request timeout,
 * because creating the tenant copies the deployment's registered data into it
 * -- its roles, its permissions and its lookup tables -- before the steps are
 * dispatched. The budget is the one the bootstrap verb this replaces declared.
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
 * The tenant and its administrator exist by the time this answers, and the steps
 * the starting point orders run afterwards, so the answer carries the run's id
 * rather than waiting for work that takes minutes.
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

/**
 * @brief Provisions one party of the caller's own tenant.
 *
 * The tenant exists already: this request provisions one of its parties, the
 * one an administrator names. The party stage is one request because both
 * clients drive it and neither is the reference — the browser when a person
 * adds a party, the shell when a script does — and the work runs as a workflow
 * instance, whose id the answer carries, because publishing a party's data
 * takes minutes and can fail half way.
 */
export interface ProvisionPartyCommand {
    /**
     * @brief The party to provision, by its identifier or its exact full name.
     *
     * A person types the name they know the party by and a script states the
     * identifier it read; the read that resolves either is the party read, so
     * neither spelling is privileged.
     */
    party: string;
    /**
     * @brief Code of the seed profile whose party stage the request runs.
     *
     * A party's data is the starting point's and the starting point states it as
     * a row: the bundles a party is published from are the profile's
     * "provision_party" step arguments. An unknown code is refused, and so is a
     * profile that orders no party step.
     *
     * An empty code means the deployment's own party starting point, which is the
     * first profile that orders a party step. A tenant administrator cannot name a
     * profile: the profiles are the system tenant's rows, and a tenant reads only
     * its own, so the service that holds both is the one that chooses.
     */
    profile_code: string;
    /**
     * @brief The legal entity the party was built from, or empty for one that is
     * not in the register.
     *
     * A person who adds a party either finds the legal entity among those the
     * deployment holds or names the party by hand, and the LEI is what tells the
     * two apart: the register's identifier of the entity the party is, or nothing
     * at all.
     *
     * The run records it, because a party identifier carries the party the writing
     * session acts in and the person who adds a party works in another one. An
     * empty value leaves the party without an LEI, which is the party a person
     * described by hand.
     */
    lei: string;
}

export interface ProvisionPartyCommandResponse {
    success: boolean;
    message: string;
    /**
     * @brief Id of the workflow instance that runs the party stage.
     *
     * The caller follows the run, its progress and its retries by this id.
     */
    instance_id: string;
    /**
     * @brief Id of the party the request resolved.
     */
    party_id: string;
}

export const subjects = {
    provision_tenant_command: 'iam.v1.ops.provision_tenant',
    provision_party_command: 'iam.v1.ops.provision_party',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    provision_tenant_command: true,
    provision_party_command: true,
} as const;
