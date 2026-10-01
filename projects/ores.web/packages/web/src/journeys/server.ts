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

/**
 * Everything a journey reaches the server with, in one place.
 *
 * A journey is mostly screens, and the screens are mostly a function of the
 * answers these calls give. Naming them here rather than calling `api` from
 * inside a step is what lets the whole journey be rendered and walked in a
 * test: the test hands the page a server of its own and asserts what the
 * person sees, with no transport and no broker in the way.
 */

import { useMemo } from 'react';
import { api } from '../api/client.js';
import { useBootstrap } from '../session/BootstrapProvider.js';
import { useSession, type SignInOutcome } from '../session/SessionProvider.js';
import type {
    CreateAdministratorRequest,
    PasswordPolicy,
    PartySummary,
    ProvisionPartyRequest,
    ProvisionPartyResult,
    ProvisionTenantRequest,
    ProvisionTenantResult,
    RegistrationPolicyView,
    RetryWorkflowInstanceResult,
    LeiEntityChoice,
    SeedProfileChoice,
    SignupRequest,
    SignupResult,
    WorkflowProgress,
} from '@ores/wire-protocol/browser';

export interface JourneyServer {
    readonly createAdministrator: (request: CreateAdministratorRequest) => Promise<void>;
    /** Ask whether the deployment still needs an administrator, after creating one. */
    readonly recheckBootstrap: () => Promise<void>;
    readonly signIn: (credentials: {
        readonly username: string;
        readonly password: string;
    }) => Promise<SignInOutcome>;
    readonly chooseParty: (partyId: string, parties: readonly PartySummary[]) => Promise<void>;
    /** Re-scopes the open session to a party the account may work in. */
    readonly switchParty: (partyId: string) => Promise<void>;
    readonly signOut: () => Promise<void>;
    readonly passwordPolicy: () => Promise<PasswordPolicy>;
    /** What the deployment offers somebody who is not in it yet. */
    readonly registrationPolicy: () => Promise<RegistrationPolicyView>;
    /** Registers an account, and answers with the state it was created in. */
    readonly signup: (request: SignupRequest) => Promise<SignupResult>;
    readonly seedProfiles: () => Promise<readonly SeedProfileChoice[]>;
    /**
     * The tenant codes the deployment already holds. Only system
     * administration may read them, so only its journeys ask.
     */
    readonly tenantCodes: () => Promise<readonly string[]>;
    /** The entities matching what a person typed, which the read matches. */
    readonly leiEntities: (search: string) => Promise<readonly LeiEntityChoice[]>;
    readonly provision: (request: ProvisionTenantRequest) => Promise<ProvisionTenantResult>;
    readonly provisionParty: (request: ProvisionPartyRequest) => Promise<ProvisionPartyResult>;
    readonly progress: (instanceId: string) => Promise<WorkflowProgress>;
    readonly retry: (instanceId: string, stepName?: string) => Promise<RetryWorkflowInstanceResult>;
    readonly changePassword: (currentPassword: string, nextPassword: string) => Promise<void>;
}

/** The deployment's own server, as a journey reaches it. */
export function useJourneyServer(): JourneyServer {
    const { recheck } = useBootstrap();
    const { signIn, chooseParty, switchParty, signOut } = useSession();

    return useMemo<JourneyServer>(
        () => ({
            createAdministrator: async (request) => {
                await api.createAdministrator(request);
            },
            recheckBootstrap: recheck,
            signIn,
            chooseParty,
            switchParty,
            signOut,
            passwordPolicy: api.passwordPolicy,
            registrationPolicy: api.registrationPolicy,
            signup: api.signup,
            seedProfiles: api.seedProfiles,
            tenantCodes: async () => (await api.tenants()).tenants.map((tenant) => tenant.code),
            leiEntities: api.leiEntities,
            provision: api.provisionTenant,
            provisionParty: api.provisionParty,
            progress: api.provisionTenantProgress,
            retry: api.retryProvisionTenant,
            changePassword: api.changePassword,
        }),
        [recheck, signIn, chooseParty, switchParty, signOut],
    );
}
