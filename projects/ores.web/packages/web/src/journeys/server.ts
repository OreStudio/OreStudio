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
    ProvisionTenantRequest,
    ProvisionTenantResult,
    RetryWorkflowInstanceResult,
    LeiEntityChoice,
    SeedProfileChoice,
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
    readonly signOut: () => Promise<void>;
    readonly passwordPolicy: () => Promise<PasswordPolicy>;
    readonly seedProfiles: () => Promise<readonly SeedProfileChoice[]>;
    readonly leiEntities: () => Promise<readonly LeiEntityChoice[]>;
    readonly provision: (request: ProvisionTenantRequest) => Promise<ProvisionTenantResult>;
    readonly progress: (instanceId: string) => Promise<WorkflowProgress>;
    readonly retry: (instanceId: string, stepName?: string) => Promise<RetryWorkflowInstanceResult>;
    readonly changePassword: (currentPassword: string, nextPassword: string) => Promise<void>;
}

/** The deployment's own server, as a journey reaches it. */
export function useJourneyServer(): JourneyServer {
    const { recheck } = useBootstrap();
    const { signIn, chooseParty, signOut } = useSession();

    return useMemo<JourneyServer>(
        () => ({
            createAdministrator: async (request) => {
                await api.createAdministrator(request);
            },
            recheckBootstrap: recheck,
            signIn,
            chooseParty,
            signOut,
            passwordPolicy: api.passwordPolicy,
            seedProfiles: api.seedProfiles,
            leiEntities: api.leiEntities,
            provision: api.provisionTenant,
            progress: api.provisionTenantProgress,
            retry: api.retryProvisionTenant,
            changePassword: api.changePassword,
        }),
        [recheck, signIn, chooseParty, signOut],
    );
}
