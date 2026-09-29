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
 * The new tenant steps' state, and what a tenant request is built from.
 *
 * The state a tenant is described by belongs to the journey and not to a
 * screen, because the rail moves between screens while the description stays
 * put: the profile chosen on one step is the form on the next and the summary
 * after it. The functions below are the only place that state becomes a
 * request, so the form and the request cannot disagree about a field.
 *
 * `useNewTenant` reads the starting points it was given rather than fetching
 * them, so the same hook serves the first run and the new tenant journey and a
 * test can hand it whatever profiles it wants to describe.
 */

import { useState } from 'react';
import type { ProvisionTenantRequest, SeedProfileChoice } from '@ores/wire-protocol/browser';

/** A tenant as the person described it, before anything was created. */
export interface TenantDetails {
    readonly name: string;
    readonly code: string;
    readonly hostname: string;
    readonly adminUsername: string;
    readonly adminEmail: string;
    readonly adminPassword: string;
    /**
     * Whether the tenant administrator takes the creating administrator's
     * password.
     *
     * It is the profile's choice, because a demo profile's administrator is
     * the person who created it and nothing is gained by typing a second
     * password for them.
     */
    readonly useMyPassword: boolean;
    /** The profile's own settings, keyed by the names it declared. */
    readonly parameters: Readonly<Record<string, string>>;
}

/** What a profile proposes before anybody types. */
export function detailsFor(profile: SeedProfileChoice): TenantDetails {
    return {
        name: profile.tenant.name,
        code: profile.tenant.code,
        hostname: profile.tenant.hostname,
        adminUsername: profile.tenant.adminUsername,
        adminEmail: profile.tenant.adminEmail,
        adminPassword: '',
        useMyPassword: profile.inheritsAdminPassword,
        parameters: Object.fromEntries(
            profile.parameters.map((parameter) => [parameter.name, parameter.defaultValue]),
        ),
    };
}

/**
 * The password the tenant's administrator will sign in with.
 *
 * A profile that inherits the password needs one the journey already holds:
 * the creating administrator typed it one step earlier.
 */
export function administratorPassword(details: TenantDetails, creatingPassword: string): string {
    return details.useMyPassword ? creatingPassword : details.adminPassword;
}

/** The request one described tenant becomes. */
export function provisionRequest(
    profile: SeedProfileChoice,
    details: TenantDetails,
    creatingPassword: string,
): ProvisionTenantRequest {
    return {
        profileCode: profile.code,
        tenantCode: details.code,
        tenantName: details.name,
        tenantHostname: details.hostname,
        tenantDescription: '',
        adminUsername: details.adminUsername,
        adminEmail: details.adminEmail,
        adminPassword: administratorPassword(details, creatingPassword),
        parameters: { ...details.parameters },
    };
}

/**
 * The principal a tenant administrator signs in with.
 *
 * A principal names the hostname a request routes to, and an account is stored
 * under its username, so the two travel together: the hostname here is the one
 * the profile set on the tenant, which is what the deployment serves it on.
 */
export function tenantPrincipal(details: TenantDetails): string {
    return details.hostname === ''
        ? details.adminUsername
        : `${details.adminUsername}@${details.hostname}`;
}

export interface NewTenant {
    readonly profile: SeedProfileChoice | undefined;
    readonly details: TenantDetails | undefined;
    /** Whether the password typed for the tenant administrator may be sent. */
    readonly passwordAcceptable: boolean;
    readonly instanceId: string | undefined;
    /** Whether the run has reached its end, so the person may move on. */
    readonly runComplete: boolean;
    chooseProfile(profile: SeedProfileChoice): void;
    describe(details: TenantDetails): void;
    acceptPassword(acceptable: boolean): void;
    recordRun(instanceId: string): void;
    recordRunComplete(): void;
}

export function useNewTenant(): NewTenant {
    const [profile, setProfile] = useState<SeedProfileChoice>();
    const [details, setDetails] = useState<TenantDetails>();
    const [passwordAcceptable, setPasswordAcceptable] = useState(false);
    const [instanceId, setInstanceId] = useState<string>();
    const [runComplete, setRunComplete] = useState(false);

    return {
        profile,
        details,
        passwordAcceptable,
        instanceId,
        runComplete,
        chooseProfile: (chosen) => {
            setProfile(chosen);
            setDetails(detailsFor(chosen));
            // The new profile's password is its own, so the last one's verdict
            // does not carry over to it.
            setPasswordAcceptable(false);
        },
        describe: setDetails,
        acceptPassword: setPasswordAcceptable,
        recordRun: setInstanceId,
        recordRunComplete: () => setRunComplete(true),
    };
}
