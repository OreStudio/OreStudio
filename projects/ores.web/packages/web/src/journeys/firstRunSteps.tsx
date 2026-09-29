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
 * The first run journey's rail.
 *
 * It is the three steps that belong to the first run alone — the welcome, the
 * administrator and the arrival — with the five tenant steps placed between
 * them. Building the list here rather than inside the page is what makes the
 * rail assertable without a browser: the list is data, and the page is the
 * state that fills it in.
 */

import type { ReactNode } from 'react';
import { FirstSignIn, type TenantEntry } from './FirstSignIn.js';
import { newTenantSteps } from './newTenantSteps.js';
import type { JourneyStep, StepId } from './runtime.js';
import type { JourneyServer } from './server.js';
import type { NewTenant } from './state.js';
import type { Translator } from '../i18n/translate.js';
import type { PasswordPolicy, SeedProfileChoice } from '@ores/wire-protocol/browser';

/** What the person typed for the installation's first account. */
export interface AdministratorDraft {
    readonly principal: string;
    readonly email: string;
    readonly password: string;
    /** Whether the password meets the policy the deployment stated. */
    readonly acceptable: boolean;
}

export interface FirstRunStepsInput {
    readonly t: Translator['t'];
    readonly server: JourneyServer;
    readonly policy: PasswordPolicy;
    readonly profiles: readonly SeedProfileChoice[];
    readonly tenant: NewTenant;
    readonly administrator: AdministratorDraft;
    /** The password typed when the administrator was created. */
    readonly creatingPassword: string;
    readonly entry: TenantEntry | undefined;
    /** Whether the tenant administrator's first sign-in has finished. */
    readonly tenantSignInComplete: boolean;
    /** Called when that sign-in finishes, so the footer's action can open. */
    readonly onTenantSignInComplete: () => void;
    /** The splash, and what the three stages of the journey are. */
    readonly welcome: ReactNode;
    /** The form that describes the installation's first account. */
    readonly administratorForm: ReactNode;
    readonly goTo: (id: StepId) => void;
    /**
     * Runs once the administrator exists and the browser is signed in as it:
     * the page keeps the password a profile may reuse and reads the starting
     * points, which need that session.
     */
    readonly onAdministratorSignedIn: () => Promise<void>;
    readonly onHandOff: (continueAsAdmin: boolean) => Promise<void>;
    readonly onFinished: () => void;
}

export function firstRunSteps(input: FirstRunStepsInput): readonly JourneyStep<ReactNode>[] {
    const { t, server, policy, administrator, entry } = input;

    return [
        {
            id: 'welcome',
            title: t('journey.welcome.title'),
            lead: t('journey.welcome.lead'),
            body: input.welcome,
            next: { label: t('journey.welcome.start'), enabled: true },
        },
        {
            id: 'administrator',
            title: t('journey.admin.title'),
            lead: t('journey.admin.lead'),
            body: input.administratorForm,
            next: {
                label: t('journey.admin.create'),
                enabled:
                    administrator.principal !== '' &&
                    administrator.email !== '' &&
                    administrator.acceptable,
                run: async () => {
                    await server.createAdministrator({
                        principal: administrator.principal,
                        password: administrator.password,
                        email: administrator.email,
                    });
                    /*
                     * The deployment leaves bootstrap mode when that succeeds,
                     * and the journey stays on its rail because it said it had
                     * started. Then it signs in as the account it just made,
                     * because the starting points and the provisioning request
                     * belong to an account.
                     */
                    await server.recheckBootstrap();
                    const outcome = await server.signIn({
                        username: administrator.principal,
                        password: administrator.password,
                    });
                    if (outcome.outcome === 'party-required') {
                        const only = outcome.parties.length === 1 ? outcome.parties[0] : undefined;
                        if (only === undefined) {
                            throw new Error(t('journey.admin.partyChoice'));
                        }
                        await server.chooseParty(only.id, outcome.parties);
                    }
                    await input.onAdministratorSignedIn();
                },
            },
        },
        ...newTenantSteps({
            t,
            server,
            policy,
            profiles: input.profiles,
            state: input.tenant,
            creatingPassword: input.creatingPassword,
            onHandOff: input.onHandOff,
        }),
        {
            id: 'signIn',
            title: t('journey.signIn.title'),
            lead: t('journey.signIn.lead'),
            final: true,
            body:
                entry !== undefined ? (
                    <FirstSignIn
                        server={server}
                        policy={policy}
                        entry={entry}
                        onReady={input.onTenantSignInComplete}
                    />
                ) : null,
            next: {
                label: t('common.continue'),
                enabled: input.tenantSignInComplete,
                run: async () => input.goTo('ready'),
            },
        },
        {
            id: 'ready',
            title: t('journey.ready.title'),
            /*
             * The confirmation is the step's own sentence, so it is the lead.
             * A notice repeating it under the same title was the same statement
             * three times on one screen.
             */
            lead: t('journey.ready.lead', { principal: entry?.principal ?? '' }),
            body: null,
            next: {
                label: t('journey.ready.home'),
                enabled: true,
                run: async () => input.onFinished(),
            },
        },
    ];
}
