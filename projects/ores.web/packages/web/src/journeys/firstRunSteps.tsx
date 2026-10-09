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
 * It is the steps that belong to the first run alone — the welcome, the
 * administrator and the arrival — with the five tenant steps placed between
 * them. Building the list here rather than inside the page is what makes the
 * rail assertable without a browser: the list is data, and the page is the
 * state that fills it in.
 *
 * The person chooses on the starting point whether the installation ends with
 * a tenant of its own. That choice decides whether the four tenant stages
 * follow the starting point at all, so it is an input here rather than a flag
 * read somewhere else: the rail that is built is the rail the person was
 * promised.
 */

import type { ReactNode } from 'react';
import { FirstSignIn, type TenantEntry } from './FirstSignIn.js';
import { newTenantSteps } from './newTenantSteps.js';
import { defineJourney, type JourneyStep, type StepId } from './runtime.js';
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

/**
 * What the installation is left with when the journey finishes.
 *
 * `first-tenant` is the journey's ordinary ending: the administrator, a tenant
 * built from a starting point, and the tenant administrator's first sign-in.
 * `system-only` creates no tenant, so the installation keeps the system tenant
 * alone, which is the state a test installation wants.
 */
export type FirstRunChoice = 'first-tenant' | 'system-only';

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
    /** Whether the installation ends with a tenant of its own, or without one. */
    readonly choice: FirstRunChoice;
    /** Records the starting point the person chose on the profile step. */
    readonly onChoose: (choice: FirstRunChoice) => void;
    /** The introduction the welcome step shows. */
    readonly welcome: ReactNode;
    /** Whether the deployment already has its administrator. */
    readonly administratorExists: boolean;
    /** The form that describes the installation's first account. */
    readonly administratorForm: ReactNode;
    /** The form that signs back in as an administrator the deployment holds. */
    readonly administratorSignIn: ReactNode;
    /**
     * What the system-only rail's sign-in shows above its action.
     *
     * No tenant administrator exists there, so the account that signs in is the
     * administrator the journey just created.
     */
    readonly administratorArrival: ReactNode;
    readonly goTo: (id: StepId) => void;
    /** Creates the administrator and enters the deployment as it. */
    readonly onCreateAdministrator: () => Promise<void>;
    /** Signs in as the administrator the deployment already has. */
    readonly onAdministratorEntered: () => Promise<void>;
    /** Signs in as the administrator the journey created, on a system-only run. */
    readonly onAdministratorSignIn: () => Promise<void>;
    readonly onHandOff: (continueAsAdmin: boolean) => Promise<void>;
    /**
     * Records that the wizard finished, so the deployment can leave its setup
     * screen even when it keeps no tenant of its own.
     */
    readonly onCompleteSystemOnboarding: () => Promise<void>;
    /**
     * Ends a bootstrap: signs the browser out and hands it the sign-in screen.
     *
     * Bootstrap runs as the system party because the settings it writes are
     * tenant-wide, which is a party nobody should be left sitting in. Both
     * rails end here, the tenant one included, so the installation's own
     * administrator signs in again as itself once the deployment is set up.
     */
    readonly onSignOutAfterBootstrap: () => Promise<void>;
}

export function firstRunSteps(input: FirstRunStepsInput): readonly JourneyStep<ReactNode>[] {
    const { t, server, policy, administrator, entry, choice } = input;
    const known = input.administratorExists;
    /*
     * The starting point offers the profiles and the installation that keeps
     * no tenant, and what it chooses is the rail: a profile runs the four
     * tenant stages after it, and no tenant goes straight to the sign-in. The
     * step is on both rails because it is where the choice is made.
     */
    const tenantSteps = newTenantSteps({
        t,
        server,
        policy,
        profiles: input.profiles,
        state: input.tenant,
        creatingPassword: input.creatingPassword,
        onHandOff: input.onHandOff,
        startingPoint: {
            noTenant: choice === 'system-only',
            chooseTenant: () => input.onChoose('first-tenant'),
            chooseNoTenant: () => input.onChoose('system-only'),
        },
    });
    /*
     * The arrival names the account that is signed in when the journey ends:
     * the tenant's administrator on the tenant rail, and the administrator the
     * journey created on the system-only one.
     */
    const principal =
        choice === 'first-tenant' ? (entry?.principal ?? '') : administrator.principal;

    return defineJourney([
        {
            id: 'welcome',
            title: t('journey.welcome.title'),
            lead: t('journey.welcome.lead'),
            body: input.welcome,
            next: { label: t('journey.welcome.start'), enabled: true },
        },
        {
            id: 'administrator',
            title: t(known ? 'journey.admin.resumeTitle' : 'journey.admin.title'),
            lead: t(known ? 'journey.admin.resumeLead' : 'journey.admin.lead'),
            body: known ? input.administratorSignIn : input.administratorForm,
            next: known
                ? {
                      label: t('journey.admin.signIn'),
                      /*
                       * Only the two fields matter here. The password exists
                       * and met whatever rules it met when it was set, so the
                       * deployment's policy is not applied to it a second
                       * time: a rule tightened since would lock somebody out
                       * of the installation they own.
                       */
                      enabled: administrator.principal !== '' && administrator.password !== '',
                      run: async () => input.onAdministratorEntered(),
                  }
                : {
                      label: t('journey.admin.create'),
                      enabled:
                          administrator.principal !== '' &&
                          administrator.email !== '' &&
                          administrator.acceptable,
                      run: async () => input.onCreateAdministrator(),
                  },
        },
        ...tenantSteps,
        {
            id: 'signIn',
            title: t('journey.signIn.title'),
            lead:
                choice === 'first-tenant'
                    ? t('journey.signIn.lead')
                    : t('journey.signIn.systemLead'),
            final: true,
            body:
                choice === 'first-tenant' ? (
                    entry !== undefined ? (
                        <FirstSignIn
                            server={server}
                            policy={policy}
                            entry={entry}
                            onReady={input.onTenantSignInComplete}
                        />
                    ) : null
                ) : (
                    input.administratorArrival
                ),
            next:
                choice === 'first-tenant'
                    ? {
                          label: t('common.continue'),
                          enabled: input.tenantSignInComplete,
                          run: async () => input.goTo('ready'),
                      }
                    : {
                          label: t('common.continue'),
                          enabled: true,
                          run: async () => input.onAdministratorSignIn(),
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
            lead: t('journey.ready.lead', { principal }),
            body: null,
            next: {
                label: t('journey.ready.home'),
                enabled: true,
                run: async () => {
                    // The flag is what releases the gate on an installation
                    // that kept the system tenant alone, so it is written
                    // before the browser is handed over, on both rails.
                    await input.onCompleteSystemOnboarding();
                    await input.onSignOutAfterBootstrap();
                },
            },
        },
    ]);
}
