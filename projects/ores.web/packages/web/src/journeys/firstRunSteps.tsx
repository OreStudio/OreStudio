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
 * administrator and the final ready — with the tenant stages placed between
 * them. Building the list here rather than inside the page is what makes the
 * rail assertable without a browser: the list is data, and the page is the
 * state that fills it in.
 *
 * The person chooses on the starting point whether the installation ends with
 * a tenant of its own. That choice decides whether the tenant stages follow the
 * starting point at all, so it is an input here rather than a flag read
 * somewhere else: the rail that is built is the rail the person was promised.
 * The tenant's own setup is not a step here: the tenant owns that run, and its
 * administrator finishes it when they first sign in.
 */

import type { ReactNode } from 'react';
import { newTenantSteps } from './newTenantSteps.js';
import { defineJourney, type JourneyStep } from './runtime.js';
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
 * `first-tenant` is the journey's ordinary ending: the administrator and a
 * tenant built from a starting point, left bootstrapping for its own
 * administrator to finish. `system-only` creates no tenant, so the
 * installation keeps the system tenant alone, which is the state a test
 * installation wants.
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
    /** Creates the administrator and enters the deployment as it. */
    readonly onCreateAdministrator: () => Promise<void>;
    /** Signs in as the administrator the deployment already has. */
    readonly onAdministratorEntered: () => Promise<void>;
    /**
     * Records that the wizard finished, so the deployment can leave its setup
     * screen even when it keeps no tenant of its own.
     */
    readonly onCompleteSystemOnboarding: () => Promise<void>;
    /**
     * Hands the browser to the routes, which read the deployment again.
     *
     * The person is not signed out. They are the administrator this deployment
     * was just given, the browser is already signed in as them, and the party
     * the setup wrote from is the one their own sign-in lands in: asking them
     * for the credentials they have just typed, or created, is a second sign-in
     * for no second session.
     */
    readonly onFinished: () => void;
}

export function firstRunSteps(input: FirstRunStepsInput): readonly JourneyStep<ReactNode>[] {
    const { t, server, policy, administrator, choice } = input;
    const known = input.administratorExists;
    /*
     * The starting point offers the profiles and the installation that keeps
     * no tenant, and what it chooses is the rail: a profile runs the tenant
     * stages after it, and no tenant goes straight to the ready step. The step
     * is on both rails because it is where the choice is made.
     */
    const tenantSteps = newTenantSteps({
        t,
        server,
        policy,
        profiles: input.profiles,
        state: input.tenant,
        creatingPassword: input.creatingPassword,
        startingPoint: {
            noTenant: choice === 'system-only',
            chooseTenant: () => input.onChoose('first-tenant'),
            chooseNoTenant: () => input.onChoose('system-only'),
        },
    });

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
            id: 'ready',
            title: t('journey.ready.title'),
            /*
             * The confirmation is the step's own sentence, so it is the lead.
             * A notice repeating it under the same title was the same statement
             * three times on one screen.
             */
            lead: t('journey.ready.lead'),
            body: null,
            next: {
                label: t('journey.ready.home'),
                enabled: true,
                run: async () => {
                    // The flag is what releases the gate, so it is written
                    // before the browser is handed over.
                    await input.onCompleteSystemOnboarding();
                    input.onFinished();
                },
            },
        },
    ]);
}
