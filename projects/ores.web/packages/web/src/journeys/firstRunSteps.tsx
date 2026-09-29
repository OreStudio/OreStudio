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
    /** Whether the deployment already has its administrator. */
    readonly administratorExists: boolean;
    /** The form that describes the installation's first account. */
    readonly administratorForm: ReactNode;
    /** The form that signs back in as an administrator the deployment holds. */
    readonly administratorSignIn: ReactNode;
    readonly goTo: (id: StepId) => void;
    /** Creates the administrator and enters the deployment as it. */
    readonly onCreateAdministrator: () => Promise<void>;
    /** Signs in as the administrator the deployment already has. */
    readonly onAdministratorEntered: () => Promise<void>;
    readonly onHandOff: (continueAsAdmin: boolean) => Promise<void>;
    readonly onFinished: () => void;
}

export function firstRunSteps(input: FirstRunStepsInput): readonly JourneyStep<ReactNode>[] {
    const { t, server, policy, administrator, entry } = input;
    const known = input.administratorExists;

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
