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
 * The steps every tenant journey runs.
 *
 * A tenant is chosen, described, reviewed and provisioned, and both the first
 * run and the new tenant journey do those four things. They are written once
 * here and placed on each rail, so a change to the way a tenant is described
 * reaches both journeys; the first run inlines them rather than nesting a
 * journey, so the person sees one rail. A first run that keeps the system
 * tenant alone runs only the first of them, because it creates no tenant to
 * describe.
 *
 * A journey that owns the browser when the run ends appends its own finish
 * step, which signs the administrator out: the tenant's administrator finishes
 * the tenant's own setup when they first sign in, so nobody stays in the tenant
 * the run just made. The first run does not pass one, because the deployment's
 * own rail has its own final step.
 *
 * The steps are data, which is why this is a function and not a component: the
 * page joins this list with its own steps and hands the result to the runtime.
 */

import type { ReactNode } from 'react';
import { isProfileTaken, RunProgress, ProfileCards, TenantForm, TenantSummary } from './parts.js';
import type { JourneyStep } from './runtime.js';
import type { JourneyServer } from './server.js';
import { provisionRequest, type NewTenant } from './state.js';
import type { Translator } from '../i18n/translate.js';
import type { PasswordPolicy, SeedProfileChoice } from '@ores/wire-protocol/browser';

export interface NewTenantStepsInput {
    readonly t: Translator['t'];
    readonly server: JourneyServer;
    readonly policy: PasswordPolicy;
    readonly profiles: readonly SeedProfileChoice[];
    readonly state: NewTenant;
    /** The password the creating administrator typed, which a profile may reuse. */
    readonly creatingPassword: string;
    /**
     * Ends the journey, where the journey owns the browser at the run's end.
     *
     * Only the new tenant journey passes this. The step signs the creating
     * administrator out, because the tenant's own administrator finishes the
     * tenant's setup when they first sign in. A first run leaves it out: its
     * own final step is the one that ends the deployment's rail.
     */
    readonly onFinished?: () => Promise<void>;
    /**
     * The tenant codes the deployment already holds. A first run holds none,
     * so it passes nothing.
     */
    readonly takenCodes?: ReadonlySet<string>;
    /**
     * The starting point a first run may answer with no tenant at all.
     *
     * Only a first run passes this. The tenant journey is somebody adding a
     * tenant to a deployment that has one, so a starting point that creates
     * none is not a thing it can offer.
     */
    readonly startingPoint?: StartingPointChoice;
}

/**
 * What a first run chooses at the starting point.
 *
 * The two answers are one choice: a profile means the installation gains a
 * tenant, and no tenant means it keeps the system tenant alone. Each answer
 * undoes the other, so the cards cannot both read as chosen.
 */
export interface StartingPointChoice {
    /** Whether keeping the system tenant alone is what the person chose. */
    readonly noTenant: boolean;
    /** Records that a profile was chosen, which means a tenant will be created. */
    readonly chooseTenant: () => void;
    /** Records that no tenant was chosen. */
    readonly chooseNoTenant: () => void;
}

export function newTenantSteps(input: NewTenantStepsInput): readonly JourneyStep<ReactNode>[] {
    const { t, server, policy, profiles, state, creatingPassword } = input;
    const takenCodes = input.takenCodes ?? new Set<string>();
    const profile = state.profile;
    const details = state.details;
    const startingPoint = input.startingPoint;
    const noTenant = startingPoint?.noTenant === true;

    const passwordReady =
        details === undefined
            ? false
            : details.useMyPassword
              ? creatingPassword !== ''
              : state.passwordAcceptable;

    /*
     * The four stages that build the tenant. A first run that keeps the system
     * tenant alone stops at the starting point, because there is no tenant to
     * describe, review or provision.
     */
    const tenantStages: readonly JourneyStep<ReactNode>[] = noTenant
        ? []
        : [
              {
                  id: 'details',
                  title: t('journey.details.title'),
                  lead: t('journey.details.lead'),
                  body:
                      profile !== undefined && details !== undefined ? (
                          <TenantForm
                              server={server}
                              profile={profile}
                              details={details}
                              policy={policy}
                              creatingPassword={creatingPassword}
                              onChange={state.describe}
                              onPasswordAcceptable={state.acceptPassword}
                          />
                      ) : null,
                  next: { label: t('common.continue'), enabled: passwordReady },
              },
              {
                  id: 'review',
                  title: t('journey.review.title'),
                  lead: t('journey.review.lead'),
                  body:
                      profile !== undefined && details !== undefined ? (
                          <TenantSummary
                              profile={profile}
                              details={details}
                              creatingPassword={creatingPassword}
                          />
                      ) : null,
                  next: {
                      label: t('journey.review.create'),
                      enabled: profile !== undefined && details !== undefined,
                      run: async () => {
                          if (profile === undefined || details === undefined) {
                              return;
                          }
                          const result = await server.provision(
                              provisionRequest(profile, details, creatingPassword),
                          );
                          if (!result.success) {
                              throw new Error(result.message);
                          }
                          if (result.instanceId === '') {
                              throw new Error(t('journey.review.noRun'));
                          }
                          state.recordRun(result.instanceId);
                      },
                  },
              },
              {
                  id: 'provisioning',
                  title: t('journey.provisioning.title'),
                  lead: t('journey.provisioning.lead'),
                  final: true,
                  body:
                      state.instanceId !== undefined ? (
                          <RunProgress
                              server={server}
                              instanceId={state.instanceId}
                              onCompleted={state.recordRunComplete}
                          />
                      ) : null,
                  next: {
                      label: t('common.continue'),
                      enabled: state.runComplete,
                  },
              },
          ];

    /*
     * The finish step is the journey's own ending, and only a journey that
     * owns the browser passes one. Its action is the sign-out: the tenant's
     * administrator is the one who finishes the tenant's setup, when they
     * first sign in.
     */
    const finish: readonly JourneyStep<ReactNode>[] =
        input.onFinished === undefined
            ? []
            : [
                  {
                      id: 'finish',
                      title: t('journey.finish.title'),
                      lead: t('journey.finish.lead'),
                      final: true,
                      body: null,
                      next: {
                          label: t('journey.finish.signOut'),
                          enabled: true,
                          run: async () => input.onFinished?.(),
                      },
                  },
              ];

    return [
        {
            id: 'profile',
            title: t('journey.profile.title'),
            lead: t('journey.profile.lead'),
            body: (
                <ProfileCards
                    profiles={profiles}
                    selected={noTenant ? undefined : profile?.code}
                    onSelect={(chosen) => {
                        startingPoint?.chooseTenant();
                        state.chooseProfile(chosen);
                    }}
                    takenCodes={takenCodes}
                    {...(startingPoint !== undefined && {
                        noTenant: {
                            selected: noTenant,
                            onSelect: startingPoint.chooseNoTenant,
                        },
                    })}
                />
            ),
            next: {
                label: t('common.continue'),
                enabled:
                    noTenant || (profile !== undefined && !isProfileTaken(profile, takenCodes)),
            },
        },
        ...tenantStages,
        ...finish,
    ];
}
