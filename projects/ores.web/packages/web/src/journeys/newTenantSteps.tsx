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
 * A tenant is chosen, described, reviewed, provisioned and handed over, and
 * both the first run and the new tenant journey do those five things. They are
 * written once here and placed on each rail, so a change to the way a tenant is
 * described reaches both journeys; the first run inlines them rather than
 * nesting a journey, so the person sees one rail.
 *
 * The steps are data, which is why this is a function and not a component: the
 * page joins this list with its own steps and hands the result to the runtime.
 */

import { useState, type ReactNode } from 'react';
import { Button, Notice } from '../ui/Primitives.js';
import { RunProgress, ProfileCards, TenantForm, TenantSummary } from './parts.js';
import type { JourneyStep } from './runtime.js';
import type { JourneyServer } from './server.js';
import { provisionRequest, tenantPrincipal, type NewTenant } from './state.js';
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
    /** Signs the creating administrator out, and into the tenant when asked. */
    readonly onHandOff: (continueAsAdmin: boolean) => Promise<void>;
}

/**
 * The hand-over, which is the last step the two journeys share.
 *
 * The person either becomes the tenant's administrator or passes the account
 * on; either way the creating administrator's session ends here, because the
 * next thing this browser does is sign in as somebody else.
 */
function HandOff({
    t,
    principal,
    mustChange,
    onContinue,
    onElsewhere,
}: {
    readonly t: Translator['t'];
    readonly principal: string;
    readonly mustChange: boolean;
    readonly onContinue: () => Promise<void>;
    readonly onElsewhere: () => Promise<void>;
}): ReactNode {
    const [busy, setBusy] = useState(false);
    const [failure, setFailure] = useState<string>();

    const choose = async (action: () => Promise<void>): Promise<void> => {
        setFailure(undefined);
        setBusy(true);
        try {
            await action();
        } catch (error) {
            setFailure(error instanceof Error ? error.message : String(error));
        } finally {
            setBusy(false);
        }
    };

    return (
        <div className="space-y-4">
            <p className="text-sm text-ink-muted">
                {t('journey.handOff.administrator', { principal })}
            </p>
            {failure !== undefined && <Notice tone="error">{failure}</Notice>}
            <div className="grid gap-3 sm:grid-cols-2">
                <button
                    type="button"
                    disabled={busy}
                    className="card p-4 text-left hover:border-accent"
                    onClick={() => void choose(onContinue)}
                >
                    <span className="font-semibold">{t('journey.handOff.continue')}</span>
                    <p className="mt-1 text-sm text-ink-muted">
                        {t('journey.handOff.continueHint', { principal })}
                    </p>
                </button>
                <button
                    type="button"
                    disabled={busy}
                    className="card p-4 text-left hover:border-line-strong"
                    onClick={() => void choose(onElsewhere)}
                >
                    <span className="font-semibold">{t('journey.handOff.elsewhere')}</span>
                    <p className="mt-1 text-sm text-ink-muted">
                        {mustChange
                            ? t('journey.handOff.elsewhereHintForced')
                            : t('journey.handOff.elsewhereHint')}
                    </p>
                </button>
            </div>
        </div>
    );
}

export function newTenantSteps(input: NewTenantStepsInput): readonly JourneyStep<ReactNode>[] {
    const { t, server, policy, profiles, state, creatingPassword } = input;
    const profile = state.profile;
    const details = state.details;

    const passwordReady =
        details === undefined
            ? false
            : details.useMyPassword
              ? creatingPassword !== ''
              : state.passwordAcceptable;

    return [
        {
            id: 'profile',
            title: t('journey.profile.title'),
            lead: t('journey.profile.lead'),
            body: (
                <ProfileCards
                    profiles={profiles}
                    selected={profile?.code}
                    onSelect={state.chooseProfile}
                />
            ),
            next: { label: t('common.continue'), enabled: profile !== undefined },
        },
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
        {
            id: 'handOff',
            title: t('journey.handOff.title'),
            lead: t('journey.handOff.lead'),
            final: true,
            body:
                details !== undefined ? (
                    <HandOff
                        t={t}
                        principal={tenantPrincipal(details)}
                        mustChange={profile?.forcePasswordChange ?? false}
                        onContinue={() => input.onHandOff(true)}
                        onElsewhere={() => input.onHandOff(false)}
                    />
                ) : null,
        },
    ];
}
