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
 * The first run journey: the rail, and the state that fills it in.
 *
 * An installation with no administrator and no tenant is brought to life on one
 * rail: the administrator is created, the first tenant's steps run inline, its
 * administrator takes over, signs in and the screen reads *Ready*. The five
 * tenant steps are the shared library, so the new tenant journey runs the same
 * ones rather than a copy of them.
 *
 * Two things the journey does are worth stating where they happen. The browser
 * signs in as the administrator it just created, because the starting-point
 * read and the provisioning request belong to an account; and it holds that
 * administrator's password in memory for no longer than the journey, because a
 * profile may hand the password to the tenant's administrator and nothing may
 * write it down.
 */

import { useEffect, useRef, useState, type FormEvent, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Field, Input, Notice } from '../ui/Primitives.js';
import { NewPasswordField } from '../ui/PasswordField.js';
import { heroSplash } from '../assets/brand.js';
import { JourneyPage } from './JourneyPage.js';
import { indexOfStep } from './runtime.js';
import { firstRunSteps, type AdministratorDraft } from './firstRunSteps.js';
import type { TenantEntry } from './FirstSignIn.js';
import type { JourneyServer } from './server.js';
import { administratorPassword, tenantPrincipal, useNewTenant } from './state.js';
import type { PasswordPolicy, SeedProfileChoice } from '@ores/wire-protocol/browser';

/** What a fresh installation's administrator is almost always called. */
const DEFAULT_PRINCIPAL = 'super_admin';

/**
 * The address that account is stored under.
 *
 * `.ores` is not a real top-level domain, so nothing can be delivered to an
 * address nobody changed, and the convention is the one every other
 * system-scope account already uses. It is editable like the name.
 */
const DEFAULT_EMAIL = 'super_admin@system.ores';

function reasonOf(error: unknown): string {
    return error instanceof Error ? error.message : String(error);
}

/** The splash, and what the three stages of the journey are. */
function Welcome(): ReactNode {
    const { t } = useTranslation();
    const stages: readonly (readonly [string, string])[] = [
        ['journey.welcome.stage.admin', 'journey.welcome.stage.adminBody'],
        ['journey.welcome.stage.tenant', 'journey.welcome.stage.tenantBody'],
        ['journey.welcome.stage.signIn', 'journey.welcome.stage.signInBody'],
    ];
    return (
        <div>
            <img src={heroSplash} alt="" className="mb-6 w-full rounded-md border border-line" />
            <ul className="grid gap-3 sm:grid-cols-3">
                {stages.map(([title, body]) => (
                    <li key={title} className="rounded-md border border-line p-3">
                        <p className="text-sm font-medium">{t(title)}</p>
                        <p className="mt-1 text-sm text-ink-muted">{t(body)}</p>
                    </li>
                ))}
            </ul>
        </div>
    );
}

/**
 * The deployment's first account.
 *
 * It is created with no session, because there is nothing to sign in with yet,
 * and the server closes bootstrap mode when it succeeds. The password's rules
 * are the deployment's, read before anybody signed in.
 */
function AdministratorForm({
    policy,
    draft,
    onPrincipal,
    onEmail,
    onPassword,
}: {
    readonly policy: PasswordPolicy;
    readonly draft: AdministratorDraft;
    readonly onPrincipal: (value: string) => void;
    readonly onEmail: (value: string) => void;
    readonly onPassword: (value: string, acceptable: boolean) => void;
}): ReactNode {
    const { t } = useTranslation();
    const stop = (event: FormEvent): void => event.preventDefault();
    return (
        <form className="space-y-4" onSubmit={stop}>
            <p className="text-sm text-ink-muted">{t('journey.admin.bootstrap')}</p>
            <Field label={t('journey.admin.username')}>
                <Input
                    value={draft.principal}
                    autoComplete="username"
                    onChange={(event) => onPrincipal(event.target.value)}
                />
            </Field>
            <Field label={t('journey.admin.email')}>
                <Input
                    type="email"
                    value={draft.email}
                    autoComplete="email"
                    onChange={(event) => onEmail(event.target.value)}
                />
            </Field>
            <NewPasswordField
                policy={policy}
                label={t('journey.admin.password')}
                value={draft.password}
                onChange={onPassword}
            />
        </form>
    );
}

export interface FirstRunJourneyProps {
    readonly server: JourneyServer;
    /** Called once the journey owns the rail, so the route table stays on it. */
    readonly onStarted: () => void;
    /** Called when the person is done, so the route table hands the browser over. */
    readonly onFinished: () => void;
}

export function FirstRunJourney({
    server,
    onStarted,
    onFinished,
}: FirstRunJourneyProps): ReactNode {
    const { t } = useTranslation();
    const [policy, setPolicy] = useState<PasswordPolicy>();
    const [profiles, setProfiles] = useState<readonly SeedProfileChoice[]>([]);
    const [loadFailure, setLoadFailure] = useState<string>();
    const [attempt, setAttempt] = useState(0);
    const [draft, setDraft] = useState<AdministratorDraft>({
        principal: DEFAULT_PRINCIPAL,
        email: DEFAULT_EMAIL,
        password: '',
        acceptable: false,
    });
    const [creatingPassword, setCreatingPassword] = useState('');
    const [entry, setEntry] = useState<TenantEntry>();
    const [at, setAt] = useState(0);
    const tenant = useNewTenant();
    const started = useRef(false);

    /*
     * The policy is read before the journey starts, because the first step that
     * changes anything asks for a password. It is the deployment's policy, so a
     * deployment that tightens a rule tightens this screen.
     */
    useEffect(() => {
        let cancelled = false;
        void (async () => {
            try {
                const answer = await server.passwordPolicy();
                if (!cancelled) {
                    setPolicy(answer);
                    setLoadFailure(undefined);
                }
            } catch (error) {
                if (!cancelled) {
                    setLoadFailure(reasonOf(error));
                }
            }
        })();
        return () => {
            cancelled = true;
        };
    }, [server, attempt]);

    useEffect(() => {
        if (!started.current) {
            started.current = true;
            onStarted();
        }
    }, [onStarted]);

    const handOff = async (continueAsAdmin: boolean): Promise<void> => {
        if (tenant.profile === undefined || tenant.details === undefined) {
            return;
        }
        const tenantPassword = administratorPassword(tenant.details, creatingPassword);
        const tenantUser = tenantPrincipal(tenant.details);
        await server.signOut();
        if (!continueAsAdmin) {
            setEntry({ kind: 'sign-in', principal: tenantUser, password: '' });
        } else {
            const outcome = await server.signIn({
                username: tenantUser,
                password: tenantPassword,
            });
            setEntry(
                outcome.outcome === 'party-required'
                    ? {
                          kind: 'party',
                          principal: tenantUser,
                          password: tenantPassword,
                          parties: outcome.parties,
                          resetRequired: outcome.passwordResetRequired,
                      }
                    : {
                          kind: 'active',
                          principal: tenantUser,
                          password: tenantPassword,
                          resetRequired: outcome.passwordResetRequired,
                      },
            );
        }
        setAt(indexOfStep(steps, 'signIn'));
    };

    if (policy === undefined) {
        return (
            <div className="card p-6">
                {loadFailure === undefined ? (
                    <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                ) : (
                    <>
                        <Notice tone="error">
                            {t('journey.policyFailed', { message: loadFailure })}
                        </Notice>
                        <div className="mt-4 flex justify-end">
                            <Button
                                variant="secondary"
                                onClick={() => setAttempt((value) => value + 1)}
                            >
                                {t('gate.retry')}
                            </Button>
                        </div>
                    </>
                )}
            </div>
        );
    }

    const steps = firstRunSteps({
        t,
        server,
        policy,
        profiles,
        tenant,
        administrator: draft,
        creatingPassword,
        entry,
        welcome: <Welcome />,
        administratorForm: (
            <AdministratorForm
                policy={policy}
                draft={draft}
                onPrincipal={(principal) => setDraft((current) => ({ ...current, principal }))}
                onEmail={(email) => setDraft((current) => ({ ...current, email }))}
                onPassword={(password, acceptable) =>
                    setDraft((current) => ({ ...current, password, acceptable }))
                }
            />
        ),
        goTo: (id) => setAt(indexOfStep(steps, id)),
        onAdministratorSignedIn: async () => {
            setCreatingPassword(draft.password);
            setProfiles(await server.seedProfiles());
        },
        onHandOff: handOff,
        onFinished: () => onFinished(),
    });

    return <JourneyPage steps={steps} at={at} onMove={setAt} />;
}
