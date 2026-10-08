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
 * rail. The person chooses at the start what the installation is left with: the
 * ordinary ending creates the first tenant, whose steps run inline, and its
 * administrator takes over and signs in; the other creates no tenant, so the
 * installation keeps the system tenant alone. The five tenant steps are the
 * shared library, so the new tenant journey runs the same ones rather than a
 * copy of them.
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
import { Button, Field, Input, Notice, cx } from '../ui/Primitives.js';
import { NewPasswordField, PasswordInput } from '../ui/PasswordField.js';
import { JourneyPage } from './JourneyPage.js';
import { JourneyHeader } from './parts.js';
import { indexOfStep } from './runtime.js';
import { firstRunSteps, type AdministratorDraft, type FirstRunChoice } from './firstRunSteps.js';
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

/**
 * The two endings the person chooses between, in the order they are offered.
 *
 * Creating the first tenant is the ordinary case, so it is stated first.
 */
const CHOICES: readonly FirstRunChoice[] = ['first-tenant', 'system-only'];

/** The stages the chosen rail runs, as a title key and a body key. */
export function welcomeStages(choice: FirstRunChoice): readonly (readonly [string, string])[] {
    if (choice === 'system-only') {
        return [
            ['journey.welcome.stage.admin', 'journey.welcome.stage.adminBody'],
            ['journey.welcome.stage.signIn', 'journey.welcome.stage.systemSignInBody'],
        ];
    }
    return [
        ['journey.welcome.stage.admin', 'journey.welcome.stage.adminBody'],
        ['journey.welcome.stage.tenant', 'journey.welcome.stage.tenantBody'],
        ['journey.welcome.stage.signIn', 'journey.welcome.stage.signInBody'],
    ];
}

/**
 * The choice, and what the chosen journey leaves behind.
 *
 * The rail names every step of the chosen journey, so these cards say what a
 * stage produces rather than repeating the step titles; the stages change with
 * the choice because the rail does, and a person who counts the rail and reads
 * the stages should find both statements true.
 *
 * The choice is made here, deliberately and in front of the person, because it
 * decides whether the installation ends up half provisioned. It is not a flag
 * read from the browser's environment: a control somebody can switch on without
 * seeing it is not a control at all.
 */
export function WelcomeCard({
    choice,
    onChoice,
}: {
    readonly choice: FirstRunChoice;
    readonly onChoice: (choice: FirstRunChoice) => void;
}): ReactNode {
    const { t } = useTranslation();
    const stages = welcomeStages(choice);
    return (
        <div className="space-y-5">
            <div
                role="radiogroup"
                aria-label={t('journey.welcome.choiceLabel')}
                className="grid gap-3 sm:grid-cols-2"
            >
                {CHOICES.map((option) => (
                    <button
                        key={option}
                        type="button"
                        role="radio"
                        aria-checked={choice === option}
                        onClick={() => onChoice(option)}
                        className={cx(
                            'card p-4 text-left transition-colors',
                            choice === option
                                ? 'border-accent ring-3 ring-accent/20'
                                : 'hover:border-line-strong',
                        )}
                    >
                        <span className="font-semibold">
                            {t(`journey.welcome.choice.${option}.title`)}
                        </span>
                        <p className="mt-1 text-sm text-ink-muted">
                            {t(`journey.welcome.choice.${option}.body`)}
                        </p>
                    </button>
                ))}
            </div>
            <ul
                className={cx(
                    'grid gap-4',
                    stages.length === 3 ? 'sm:grid-cols-3' : 'sm:grid-cols-2',
                )}
            >
                {stages.map(([title, body]) => (
                    <li key={title} className="rounded-md border border-line p-4">
                        <p className="text-sm font-medium">{t(title)}</p>
                        <p className="mt-1 text-sm text-ink-muted">{t(body)}</p>
                    </li>
                ))}
            </ul>
        </div>
    );
}

/**
 * What the system-only rail's sign-in shows.
 *
 * No tenant was created, so there is no tenant administrator to hand over to:
 * the account that signs in is the administrator the journey just made. The
 * journey holds that password, so the step signs in with it rather than asking
 * for it a second time.
 */
export function AdministratorArrival({ principal }: { readonly principal: string }): ReactNode {
    const { t } = useTranslation();
    return (
        <p className="text-sm text-ink-muted">{t('journey.signIn.administrator', { principal })}</p>
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

/**
 * The deployment's administrator, signing back in.
 *
 * A browser that closed after the administrator was created comes back to a
 * deployment that has one, so the journey cannot create it again and cannot
 * carry on without it either: the starting points and the provisioning request
 * belong to an account. The password typed here is kept for the same reason the
 * created one was: a profile may hand the creating administrator's password to
 * the tenant's administrator, and this is that password.
 *
 * The policy is not applied to the field. The password exists and meets
 * whatever rules it met when it was set, and refusing it here because a rule
 * changed since would lock somebody out of their own installation.
 */
function AdministratorSignIn({
    draft,
    onPrincipal,
    onPassword,
}: {
    readonly draft: AdministratorDraft;
    readonly onPrincipal: (value: string) => void;
    readonly onPassword: (value: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const stop = (event: FormEvent): void => event.preventDefault();
    return (
        <form className="space-y-4" onSubmit={stop}>
            <Field label={t('journey.admin.username')}>
                <Input
                    value={draft.principal}
                    autoComplete="username"
                    onChange={(event) => onPrincipal(event.target.value)}
                />
            </Field>
            <Field label={t('journey.admin.password')}>
                <PasswordInput
                    value={draft.password}
                    autoComplete="current-password"
                    onChange={(event) => onPassword(event.target.value)}
                />
            </Field>
        </form>
    );
}

export interface FirstRunJourneyProps {
    readonly server: JourneyServer;
    /**
     * Whether the deployment still has no administrator.
     *
     * It decides the first step that does anything: one deployment creates its
     * administrator, the other signs in as the one it has.
     */
    readonly inBootstrapMode: boolean;
    /** Called once the journey owns the rail, so the route table stays on it. */
    readonly onStarted: () => void;
    /** Called when the person is done, so the route table hands the browser over. */
    readonly onFinished: () => void;
}

export function FirstRunJourney({
    server,
    inBootstrapMode,
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
    const [tenantSignInComplete, setTenantSignInComplete] = useState(false);
    /*
     * What the installation is left with. The ordinary ending is the default,
     * and the choice lives here rather than on the server because it is the
     * person's, made in front of them on the welcome card.
     *
     * The choice changes the rail's shape: the five tenant steps and the
     * tenant sign-in come and go. The control that makes it is on the welcome
     * step, which is the rail's first, so only steps after the one the person
     * stands on change and the position stays inside both rails.
     */
    const [choice, setChoice] = useState<FirstRunChoice>('first-tenant');
    /*
     * Where the rail stands. Nothing is stated until somebody moves: the
     * deployment decides where the journey opens, which is the step that signs
     * in as its administrator when it has one and the welcome when it does not.
     * A browser that closed halfway through comes back to that step rather than
     * to the beginning of work that is partly done.
     */
    const [at, setAt] = useState<number>();
    const tenant = useNewTenant(creatingPassword);
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

    /**
     * Signing in as the deployment's administrator.
     *
     * The starting-point read and the provisioning request both belong to an
     * account, so the journey enters as the administrator before either. An
     * account that works in exactly one party is settled here; one that works
     * in several cannot be guessed at, and the person is asked to choose.
     */
    const signInAsAdministrator = async (password: string): Promise<void> => {
        const outcome = await server.signIn({ username: draft.principal, password });
        if (outcome.outcome === 'party-required') {
            const only = outcome.parties.length === 1 ? outcome.parties[0] : undefined;
            if (only === undefined) {
                throw new Error(t('journey.admin.partyChoice'));
            }
            await server.chooseParty(only.id, outcome.parties);
        }
    };

    /**
     * Entering as the administrator, and reading what it may build.
     *
     * The password is kept for the journey's length, because a profile may hand
     * the creating administrator's password to the tenant's administrator and
     * the hand-over signs in with it. It reaches no storage.
     */
    const enterAsAdministrator = async (password: string): Promise<void> => {
        await signInAsAdministrator(password);
        setCreatingPassword(password);
        setProfiles(await server.seedProfiles());
    };

    /**
     * Creating the administrator, and then entering as it.
     *
     * The deployment leaves bootstrap mode when the account exists, which is
     * the fact the next question turns on, so it is asked again before the
     * journey signs in as the account it has just made.
     */
    const createAdministrator = async (): Promise<void> => {
        await server.createAdministrator({
            principal: draft.principal,
            password: draft.password,
            email: draft.email,
        });
        await server.recheckBootstrap();
        await enterAsAdministrator(draft.password);
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
        tenantSignInComplete,
        onTenantSignInComplete: () => setTenantSignInComplete(true),
        choice,
        welcome: <WelcomeCard choice={choice} onChoice={setChoice} />,
        administratorArrival: <AdministratorArrival principal={draft.principal} />,
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
        administratorExists: !inBootstrapMode,
        administratorSignIn: (
            <AdministratorSignIn
                draft={draft}
                onPrincipal={(principal) => setDraft((current) => ({ ...current, principal }))}
                onPassword={(password) => setDraft((current) => ({ ...current, password }))}
            />
        ),
        onCreateAdministrator: createAdministrator,
        onAdministratorEntered: () => enterAsAdministrator(draft.password),
        onAdministratorSignIn: () => signInAsAdministrator(creatingPassword),
        onHandOff: handOff,
        onCompleteSystemOnboarding: () => server.completeSystemOnboarding(),
        onFinished: () => onFinished(),
    });

    return (
        <JourneyPage
            steps={steps}
            at={at ?? indexOfStep(steps, inBootstrapMode ? 'welcome' : 'administrator')}
            onMove={setAt}
            header={<JourneyHeader tenant={tenant} />}
        />
    );
}
