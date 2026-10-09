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
 * rail. The person chooses at the starting point what the installation is left
 * with: the ordinary ending creates the first tenant, whose stages run inline,
 * and leaves it bootstrapping for its own administrator; the other creates no
 * tenant, so the installation keeps the system tenant alone. The four tenant
 * stages are the shared library, so the new tenant journey runs the same ones
 * rather than a copy of them. The tenant's own setup is its administrator's
 * run, not a step here, so this rail ends once the tenant exists and the
 * system flag is recorded.
 *
 * Two things the journey does are worth stating where they happen. The browser
 * signs in as the administrator it just created, because the starting-point
 * read and the provisioning request belong to an account; and it holds that
 * administrator's password in memory for no longer than the journey, because a
 * profile may give the password to the tenant's administrator and nothing may
 * write it down.
 */

import { useEffect, useRef, useState, type FormEvent, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Field, Input, Notice } from '../ui/Primitives.js';
import { NewPasswordField, PasswordInput } from '../ui/PasswordField.js';
import { JourneyPage } from './JourneyPage.js';
import { JourneyHeader } from './parts.js';
import { indexOfStep, type StepId } from './runtime.js';
import { firstRunSteps, type AdministratorDraft, type FirstRunChoice } from './firstRunSteps.js';
import type { JourneyServer } from './server.js';
import { useNewTenant } from './state.js';
import type { PasswordPolicy, PartySummary, SeedProfileChoice } from '@ores/wire-protocol/browser';

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
 * The step a first run opens on.
 *
 * A deployment with no administrator opens at the welcome, because the person
 * has to be told what they are starting. A deployment that has one, and a
 * browser that is not signed in as it, opens at the administrator: the account
 * exists, and only its owner may carry on. A browser already signed in has
 * nothing to authenticate, so the rail resumes at the starting point, which is
 * the first question that is genuinely open.
 */
export function startStep(signInRequired: boolean, signedIn: boolean): StepId {
    if (signInRequired) {
        return 'administrator';
    }
    return signedIn ? 'profile' : 'welcome';
}

/**
 * The party category a tenant's system party carries.
 *
 * Bootstrap signs in here and nowhere else: the settings it writes are
 * tenant-wide, which means scoped to this party, and a session in any other
 * party cannot write them.
 */
const SYSTEM_PARTY_CATEGORY = 'System';

/**
 * The system party among the parties a sign-in offers, or nothing when the
 * account works in none.
 *
 * Bootstrap signs in here and nowhere else. It writes tenant-wide settings, and
 * a tenant-wide setting is scoped to the tenant's system party; a session in any
 * other party is refused by the party isolation policy, whose visible set walks
 * down from the selected party and finds the system party only when the selected
 * party is its root.
 */
export function systemPartyOf(parties: readonly PartySummary[]): PartySummary | undefined {
    return parties.find((party) => party.partyCategory === SYSTEM_PARTY_CATEGORY);
}

/**
 * What the welcome states about the journey, as a title key and a body key.
 *
 * It is an introduction and nothing else: the person reads what the setup does
 * before starting it. What the installation is left with is chosen at the
 * starting point, so no stage here is a choice.
 */
const WELCOME_STAGES: readonly (readonly [string, string])[] = [
    ['journey.welcome.stage.admin', 'journey.welcome.stage.adminBody'],
    ['journey.welcome.stage.tenant', 'journey.welcome.stage.tenantBody'],
    ['journey.welcome.stage.handOver', 'journey.welcome.stage.handOverBody'],
];

/**
 * The welcome, which introduces the journey the person is about to take.
 *
 * The rail names every step, so these cards say what a stage produces rather
 * than repeating the step titles: a person who counts the rail and reads the
 * stages should find both statements true.
 */
export function WelcomeIntro(): ReactNode {
    const { t } = useTranslation();
    return (
        <ul className="grid gap-4 sm:grid-cols-3">
            {WELCOME_STAGES.map(([title, body]) => (
                <li key={title} className="rounded-md border border-line p-4">
                    <p className="text-sm font-medium">{t(title)}</p>
                    <p className="mt-1 text-sm text-ink-muted">{t(body)}</p>
                </li>
            ))}
        </ul>
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
    /**
     * Whether this browser is already signed in.
     *
     * A deployment that has its administrator, and a browser already signed in
     * as it, is a journey with nothing to authenticate: the person either made
     * that account a moment ago or signed in before the page was reloaded, and
     * asking them for the credentials again asks for a session they hold. The
     * rail therefore resumes at the starting point instead of the administrator.
     */
    readonly signedIn: boolean;
    /** Called once the journey owns the rail, so the route table stays on it. */
    readonly onStarted: () => void;
    /** Called when the person is done, so the route table hands the browser over. */
    readonly onFinished: () => void;
}

export function FirstRunJourney({
    server,
    inBootstrapMode,
    signedIn,
    onStarted,
    onFinished,
}: FirstRunJourneyProps): ReactNode {
    const { t } = useTranslation();
    const [policy, setPolicy] = useState<PasswordPolicy>();
    const [profiles, setProfiles] = useState<readonly SeedProfileChoice[]>([]);
    const [profilesFailure, setProfilesFailure] = useState<string>();
    const [loadFailure, setLoadFailure] = useState<string>();
    const [attempt, setAttempt] = useState(0);
    const [draft, setDraft] = useState<AdministratorDraft>({
        principal: DEFAULT_PRINCIPAL,
        email: DEFAULT_EMAIL,
        password: '',
        acceptable: false,
    });
    const [creatingPassword, setCreatingPassword] = useState('');
    /*
     * Whether the deployment already held its administrator when this journey
     * began, fixed for as long as the journey runs.
     *
     * Creating the administrator clears the deployment's bootstrap flag, and the
     * journey reads that flag again when the account is made, because signing in
     * as it happens against the deployment rather than the copy this page holds.
     * A rail that derived its shape from the live flag would turn the form the
     * person has just filled in into a sign-in for the account they have just
     * created: the second sign-in this journey exists to spare them.
     */
    const startedWithAdministrator = useRef(!inBootstrapMode).current;
    /*
     * Whether the person is asked to sign in as the deployment's administrator
     * rather than create it.
     *
     * The answer frozen above is not quite enough on its own: a browser sitting
     * on this rail while the deployment is rebuilt underneath it holds a stale
     * one, and would ask for a sign-in to a deployment that is waiting for its
     * first administrator. A deployment that says it is in bootstrap mode has
     * nobody to sign in as, so that answer is taken as it stands. And a browser
     * that is already signed in is not asked at all: the rail asks for a
     * session, and it has one.
     */
    const signInRequired = startedWithAdministrator && !inBootstrapMode && !signedIn;
    /*
     * What the installation is left with. The ordinary ending is the default,
     * and the choice lives here rather than on the server because it is the
     * person's, made in front of them on the starting point.
     *
     * The choice changes the rail's shape: the four tenant stages come and go.
     * The control that makes it is on the starting point, which is the last step
     * both rails share, so only steps after the one the person stands on change
     * and the position stays inside both rails.
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

    /**
     * Signing in as the deployment's administrator.
     *
     * The starting-point read and the provisioning request both belong to an
     * account, so the journey enters as the administrator before either.
     *
     * The party is always the tenant's system party, and it is never asked for.
     * Bootstrap writes tenant-wide settings, and a tenant-wide setting is scoped
     * to the system party: a session sitting in any other party is refused by
     * the party isolation policy, because the visible set walks down from the
     * selected party and the system party is its root. Offering the choice would
     * offer a way to fail, so the choice is not offered.
     */
    const signInAsAdministrator = async (password: string): Promise<void> => {
        const outcome = await server.signIn({ username: draft.principal, password });
        if (outcome.outcome === 'party-required') {
            const system = systemPartyOf(outcome.parties);
            if (system === undefined) {
                throw new Error(t('journey.admin.noSystemParty'));
            }
            await server.chooseParty(system.id, outcome.parties);
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
    };

    /*
     * The starting points, once there is an account to read them with.
     *
     * They are the system tenant's rows, so the read needs an account and cannot
     * happen before one exists. It used to be part of creating the administrator,
     * which covered the first run and nothing else: a browser that arrives
     * already signed in — the person who made the administrator a moment ago, or
     * one who signed in and reloaded — resumed at the starting point with nothing
     * to choose from, because this page had never asked for them.
     */
    useEffect(() => {
        if (!signedIn) {
            return;
        }
        let cancelled = false;
        void (async () => {
            try {
                const loaded = await server.seedProfiles();
                if (!cancelled) {
                    setProfiles(loaded);
                    setProfilesFailure(undefined);
                }
            } catch (error) {
                if (!cancelled) {
                    setProfilesFailure(reasonOf(error));
                }
            }
        })();
        return () => {
            cancelled = true;
        };
    }, [server, signedIn]);

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
        choice,
        onChoose: setChoice,
        welcome: <WelcomeIntro />,
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
        administratorExists: signInRequired,
        administratorSignIn: (
            <AdministratorSignIn
                draft={draft}
                onPrincipal={(principal) => setDraft((current) => ({ ...current, principal }))}
                onPassword={(password) => setDraft((current) => ({ ...current, password }))}
            />
        ),
        onCreateAdministrator: createAdministrator,
        onAdministratorEntered: () => enterAsAdministrator(draft.password),
        onCompleteSystemOnboarding: () => server.completeSystemOnboarding(),
        onFinished,
    });

    /*
     * The header names the tenant being described, and an installation that
     * keeps no tenant has none: a profile chosen before the person changed
     * their mind is not a tenant they are creating.
     */
    const describedTenant = choice === 'system-only' ? { ...tenant, profile: undefined } : tenant;

    return (
        <JourneyPage
            steps={steps}
            at={at ?? indexOfStep(steps, startStep(signInRequired, signedIn))}
            onMove={setAt}
            header={
                <>
                    {profilesFailure !== undefined && (
                        <Notice tone="error">
                            {t('journey.profilesFailed', { message: profilesFailure })}
                        </Notice>
                    )}
                    <JourneyHeader tenant={describedTenant} />
                </>
            }
        />
    );
}
