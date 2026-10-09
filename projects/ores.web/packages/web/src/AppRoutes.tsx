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

import { useEffect, useRef, useState, type ReactNode } from 'react';
import { Navigate, Route, Routes, useNavigate, useParams } from 'react-router';
import { useQuery } from '@tanstack/react-query';
import { api } from './api/client.js';
import { useTranslation } from './i18n/Provider.js';
import { useBootstrap, type BootstrapState } from './session/BootstrapProvider.js';
import { useSession, type SessionState } from './session/SessionProvider.js';
import { useSite } from './session/SiteProvider.js';
import { ErrorBoundary } from './components/ErrorBoundary.js';
import { ErrorBanner } from './components/ErrorBanner.js';
import { setupAnswerStep } from './session/setupAnswer.js';
import { useJourneyServer } from './journeys/server.js';
import { FirstRunJourney } from './journeys/FirstRunJourney.js';
import { NewTenantJourney } from './journeys/NewTenantJourney.js';
import { NewPartyJourney } from './journeys/NewPartyJourney.js';
import { SignUpJourney } from './journeys/SignUpJourney.js';
import { TenantSetupJourney } from './journeys/TenantSetupJourney.js';
import { AppShell } from './components/AppShell.js';
import type { ShellWidth } from './shell/layout.js';
import { PublicShell } from './components/PublicShell.js';
import { AuditPage } from './pages/AuditPage.js';
import { HomePage } from './pages/HomePage.js';
import { OperationsArea } from './operations/OperationsArea.js';
import { BusPage } from './operations/BusPage.js';
import { GridPage } from './operations/GridPage.js';
import { LogsPage } from './operations/LogsPage.js';
import { ServicesPage } from './operations/ServicesPage.js';
import { VersionsPage } from './operations/VersionsPage.js';
import { RescuePage } from './pages/RescuePage.js';
import { SecurityPage } from './pages/SecurityPage.js';
import { MyAccessPage } from './access/MyAccessPage.js';
import { PeoplePage } from './access/PeoplePage.js';
import { PersonPage } from './access/PersonPage.js';
import { RolePage } from './access/RolePage.js';
import { RolesPage } from './access/RolesPage.js';
import { RequestDetailPage } from './inbox/RequestDetailPage.js';
import { RequestStoryPage } from './inbox/RequestStoryPage.js';
import { RequestsPage } from './inbox/RequestsPage.js';
import { ClassificationListPage } from './refdata/ClassificationListPage.js';
import { ClassificationRowPage } from './refdata/ClassificationRowPage.js';
import { ClassificationsPage } from './refdata/ClassificationsPage.js';
import { RefdataPage } from './refdata/RefdataPage.js';
import { CurrenciesPage, CurrencyPage } from './refdata/currencies.js';
import { CalendarPage, CalendarsPage } from './refdata/calendars.js';
import { CurrencyPairPage, CurrencyPairsPage } from './refdata/currencyPairs.js';
import { DeskGroupPage, DeskGroupsPage } from './refdata/deskGroups.js';
import { TenantAccountPage } from './access/TenantAccountPage.js';
import { PartiesPage } from './pages/PartiesPage.js';
import { ProfilePage } from './pages/ProfilePage.js';
import { TenantPage } from './pages/TenantPage.js';
import { TenantRunPage } from './pages/TenantRunPage.js';
import { TenantsPage } from './pages/TenantsPage.js';
import { SignInPage, type SignInPageProps } from './pages/SignInPage.js';
import { Button, Notice } from './ui/Primitives.js';
import type { Account, SessionView } from '@ores/wire-protocol/browser';
import type { EnvironmentView } from '@ores/contracts';

/**
 * The route table, and the gate that keeps an installation on its setup screen.
 *
 * The gate is the whole reason this is a function of two states rather than a
 * component that reads them: "while the deployment is not set up, the setup
 * page is the only page" is a rule, and a rule a test cannot call is a rule
 * nobody has checked. `ConnectedApp` below is the wiring.
 *
 * A deployment with no administrator has nobody to sign in as, so every path
 * renders the first run journey rather than redirecting to a setup path: there
 * is nothing else to be at, and a redirect leaves a URL somebody can share that
 * leads nowhere. Creating the administrator closes that question but not the
 * job, so the gate carries further reasons to stay. The first is that the
 * journey has begun in this browser, which holds the rail until the person
 * finishes. The second is that a system administrator is signed in and the
 * system provisioner wizard has not recorded that it finished: a first-run
 * installation may keep only the system tenant, and the wizard signs the person
 * out at the end, so the flag is read through their session while they have
 * one. The third is a session whose tenant is still bootstrapping, which holds
 * the browser on the tenant setup screen: the fact comes from the tenant's own
 * lifecycle status, so it reads the same in every party of the tenant. A
 * visitor with no session is never held, and neither is a session in a tenant
 * that has finished its run or whose deployment kept no tenant of its own: the
 * deployment's lacking a tenant of its own is not a reason to hold the rail.
 * The deployment's own state is what survives a reload; the tab's memory only
 * outlives the run.
 */
export interface AppRoutesProps {
    readonly gate: BootstrapState;
    readonly session: SessionState;
    /** The first run journey, which only the wiring can reach the server for. */
    readonly journey: ReactNode;
    /**
     * The tenant setup screen, which a signed-in tenant administrator runs.
     *
     * It follows the run the tenant owns, and the gate holds that administrator
     * on it until the run records its finish.
     */
    readonly tenantSetupJourney: ReactNode;
    /**
     * The new tenant journey, which a signed-in administrator runs.
     *
     * It is a route rather than a gate: the installation is already set up, so
     * the person arrives at it from a screen and leaves it for one.
     */
    readonly newTenantJourney: ReactNode;
    /**
     * The new party journey, which a tenant administrator runs.
     *
     * It is a route for the same reason, and it is the tenant's own: the
     * session already names the tenant, so the journey adds a party to it
     * rather than asking which one.
     */
    readonly newPartyJourney: ReactNode;
    /**
     * The registration door, which a visitor with no account runs.
     *
     * It is a route rather than a gate for the same reason the other two are:
     * the installation is set up, and this is a screen a person arrives at from
     * the sign-in dialog and leaves for it.
     */
    readonly signUpJourney: ReactNode;
    /** Whether that journey is running, which keeps the browser on its rail. */
    readonly journeyInProgress: boolean;
    /**
     * The signed-in person's own account, when the wiring has read it.
     *
     * The shell's account menu shows their name and picture from it, and the
     * profile screen is the one place that changes both.
     */
    readonly self?: Account | null;
    /**
     * The environment the deployment serves, when the wiring has read it.
     *
     * Every shell states it in the footer beside the two builds, so a person
     * can tell which checkout they are pointed at and whether it is
     * production.
     */
    readonly environment?: EnvironmentView | undefined;
    readonly onSignIn: SignInPageProps['onSignIn'];
    readonly onChooseParty: SignInPageProps['onChooseParty'];
    readonly onSignOut: () => void;
    readonly onRetryBootstrap: () => void;
}

export function AppRoutes({
    gate,
    session,
    journey,
    tenantSetupJourney,
    newTenantJourney,
    newPartyJourney,
    signUpJourney,
    journeyInProgress,
    self,
    environment,
    onSignIn,
    onChooseParty,
    onSignOut,
    onRetryBootstrap,
}: AppRoutesProps): ReactNode {
    const shell: ShellActions = { onSignOut, self: self ?? null, environment };
    const { t } = useTranslation();

    if (gate.status === 'loading' || session.status === 'loading') {
        return <Centred>{t('common.loading')}</Centred>;
    }

    if (gate.status === 'unreachable') {
        return (
            <Centred>
                <div className="w-full max-w-[420px]">
                    <Notice tone="error">{t('gate.unreachable', { reason: gate.reason })}</Notice>
                    <div className="flex justify-center">
                        <Button variant="secondary" onClick={onRetryBootstrap}>
                            {t('gate.retry')}
                        </Button>
                    </div>
                </div>
            </Centred>
        );
    }

    /*
     * The two setup flags are settings, and settings are read through a session,
     * while the rest of the bootstrap answer is read without one. So an answer
     * that names no account, or one the session has since left, says nothing
     * about the account in hand: acting on it would read a deployment that has
     * never been set up and put the wizard in front of somebody who has just
     * finished it. The answer is on its way, and nothing is decided until it
     * arrives.
     */
    const signedInAccountId = session.status === 'authenticated' ? session.session.accountId : '';
    if (gate.accountId !== signedInAccountId) {
        return <Centred>{t('common.loading')}</Centred>;
    }

    /*
     * A system administrator resuming a provisioner wizard that has not
     * recorded its finish stays on the rail. The mode is the session's own,
     * read from the login answer rather than derived from the tenant here.
     */
    const resumingSystemSetup =
        session.status === 'authenticated' &&
        session.session.mode === 'system-administration' &&
        !gate.onboardingComplete;

    /*
     * A session whose tenant is still bootstrapping stays on the tenant setup
     * screen. The fact is the session's, taken from the tenant's own lifecycle
     * status when the session opened, so it reads the same in every party of
     * the tenant. The setting this replaces lived under the tenant's system
     * party, and a session in any other party read it as unfinished forever.
     * The system tenant is never bootstrapping, so a super administrator is
     * never held here.
     */
    const resumingTenantSetup =
        session.status === 'authenticated' && session.session.tenantBootstrapping;

    /*
     * A rail is the one public screen somebody can be signed in on, so it
     * carries the way out a signed-in person owes. A setup that cannot be
     * finished, or a session the deployment has forgotten, must not be a room
     * with no door.
     */
    const railActions =
        session.status === 'authenticated' ? (
            <Button variant="secondary" onClick={() => void onSignOut()}>
                {t('nav.signOut')}
            </Button>
        ) : undefined;

    if (gate.inBootstrapMode || resumingSystemSetup || journeyInProgress) {
        return (
            <Routes>
                <Route
                    path="*"
                    element={
                        <PublicShell
                            wide
                            serverVersion={gate.version}
                            environment={environment}
                            actions={railActions}
                        >
                            {journey}
                        </PublicShell>
                    }
                />
            </Routes>
        );
    }

    if (resumingTenantSetup) {
        return (
            <Routes>
                <Route
                    path="*"
                    element={
                        <PublicShell
                            wide
                            serverVersion={gate.version}
                            environment={environment}
                            actions={railActions}
                        >
                            {tenantSetupJourney}
                        </PublicShell>
                    }
                />
            </Routes>
        );
    }

    return (
        <Routes>
            <Route
                path="/login"
                element={
                    session.status === 'authenticated' ? (
                        <Navigate to="/" replace />
                    ) : (
                        <PublicShell serverVersion={gate.version} environment={environment}>
                            <SignInPage onSignIn={onSignIn} onChooseParty={onChooseParty} />
                        </PublicShell>
                    )
                }
            />
            <Route
                path="/signup"
                element={
                    session.status === 'authenticated' ? (
                        <Navigate to="/" replace />
                    ) : (
                        <PublicShell wide serverVersion={gate.version} environment={environment}>
                            {signUpJourney}
                        </PublicShell>
                    )
                }
            />
            <Route
                path="/"
                element={signedIn(gate.version, session, shell, (view) => (
                    <HomePage
                        username={view.username}
                        email={view.email}
                        tenantName={view.tenantName}
                        partyName={view.party.name}
                        mode={view.mode}
                        self={shell.self}
                    />
                ))}
            />
            {/*
             * The roster is a table, so it takes the room it is given: a list
             * bounded at the width a form wants leaves a dead margin on either
             * side of nothing.
             */}
            <Route
                path="/tenants"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <TenantsPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/security"
                element={signedIn(gate.version, session, shell, (view) => (
                    <SecurityPage session={view} />
                ))}
            />
            {/*
             * The member's own profile, and the administrator's way into
             * somebody else's. It is one screen because it is one record: the
             * panels are the ones the person themselves sees, and the search
             * above them is the only difference.
             */}
            <Route
                path="/profile"
                element={signedIn(gate.version, session, shell, (view) => (
                    <ProfilePage session={view} />
                ))}
            />
            {/*
             * The administrator's half of the credentials topic. It is a
             * screen of its own rather than a panel beside Security, because
             * it acts on somebody else's account and Security is the member's
             * own.
             */}
            <Route
                path="/rescue"
                element={signedIn(gate.version, session, shell, () => (
                    <RescuePage />
                ))}
            />
            {/*
             * The record of what happened: the tenant's sessions, their
             * activity and the sign-ins that failed. It is the administrator's
             * third screen rather than a panel of Security, because it reads
             * the tenant and not the member.
             */}
            <Route
                path="/audit"
                element={signedIn(gate.version, session, shell, () => (
                    <AuditPage />
                ))}
            />
            {/*
             * The operations area: its hub, the versions screen the prototype
             * established, and the services, grid, bus and logs screens. No
             * route here is gated: any signed-in person who knows the URL
             * reaches them, and the menus decide only what is offered, not who
             * may look. The services, grid, bus and logs reads are refused by
             * their BFF routes unless the session acts on the deployment, so
             * that reachability is enforced where the data is. The hub, the
             * roster, the grid, the bus and the logs are grids, so they take
             * the width; the versions panels are a column of detail, so they
             * keep the default.
             */}
            <Route
                path="/operations"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <OperationsArea />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/operations/services"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <ServicesPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/operations/grid"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <GridPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/operations/bus"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <BusPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/operations/logs"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <LogsPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/operations/versions"
                element={signedIn(gate.version, session, shell, (view, serverVersion) => (
                    <VersionsPage session={view} serverVersion={serverVersion} />
                ))}
            />
            <Route
                path="/tenants/new"
                element={signedIn(gate.version, session, shell, () => newTenantJourney)}
            />
            <Route
                path="/tenants/runs/:instanceId"
                element={signedIn(gate.version, session, shell, () => (
                    <TenantRunPage />
                ))}
            />
            <Route
                path="/tenants/:code/people/:username"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <TenantAccountPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/tenants/:code"
                element={signedIn(gate.version, session, shell, () => (
                    <TenantPage />
                ))}
            />
            <Route
                path="/parties"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <PartiesPage />
                    ),
                    'workspace',
                )}
            />
            {/*
             * Access: a person's own roles, and the administrator's people and
             * role catalogue. The lists are tables, so they take the width.
             */}
            <Route
                path="/access"
                element={signedIn(gate.version, session, shell, (view) => (
                    <MyAccessPage tenantName={view.tenantName} />
                ))}
            />
            <Route
                path="/people"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    (view) => (
                        <PeoplePage mode={view.mode} />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/people/:username"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    (view) => (
                        <PersonPage me={view.username} />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/refdata"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <RefdataPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/refdata/classifications"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <ClassificationsPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/refdata/classifications/:list"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <ClassificationListPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/refdata/classifications/:list/:code"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <ClassificationRowPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/refdata/currencies"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <CurrenciesPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/refdata/currencies/:code"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <CurrencyPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/refdata/desk-groups"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <DeskGroupsPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/refdata/desk-groups/:code"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <DeskGroupPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/refdata/currency-pairs"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <CurrencyPairsPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/refdata/currency-pairs/:code"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <CurrencyPairPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/refdata/calendars"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <CalendarsPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/refdata/calendars/:code"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <CalendarPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/classifications"
                element={<Navigate to="/refdata/classifications" replace />}
            />
            <Route path="/classifications/:list" element={<ClassificationsRedirect />} />
            <Route
                path="/roles"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <RolesPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/roles/:roleId"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <RolePage />
                    ),
                    'workspace',
                )}
            />
            {/*
             * The approval requests: the administrator's queue, and the screen
             * one request is answered on. Both are tables and bundles, so both
             * take the width.
             */}
            <Route
                path="/requests"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    () => (
                        <RequestsPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/requests/:id"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    (view) => (
                        <RequestDetailPage me={view.username} />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/requests/:id/story"
                element={signedIn(
                    gate.version,
                    session,
                    shell,
                    (view) => (
                        <RequestStoryPage me={view.username} />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/parties/new"
                element={signedIn(gate.version, session, shell, () => newPartyJourney)}
            />
            <Route path="*" element={<Navigate to="/" replace />} />
        </Routes>
    );
}

/** The wiring: the two states, and the actions the screens can take. */
export function ConnectedApp(): ReactNode {
    const { state: gate, recheck, reading } = useBootstrap();
    const { state: session, signIn, chooseParty, refresh, signOut } = useSession();
    const { environment } = useSite();
    const { t } = useTranslation();
    const server = useJourneyServer();
    const navigate = useNavigate();
    const [journeyInProgress, setJourneyInProgress] = useState(false);
    const username = session.status === 'authenticated' ? session.session.username : '';
    /*
     * The gate's setup flags are read through the session, and the session they
     * were first read through was nobody's. Signing in changes what that answer
     * means, so the deployment is asked again whenever the answer names an
     * account other than the one in hand. The action is held in a ref so a
     * changed answer cannot ask again by changing the effect's own dependencies,
     * and the account the answer names is what ends the asking.
     *
     * A deployment that answers twice without naming the account this browser
     * holds does not know the session it is being shown, and the browser belongs
     * at the sign-in form: asking a third time would answer the same way, and a
     * screen whose one action cannot succeed is worse than a fresh sign-in. That
     * is what a browser left open across a restart sees, and what a rail nobody
     * can leave turns into.
     */
    const recheckNow = useRef(recheck);
    useEffect(() => {
        recheckNow.current = recheck;
    }, [recheck]);
    const signedInAs = session.status === 'authenticated' ? session.session.accountId : '';
    const askedFor = useRef<string | undefined>(undefined);
    useEffect(() => {
        const step = setupAnswerStep({
            answerAccountId: gate.status === 'ready' ? gate.accountId : signedInAs,
            answerSessionPresent: gate.status === 'ready' && gate.sessionPresent,
            signedInAs,
            reading,
            asked: askedFor.current === signedInAs,
        });
        if (step === 'ask') {
            askedFor.current = signedInAs;
            void recheckNow.current();
            return;
        }
        if (step === 'sign-out') {
            void signOut();
        }
    }, [gate, session, signedInAs, signOut, reading]);
    /*
     * The rail belongs to a setup that has not finished. Once the deployment
     * records that it has, this browser is done with the wizard whatever the tab
     * remembers, so a rail entered on an answer that was still being read cannot
     * hold it on a finished setup.
     */
    useEffect(() => {
        if (gate.status === 'ready' && gate.onboardingComplete) {
            setJourneyInProgress(false);
        }
    }, [gate]);
    /*
     * The signed-in person's own account: the shell's menu shows their name
     * and picture from it, and the profile screen reads the same answer for
     * its panels. A member may not hold the account read the route asks for,
     * so a refused answer is left alone here; the shell falls back to the
     * username and the initials, and the profile screen states the refusal.
     */
    const account = useQuery({
        queryKey: ['account', username],
        queryFn: () => api.account(username),
        enabled: username !== '',
    });

    return (
        <>
            {/*
             * The banner stands outside the boundary rather than inside it, so
             * a screen that cannot render at all still leaves what the request
             * said on the page beside the screen that stands in for it.
             */}
            <ErrorBanner />
            <ErrorBoundary title={t('journey.failedHeading')} hint={t('journey.reloadHint')}>
                <AppRoutes
                    gate={gate}
                    session={session}
                    self={account.data ?? null}
                    environment={environment}
                    journey={
                        <FirstRunJourney
                            server={server}
                            inBootstrapMode={gate.status === 'ready' && gate.inBootstrapMode}
                            signedIn={session.status === 'authenticated'}
                            onStarted={() => setJourneyInProgress(true)}
                            onFinished={() => {
                                setJourneyInProgress(false);
                                /*
                                 * The tenant the journey just made is the fact that
                                 * releases the gate, and this answer is what carries
                                 * it: without asking again the deployment would still
                                 * read as one with no tenant, and the journey would
                                 * keep the browser it has just finished with.
                                 */
                                void recheck();
                            }}
                        />
                    }
                    newTenantJourney={
                        <NewTenantJourney
                            server={server}
                            /*
                             * The journey ends either inside the new tenant or signed
                             * out, and both are places the route table already knows:
                             * the home page sends a signed-out visitor to sign in.
                             */
                            onFinished={() => navigate('/')}
                        />
                    }
                    tenantSetupJourney={
                        <TenantSetupJourney
                            server={server}
                            /*
                             * The run's finish is what makes the tenant active, so
                             * the session is read again rather than trusted: its
                             * answer is what lets this administrator into the
                             * application.
                             */
                            onFinished={() => {
                                void refresh();
                            }}
                        />
                    }
                    newPartyJourney={
                        <NewPartyJourney
                            server={server}
                            /*
                             * The journey ends working as the new party, or at the
                             * screen the person came from; both are the home page.
                             */
                            onFinished={() => navigate('/')}
                        />
                    }
                    signUpJourney={<SignUpJourney server={server} />}
                    journeyInProgress={journeyInProgress}
                    onSignIn={signIn}
                    onChooseParty={chooseParty}
                    onSignOut={() => {
                        void signOut();
                    }}
                    onRetryBootstrap={() => {
                        void recheck();
                    }}
                />
            </ErrorBoundary>
        </>
    );
}

/**
 * The application shell around a screen, and the guard in front of it.
 *
 * An unauthenticated visitor is sent to the sign-in screen rather than told
 * they may not look: they may, once they have signed in.
 */
/** What the shell around every signed-in screen knows and can do. */
interface ShellActions {
    readonly onSignOut: () => void;
    readonly self: Account | null;
    /** The environment the deployment serves, which every shell states. */
    readonly environment: EnvironmentView | undefined;
}

function signedIn(
    serverVersion: string,
    session: SessionState,
    shell: ShellActions,
    screen: (session: SessionView, serverVersion: string) => ReactNode,
    width: ShellWidth = 'column',
): ReactNode {
    if (session.status !== 'authenticated') {
        return <Navigate to="/login" replace />;
    }
    const view = session.session;
    /*
     * The session states the build it was opened against, which is newer than
     * the deployment's first answer; that answer is what a screen has before
     * anybody signs in. The screen and the footer both get this resolved value,
     * so the versions panel cannot state a build the footer disagrees with.
     */
    const version = view.version !== '' ? view.version : serverVersion;
    return (
        <AppShell
            username={view.username}
            tenantName={view.tenantName}
            partyName={view.party.name}
            mode={view.mode}
            width={width}
            serverVersion={version}
            environment={shell.environment}
            onSignOut={shell.onSignOut}
            self={shell.self}
        >
            {screen(view, version)}
        </AppShell>
    );
}

function Centred({ children }: { readonly children: ReactNode }): ReactNode {
    return <div className="grid min-h-full place-items-center bg-bg-primary px-5">{children}</div>;
}

/** The first build's address for one list, kept so a saved link still opens it. */
function ClassificationsRedirect(): ReactNode {
    const { list } = useParams();
    return <Navigate to={`/refdata/classifications/${encodeURIComponent(list ?? '')}`} replace />;
}
