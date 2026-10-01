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

import { useState, type ReactNode } from 'react';
import { Navigate, Route, Routes, useNavigate } from 'react-router';
import { useTranslation } from './i18n/Provider.js';
import { useBootstrap, type BootstrapState } from './session/BootstrapProvider.js';
import { useSession, type SessionState } from './session/SessionProvider.js';
import { useJourneyServer } from './journeys/server.js';
import { FirstRunJourney } from './journeys/FirstRunJourney.js';
import { NewTenantJourney } from './journeys/NewTenantJourney.js';
import { NewPartyJourney } from './journeys/NewPartyJourney.js';
import { SignUpJourney } from './journeys/SignUpJourney.js';
import { AppShell } from './components/AppShell.js';
import type { ShellWidth } from './shell/layout.js';
import { PublicShell } from './components/PublicShell.js';
import { HomePage } from './pages/HomePage.js';
import { TenantsPage } from './pages/TenantsPage.js';
import { SignInPage, type SignInPageProps } from './pages/SignInPage.js';
import { Button, Notice } from './ui/Primitives.js';
import type { SessionView } from '@ores/wire-protocol/browser';

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
 * job, so the gate carries two further reasons to stay: the deployment has no
 * tenant of its own, which is the state the journey exists to leave behind, and
 * the journey has begun in this browser, which holds the rail after the tenant
 * exists until the person finishes. The deployment's own state is what survives
 * a reload; the tab's memory only outlives the tenant.
 */
export interface AppRoutesProps {
    readonly gate: BootstrapState;
    readonly session: SessionState;
    /** The first run journey, which only the wiring can reach the server for. */
    readonly journey: ReactNode;
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
    readonly onSignIn: SignInPageProps['onSignIn'];
    readonly onChooseParty: SignInPageProps['onChooseParty'];
    readonly onSignOut: () => void;
    readonly onRetryBootstrap: () => void;
}

export function AppRoutes({
    gate,
    session,
    journey,
    newTenantJourney,
    newPartyJourney,
    signUpJourney,
    journeyInProgress,
    onSignIn,
    onChooseParty,
    onSignOut,
    onRetryBootstrap,
}: AppRoutesProps): ReactNode {
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

    if (gate.inBootstrapMode || !gate.hasTenant || journeyInProgress) {
        return (
            <Routes>
                <Route
                    path="*"
                    element={
                        <PublicShell wide serverVersion={gate.version}>
                            {journey}
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
                        <PublicShell serverVersion={gate.version}>
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
                        <PublicShell wide serverVersion={gate.version}>
                            {signUpJourney}
                        </PublicShell>
                    )
                }
            />
            <Route
                path="/"
                element={signedIn(gate.version, session, onSignOut, (view) => (
                    <HomePage
                        username={view.username}
                        email={view.email}
                        tenantName={view.tenantName}
                        partyName={view.party.name}
                        mode={view.mode}
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
                    onSignOut,
                    () => (
                        <TenantsPage />
                    ),
                    'workspace',
                )}
            />
            <Route
                path="/tenants/new"
                element={signedIn(gate.version, session, onSignOut, () => newTenantJourney)}
            />
            <Route
                path="/parties/new"
                element={signedIn(gate.version, session, onSignOut, () => newPartyJourney)}
            />
            <Route path="*" element={<Navigate to="/" replace />} />
        </Routes>
    );
}

/** The wiring: the two states, and the actions the screens can take. */
export function ConnectedApp(): ReactNode {
    const { state: gate, recheck } = useBootstrap();
    const { state: session, signIn, chooseParty, signOut } = useSession();
    const server = useJourneyServer();
    const navigate = useNavigate();
    const [journeyInProgress, setJourneyInProgress] = useState(false);

    return (
        <AppRoutes
            gate={gate}
            session={session}
            journey={
                <FirstRunJourney
                    server={server}
                    inBootstrapMode={gate.status === 'ready' && gate.inBootstrapMode}
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
    );
}

/**
 * The application shell around a screen, and the guard in front of it.
 *
 * An unauthenticated visitor is sent to the sign-in screen rather than told
 * they may not look: they may, once they have signed in.
 */
function signedIn(
    serverVersion: string,
    session: SessionState,
    onSignOut: () => void,
    screen: (session: SessionView) => ReactNode,
    width: ShellWidth = 'column',
): ReactNode {
    if (session.status !== 'authenticated') {
        return <Navigate to="/login" replace />;
    }
    const view = session.session;
    /*
     * The session states the build it was opened against, which is newer than
     * the deployment's first answer; that answer is what a screen has before
     * anybody signs in.
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
            onSignOut={onSignOut}
        >
            {screen(view)}
        </AppShell>
    );
}

function Centred({ children }: { readonly children: ReactNode }): ReactNode {
    return <div className="grid min-h-full place-items-center bg-bg-primary px-5">{children}</div>;
}
