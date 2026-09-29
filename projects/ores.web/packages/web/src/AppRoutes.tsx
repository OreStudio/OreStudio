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
import { Navigate, Route, Routes } from 'react-router';
import { useTranslation } from './i18n/Provider.js';
import { useBootstrap, type BootstrapState } from './session/BootstrapProvider.js';
import { useSession, type SessionState } from './session/SessionProvider.js';
import { useJourneyServer } from './journeys/server.js';
import { FirstRunJourney } from './journeys/FirstRunJourney.js';
import { AppShell } from './components/AppShell.js';
import { PublicShell } from './components/PublicShell.js';
import { HomePage } from './pages/HomePage.js';
import { SignInPage, type SignInPageProps } from './pages/SignInPage.js';
import { Button, Notice } from './ui/Primitives.js';
import type { SessionView } from '@ores/wire-protocol/browser';

/**
 * The route table, and the bootstrap gate.
 *
 * The gate is the whole reason this is a function of two states rather than a
 * component that reads them: "while the system is in bootstrap mode, the setup
 * page is the only page" is a rule, and a rule a test cannot call is a rule
 * nobody has checked. `ConnectedApp` below is the wiring.
 *
 * A deployment in bootstrap mode has nobody to sign in as, so every path
 * renders the first run journey rather than redirecting to a setup path: there
 * is nothing else to be at, and a redirect leaves a URL somebody can share that
 * leads nowhere. The journey outlives that mode — creating the administrator
 * closes it — so the gate carries its second reason to stay: the journey has
 * begun in this browser, and it stays on the rail until it finishes.
 */
export interface AppRoutesProps {
    readonly gate: BootstrapState;
    readonly session: SessionState;
    /** The first run journey, which only the wiring can reach the server for. */
    readonly journey: ReactNode;
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

    if (gate.inBootstrapMode || journeyInProgress) {
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
                path="/"
                element={signedIn(gate.version, session, onSignOut, (view) => (
                    <HomePage
                        username={view.username}
                        email={view.email}
                        tenantName={view.tenantName}
                        partyName={view.party.name}
                    />
                ))}
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
    const [journeyInProgress, setJourneyInProgress] = useState(false);

    return (
        <AppRoutes
            gate={gate}
            session={session}
            journey={
                <FirstRunJourney
                    server={server}
                    onStarted={() => setJourneyInProgress(true)}
                    onFinished={() => setJourneyInProgress(false)}
                />
            }
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
): ReactNode {
    if (session.status !== 'authenticated') {
        return <Navigate to="/login" replace />;
    }
    const view = session.session;
    return (
        <AppShell
            username={view.username}
            tenantName={view.tenantName}
            partyName={view.party.name}
            serverVersion={serverVersion}
            onSignOut={onSignOut}
        >
            {screen(view)}
        </AppShell>
    );
}

function Centred({ children }: { readonly children: ReactNode }): ReactNode {
    return <div className="grid min-h-full place-items-center bg-bg-primary px-5">{children}</div>;
}
