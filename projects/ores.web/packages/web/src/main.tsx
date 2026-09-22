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

import { StrictMode, type ReactNode } from 'react';
import { createRoot } from 'react-dom/client';
import { BrowserRouter, Navigate, Route, Routes } from 'react-router';
import { AppProviders, SessionProvider, createQueryClient, useSession } from './session/SessionProvider.js';
import { TranslationProvider } from './i18n/Provider.js';
import { AppShell } from './components/AppShell.js';
import { PublicShell } from './components/PublicShell.js';
import { HomePage, PlannedPage } from './components/pages/HomePage.js';
import { ComponentPage } from './components/pages/ComponentPage.js';
import { COMPONENTS, PLATFORM_COMPONENTS } from './components/registry.js';
import { humanise } from './components/labels.js';
import { LandingPage } from './pages/LandingPage.js';
import { SignUpPage } from './pages/SignUpPage.js';
import { DeveloperPage } from './pages/DeveloperPage.js';
import { SignInPage } from './pages/SignInPage.js';
import { entityRoutes } from './entity/entityRoutes.js';
import './styles.css';

/**
 * Application entry point.
 *
 * The environment is fixed when the process starts, so there is nothing here
 * about choosing where to connect. A visitor lands, signs in if they have an
 * account, and that is the whole journey.
 */
const queryClient = createQueryClient();

/** The sign-in screen, which gets out of the way once there is a session. */
function SignInRoute(): ReactNode {
  const { state } = useSession();
  return state.status === 'authenticated' ? <Navigate to="/" replace /> : <SignInPage />;
}

/** Wraps a screen so it is only reachable with a session. */
function guarded(element: ReactNode): ReactNode {
  return <Guarded>{element}</Guarded>;
}

function Guarded({ children }: { readonly children: ReactNode }): ReactNode {
  const { state } = useSession();
  return state.status === 'authenticated' ? children : <Navigate to="/login" replace />;
}

/**
 * A screen for an entity whose own screen is not built yet.
 *
 * The routes exist for every declared entity so the navigation is complete and a
 * person can see the shape of the system. The ones without a screen say so
 * instead of rendering an empty table.
 */
function plannedRoutes(): readonly ReactNode[] {
  return COMPONENTS.flatMap((component) =>
    component.entities
      .filter((entity) => entity.planned === true)
      .map((entity) => (
        <Route
          key={`${component.id}/${entity.id}`}
          path={`${component.path}/${entity.path}`}
          element={guarded(<PlannedPage title={humanise(entity.id)} />)}
        />
      )),
  );
}

function App(): ReactNode {
  const { state } = useSession();

  if (state.status === 'loading') {
    return (
      <div className="grid min-h-full place-items-center bg-bg-primary">
        <span className="text-sm text-ink-faint">Loading...</span>
      </div>
    );
  }

  const authenticated = state.status === 'authenticated';

  return (
    <Routes>
      {/*
        Two shells, because the two situations have nothing in common. A visitor
        gets a hero and one action; a signed-in person gets navigation.
      */}
      {authenticated ? (
        <Route element={<AppShell />}>
          <Route path="/" element={<HomePage />} />

          {/* A landing page per component, with its tasks and its entities. */}
          {COMPONENTS.map((component) => (
            <Route
              key={component.id}
              path={component.path}
              element={<ComponentPage />}
            />
          ))}
          {PLATFORM_COMPONENTS.map((component) => (
            <Route
              key={component.id}
              path={`${component.path}/deployment`}
              element={<DeveloperPage />}
            />
          ))}

          {plannedRoutes()}

          {/*
            Every wired entity's screens, taken from the registry rather than
            listed here. A declaration carries its descriptor, so declaring an
            entity is what routes it: the sidebar, the breadcrumbs and these
            routes read one list, and an entity that is declared cannot be one
            the router does not know.
          */}
          {COMPONENTS.flatMap((component) =>
            component.entities.flatMap((entity) =>
              entity.descriptor === undefined
                ? []
                : entityRoutes(entity.descriptor, component.path),
            ),
          )}
        </Route>
      ) : (
        <>
          <Route
            path="/"
            element={
              <PublicShell>
                <LandingPage />
              </PublicShell>
            }
          />
          <Route
            path="/signup"
            element={
              <PublicShell>
                <SignUpPage />
              </PublicShell>
            }
          />
          <Route path="/login" element={<SignInRoute />} />
        </>
      )}

      {/* A signed-in person asking for the sign-in screen is already in. */}
      <Route path="/login" element={<Navigate to="/" replace />} />
      <Route path="*" element={<Navigate to="/" replace />} />
    </Routes>
  );
}

const container = document.getElementById('root');
if (container === null) {
  throw new Error('missing #root element');
}

createRoot(container).render(
  <StrictMode>
    <AppProviders queryClient={queryClient}>
      <TranslationProvider>
        <SessionProvider>
          <BrowserRouter>
            <App />
          </BrowserRouter>
        </SessionProvider>
      </TranslationProvider>
    </AppProviders>
  </StrictMode>,
);
