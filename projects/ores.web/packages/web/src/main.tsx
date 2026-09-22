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
import { COMPONENTS, PLATFORM_COMPONENTS, iamComponent } from './components/registry.js';
import { humanise } from './components/labels.js';
import { LandingPage } from './pages/LandingPage.js';
import { SignUpPage } from './pages/SignUpPage.js';
import { DeveloperPage } from './pages/DeveloperPage.js';
import { SignInPage } from './pages/SignInPage.js';
import { AccountsPage } from './pages/AccountsPage.js';
import { FirstRunJourneyPrototype, NewTenantJourneyPrototype } from './pages/prototype/newTenantJourney/NewTenantJourneyPrototype.js';
import { CountryListPage } from './components/refdata/entities/country/CountryListPage.js';
import { CountryDetailPage } from './components/refdata/entities/country/CountryDetailPage.js';
import { CountryHistoryPage } from './components/refdata/entities/country/CountryHistoryPage.js';
import { entityRoutes } from './entity/entityRoutes.js';
import { tenantTypeDescriptor } from './generated/iam/web/tenant_type_declaration.js';
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
          <Route path="/iam/account" element={<AccountsPage />} />
          {/* PROTOTYPE ONLY: the new tenant journey variants. */}
          <Route path="/iam/tenant/onboard" element={<NewTenantJourneyPrototype />} />

          {/* The first entity on the shared machinery. Every other entity will
              look exactly like this: a list page and a detail page, both thin.
              The identity is the natural key, so :id is the alpha-2 code. */}
          <Route path="/refdata/country" element={<CountryListPage />} />

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
            Entity detail routes. React Router 8 does not support a regular
            expression in a path, so an unknown code is handled by the screen
            rather than excluded by the route: a record that is not found says so,
            which is better than a route that matches nothing and shows nothing.
          */}
          <Route path="/refdata/country/new" element={<CountryDetailPage mode="create" />} />
          <Route path="/refdata/country/:id" element={<CountryDetailPage mode="read" />} />
          <Route path="/refdata/country/:id/edit" element={<CountryDetailPage mode="edit" />} />
          <Route path="/refdata/country/:id/history" element={<CountryHistoryPage />} />

          {/*
            An entity on the shared machinery is one declaration and one line
            here. The factory reads the declaration: the paths, the screens and
            which of the five routes exist are all derived, so adding an entity is
            a model change rather than five more routes.
          */}
          {entityRoutes(tenantTypeDescriptor, iamComponent.path)}
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
          {/* PROTOTYPE ONLY: first run, the page a system in bootstrap mode shows. */}
          <Route
            path="/setup"
            element={
              <PublicShell>
                <FirstRunJourneyPrototype />
              </PublicShell>
            }
          />
          {/* PROTOTYPE ONLY: stubbed, so it needs no session. */}
          <Route
            path="/iam/tenant/onboard"
            element={
              <PublicShell>
                <NewTenantJourneyPrototype />
              </PublicShell>
            }
          />
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
