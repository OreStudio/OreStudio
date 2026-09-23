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
import { BrowserRouter } from 'react-router';
import { AppProviders, SessionProvider, createQueryClient, useSession, type SessionState } from './session/SessionProvider.js';
import { TranslationProvider } from './i18n/Provider.js';
import './styles.css';

/**
 * Application entry point.
 *
 * The environment is fixed when the process starts, so there is nothing here
 * about choosing where to connect. The providers stay composed as they were:
 * one query client, the translations, the session, and the router.
 */
const queryClient = createQueryClient();

/**
 * Reading the session is what makes the transport visible: the placeholder
 * looks the same whether the server answered or not, and a shell that cannot
 * tell the difference is not a shell anybody can debug.
 */
function sessionLine(state: SessionState): string {
  switch (state.status) {
    case 'authenticated':
      return `Signed in as ${state.session.username}.`;
    case 'loading':
      return 'Checking the session...';
    default:
      return 'No session.';
  }
}

/**
 * What the browser renders today.
 *
 * The interface is not built yet. There are no screens and no routes, so this
 * states that in one place rather than rendering nothing, which reads as a
 * broken deployment rather than an unbuilt one.
 */
function App(): ReactNode {
  const { state } = useSession();

  return (
    <main className="mx-auto max-w-[680px] px-5 py-16">
      <h1 className="text-xl font-semibold tracking-tight text-ink">ORE Studio</h1>
      <p className="mt-3 text-sm text-ink-muted">
        The interface is not built yet. This build is the application shell and
        the transport that talks to the server; no screens are wired to it.
      </p>
      <p className="mt-2 text-xs text-ink-faint">{sessionLine(state)}</p>
    </main>
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
