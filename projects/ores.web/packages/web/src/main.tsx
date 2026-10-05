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

import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import { BrowserRouter } from 'react-router';
import { AppProviders, SessionProvider, createQueryClient } from './session/SessionProvider.js';
import { BootstrapProvider } from './session/BootstrapProvider.js';
import { TranslationProvider } from './i18n/Provider.js';
import { ConnectedApp } from './AppRoutes.js';
import { isPrototypePath, PrototypeApp } from './prototype/PrototypeApp.js';
import './styles.css';

/**
 * Application entry point.
 *
 * The environment is fixed when the process starts, so there is nothing here
 * about choosing where to connect. The providers are the whole of the wiring:
 * one query client, the translations, the session, the bootstrap gate, and the
 * router. What renders is `ConnectedApp`, which is the route table and the gate
 * the story decided on.
 */
const queryClient = createQueryClient();

const container = document.getElementById('root');
if (container === null) {
    throw new Error('missing #root element');
}

createRoot(container).render(
    <StrictMode>
        <AppProviders queryClient={queryClient}>
            <TranslationProvider>
                <SessionProvider>
                    {/* Inside the session, because the gate decides what the
                        session may even be used for, and inside the query
                        client, because asking is a query. */}
                    <BootstrapProvider>
                        <BrowserRouter>
                            {/* PROTOTYPE. Throwaway. Delete with the branch.
                                A prototype path is answered in place of the
                                application, so a reviewer opens one URL. The
                                providers above it still mount; nothing on a
                                prototype path reads them. */}
                            {isPrototypePath(window.location.pathname) ? (
                                <PrototypeApp pathname={window.location.pathname} />
                            ) : (
                                <ConnectedApp />
                            )}
                        </BrowserRouter>
                    </BootstrapProvider>
                </SessionProvider>
            </TranslationProvider>
        </AppProviders>
    </StrictMode>,
);
