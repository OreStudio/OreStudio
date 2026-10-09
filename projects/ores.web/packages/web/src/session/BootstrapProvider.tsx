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

import { useQuery } from '@tanstack/react-query';
import { createContext, use, useMemo, type ReactNode } from 'react';
import { api } from '../api/client.js';
import { ApiFailure } from '../api/transport.js';

/**
 * Whether the deployment is still waiting for its first administrator.
 *
 * The interface reads this before it renders anything, because it decides what
 * can be offered: in bootstrap mode there is no sign-in, and the setup page is
 * the only screen. The rule itself is in `AppRoutes`, so it can be tested
 * without a server; this holds the state and the way to ask again.
 *
 * A failure is a state, not a fall-through. Answering "not in bootstrap mode"
 * because the server did not reply would show a sign-in form for a deployment
 * that may have no accounts, which is the confusion the flag exists to
 * prevent.
 */

export type BootstrapState =
    | { readonly status: 'loading' }
    | {
          readonly status: 'ready';
          readonly inBootstrapMode: boolean;
          /**
           * Whether the deployment has a tenant of its own.
           *
           * An installation is set up when it has an administrator and a
           * tenant, and the two facts arrive together because the screen that
           * finishes the job is the same screen in both cases.
           */
          readonly hasTenant: boolean;
          /**
           * Whether the system provisioner wizard recorded that it finished.
           *
           * A first-run installation may keep only the system tenant, so this
           * is the fact that lets it leave the setup screen; hasTenant alone
           * would hold it there forever.
           */
          readonly onboardingComplete: boolean;
          /**
           * Whether the tenant the caller signed in to finished its own setup.
           *
           * A provisioned tenant's setup is a run the tenant owns, and the run
           * clears its flag when it ends. A session in a tenant whose flag is
           * unset is held on the tenant setup screen.
           */
          readonly onboardingTenantComplete: boolean;
          /**
           * The account those two flags were read for, or empty.
           *
           * They are settings, and settings are not readable without a session,
           * while the rest of this answer is read without one. Comparing this
           * with the session in hand is how a screen tells an answer about the
           * visitor from one about the account that has just signed in.
           */
          readonly accountId: string;
          /**
           * Whether the request that answered carried a session cookie.
           *
           * The two flags above are read through a session, and a browser whose
           * session has ended still presents one. A screen reads this beside the
           * account: nothing presented means the answer is about the visitor and
           * is asked for again once somebody signs in, while something presented
           * that resolves to no account means the browser's session is over and
           * it belongs at the sign-in form.
           */
          readonly sessionPresent: boolean;
          readonly message: string;
          /** The build the deployment answered with, which every shell states. */
          readonly version: string;
      }
    | { readonly status: 'unreachable'; readonly reason: string };

interface BootstrapContextValue {
    readonly state: BootstrapState;
    /** Ask the server again, after the administrator has been created. */
    readonly recheck: () => Promise<void>;
    /**
     * Whether an answer is being read right now.
     *
     * The answer carries facts about a session, so a screen reconciling it with
     * the session in hand has to tell an answer that has arrived from one that is
     * on its way: judging the answer it already holds while a fresh one is in
     * flight reads a stale fact as a verdict about the browser.
     */
    readonly reading: boolean;
}

const BootstrapContext = createContext<BootstrapContextValue | undefined>(undefined);

export function useBootstrap(): BootstrapContextValue {
    const value = use(BootstrapContext);
    if (value === undefined) {
        throw new Error('useBootstrap must be used inside BootstrapProvider');
    }
    return value;
}

export const BOOTSTRAP_QUERY_KEY = ['bootstrap'] as const;

export function BootstrapProvider({ children }: { readonly children: ReactNode }): ReactNode {
    const query = useQuery({
        queryKey: BOOTSTRAP_QUERY_KEY,
        queryFn: api.bootstrapStatus,
        // The answer changes once, when the first administrator is created, and
        // the setup journey asks for it again rather than waiting for it to go
        // stale.
        staleTime: Infinity,
        retry: false,
    });

    const { data, isPending, isError, isFetching, error, refetch } = query;

    const value = useMemo<BootstrapContextValue>(() => {
        const recheck = async (): Promise<void> => {
            await refetch();
        };
        if (isPending) {
            return { state: { status: 'loading' }, recheck, reading: isFetching };
        }
        if (isError) {
            const reason =
                error instanceof ApiFailure
                    ? `${error.status}: ${error.message}`
                    : error instanceof Error
                      ? error.message
                      : String(error);
            return { state: { status: 'unreachable', reason }, recheck, reading: isFetching };
        }
        return {
            state: {
                status: 'ready',
                inBootstrapMode: data.isInBootstrapMode,
                hasTenant: data.hasTenant,
                onboardingComplete: data.onboardingComplete,
                onboardingTenantComplete: data.onboardingTenantComplete,
                accountId: data.accountId,
                sessionPresent: data.sessionPresent,
                message: data.message,
                version: data.version,
            },
            recheck,
            reading: isFetching,
        };
    }, [data, isPending, isError, isFetching, error, refetch]);

    return <BootstrapContext value={value}>{children}</BootstrapContext>;
}
