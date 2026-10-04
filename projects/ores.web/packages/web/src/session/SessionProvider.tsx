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

import { QueryClient, QueryClientProvider, useQuery, useQueryClient } from '@tanstack/react-query';
import {
    createContext,
    use,
    useCallback,
    useEffect,
    useMemo,
    useState,
    type ReactNode,
} from 'react';
import { api } from '../api/client.js';
import { ApiFailure } from '../api/transport.js';
import type { PartySummary, SessionView } from '@ores/wire-protocol/browser';

/**
 * Session state for the whole application.
 *
 * The browser holds an opaque cookie rather than a token, so it cannot decide
 * locally whether it is signed in. One query answers that on load, and every
 * component reads the result from here instead of fetching again.
 *
 * A 401 is an expected state rather than a failure, so it resolves to "no
 * session" and routes the user to the sign-in screen.
 */

export type SessionState =
    | { readonly status: 'loading' }
    | { readonly status: 'anonymous' }
    | { readonly status: 'authenticated'; readonly session: SessionView };

/**
 * A login that still needs a party, or one that completed.
 *
 * The forced-change flag travels with both, because the login answer carries
 * it: a screen that has just signed somebody in is the screen that has to ask
 * them to set a password of their own.
 */
export type SignInOutcome =
    | { readonly outcome: 'active'; readonly passwordResetRequired: boolean }
    | {
          readonly outcome: 'party-required';
          readonly parties: readonly PartySummary[];
          readonly passwordResetRequired: boolean;
      };

interface SessionContextValue {
    readonly state: SessionState;
    readonly signIn: (credentials: {
        username: string;
        password: string;
    }) => Promise<SignInOutcome>;
    readonly chooseParty: (partyId: string, parties: readonly PartySummary[]) => Promise<void>;
    /**
     * Re-scopes the open session to another of the account's own parties.
     *
     * The party list is the server's answer here rather than a list the caller
     * passes, because the party may have been added after the login and no
     * caller holds a complete list.
     */
    readonly switchParty: (partyId: string) => Promise<void>;
    readonly signOut: () => Promise<void>;
}

const SessionContext = createContext<SessionContextValue | undefined>(undefined);

/**
 * How long an access token lasts, in seconds, or nothing outside a signed-in
 * session. A screen that states how soon a change reaches people already
 * signed in reads it here rather than assuming a number.
 */
export function useAccessLifetimeSeconds(): number | undefined {
    const value = use(SessionContext);
    return value?.state.status === 'authenticated'
        ? value.state.session.accessLifetimeSeconds
        : undefined;
}

export function useSession(): SessionContextValue {
    const value = use(SessionContext);
    if (value === undefined) {
        throw new Error('useSession must be used inside SessionProvider');
    }
    return value;
}

export const SESSION_QUERY_KEY = ['session'] as const;

export function createQueryClient(): QueryClient {
    const client: QueryClient = new QueryClient({
        defaultOptions: {
            queries: {
                // A 4xx is an expected answer, not a transient fault, so retrying it
                // only delays the redirect to the sign-in screen.
                retry: (failureCount, error) =>
                    !(error instanceof ApiFailure && error.status < 500) && failureCount < 2,
                staleTime: 30_000,
                refetchOnWindowFocus: true,
            },
        },
    });
    return client;
}

export function AppProviders({
    children,
    queryClient,
}: {
    readonly children: ReactNode;
    readonly queryClient: QueryClient;
}): ReactNode {
    return <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>;
}

export function SessionProvider({ children }: { readonly children: ReactNode }): ReactNode {
    const queryClient = useQueryClient();
    const [state, setState] = useState<SessionState>({ status: 'loading' });

    const sessionQuery = useQuery({
        queryKey: SESSION_QUERY_KEY,
        queryFn: api.session,
    });
    const { data, isPending, isError, error } = sessionQuery;

    useEffect(() => {
        if (isPending) {
            return;
        }
        if (isError) {
            // A failure that is not "no session" is still a failure to load. Treat
            // it as anonymous so the user gets a sign-in screen and a clear retry
            // rather than a blank page.
            setState({ status: 'anonymous' });
            return;
        }
        setState(
            data === null || data === undefined
                ? { status: 'anonymous' }
                : { status: 'authenticated', session: data },
        );
    }, [data, isError, isPending, error]);

    const signIn = useCallback<SessionContextValue['signIn']>(
        async (credentials) => {
            const result = await api.login(credentials);
            if (result.outcome === 'active') {
                queryClient.setQueryData(SESSION_QUERY_KEY, result.session);
                setState({ status: 'authenticated', session: result.session });
                return {
                    outcome: 'active',
                    passwordResetRequired: result.session.passwordResetRequired,
                };
            }
            // A pending selection is not a session yet, so nothing is cached. The
            // sign-in screen renders the picker from this return value.
            return {
                outcome: 'party-required',
                parties: result.availableParties,
                passwordResetRequired: result.passwordResetRequired,
            };
        },
        [queryClient],
    );

    const chooseParty = useCallback<SessionContextValue['chooseParty']>(
        async (partyId, parties) => {
            const session = await api.selectParty(partyId);
            // The login reply offers fewer fields than the session view, so the
            // party list is carried forward from the sign-in response.
            const merged: SessionView = {
                ...session,
                availableParties: [...parties],
            };
            queryClient.setQueryData(SESSION_QUERY_KEY, merged);
            setState({ status: 'authenticated', session: merged });
        },
        [queryClient],
    );

    const switchParty = useCallback<SessionContextValue['switchParty']>(
        async (partyId) => {
            const session = await api.switchParty(partyId);
            queryClient.setQueryData(SESSION_QUERY_KEY, session);
            setState({ status: 'authenticated', session });
        },
        [queryClient],
    );

    const signOut = useCallback<SessionContextValue['signOut']>(async () => {
        await api.logout();
        queryClient.removeQueries({
            predicate: (query) => query.queryKey[0] !== SESSION_QUERY_KEY[0],
        });
        queryClient.setQueryData(SESSION_QUERY_KEY, null);
        setState({ status: 'anonymous' });
    }, [queryClient]);

    const value = useMemo<SessionContextValue>(
        () => ({
            state,
            signIn,
            chooseParty,
            switchParty,
            signOut,
        }),
        [state, signIn, chooseParty, switchParty, signOut],
    );

    return <SessionContext value={value}>{children}</SessionContext>;
}
