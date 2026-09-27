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
          readonly message: string;
      }
    | { readonly status: 'unreachable'; readonly reason: string };

interface BootstrapContextValue {
    readonly state: BootstrapState;
    /** Ask the server again, after the administrator has been created. */
    readonly recheck: () => Promise<void>;
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

    const { data, isPending, isError, error, refetch } = query;

    const value = useMemo<BootstrapContextValue>(() => {
        const recheck = async (): Promise<void> => {
            await refetch();
        };
        if (isPending) {
            return { state: { status: 'loading' }, recheck };
        }
        if (isError) {
            const reason =
                error instanceof ApiFailure
                    ? `${error.status}: ${error.message}`
                    : error instanceof Error
                      ? error.message
                      : String(error);
            return { state: { status: 'unreachable', reason }, recheck };
        }
        return {
            state: {
                status: 'ready',
                inBootstrapMode: data.isInBootstrapMode,
                message: data.message,
            },
            recheck,
        };
    }, [data, isPending, isError, error, refetch]);

    return <BootstrapContext value={value}>{children}</BootstrapContext>;
}
