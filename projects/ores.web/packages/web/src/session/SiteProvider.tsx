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
import type { EnvironmentView } from '@ores/contracts';
import { api } from '../api/client.js';

/**
 * Which environment this site serves.
 *
 * The environment is a fact about the deployment, not about the person using
 * it, so it is read once for the whole page rather than by every component
 * that states it. The read sits beside the bootstrap read because the two
 * arrive together, before a session exists. It is a separate context because
 * the questions are separate: the bootstrap flag decides what the interface
 * may even offer, while the environment only labels what it does offer, and a
 * failure of one must not withhold the other.
 *
 * A failure is a state, not a fall-through: the footer says the environment is
 * unknown rather than inventing a name, exactly as it does for the build the
 * deployment has not answered with yet.
 */

interface SiteContextValue {
    /**
     * The environment the deployment serves, or nothing before it answers.
     *
     * Undefined covers both a read that has not arrived and one that failed;
     * the footer states the same thing for either, because it cannot tell the
     * difference and must not guess.
     */
    readonly environment: EnvironmentView | undefined;
}

const SiteContext = createContext<SiteContextValue | undefined>(undefined);

export function useSite(): SiteContextValue {
    const value = use(SiteContext);
    if (value === undefined) {
        throw new Error('useSite must be used inside SiteProvider');
    }
    return value;
}

export const SITE_QUERY_KEY = ['site'] as const;

export function SiteProvider({ children }: { readonly children: ReactNode }): ReactNode {
    const { data } = useQuery({
        queryKey: SITE_QUERY_KEY,
        queryFn: api.site,
        // The environment is fixed when the deployment starts, so the answer
        // cannot change while a page is open.
        staleTime: Infinity,
        retry: false,
    });

    const value = useMemo<SiteContextValue>(() => ({ environment: data?.environment }), [data]);

    return <SiteContext value={value}>{children}</SiteContext>;
}
