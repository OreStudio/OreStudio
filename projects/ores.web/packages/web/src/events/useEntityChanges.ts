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

import { useEffect, useRef, useState } from 'react';
import { useEntityChangeEvents, type WatchedEntity } from './EntityEvents.js';

/** The part of a query the hook needs to know when the screen last loaded. */
export interface LoadState {
    /** When the data on screen arrived, in milliseconds. */
    readonly dataUpdatedAt: number;
    /** Whether a load is in flight. */
    readonly isFetching: boolean;
}

/**
 * Whether what a screen shows is older than a change it has heard of.
 *
 * A change is news when it reached the browser after the load that produced the
 * data began. It is compared on the browser's own clock at both ends, because the
 * server's clock and the browser's differ, and a comparison across them makes a
 * screen stale for a skew or fresh for one. A change that arrives while a load is
 * in flight counts, because that load may have read before it. The cost is one
 * refresh that finds nothing new, which is the safe direction.
 */
export function isStale(changeArrivedAt: number | undefined, loadStartedAt: number): boolean {
    return changeArrivedAt !== undefined && changeArrivedAt > loadStartedAt;
}

/**
 * Tells a screen that the entities it shows have changed on the server.
 *
 * `stale` is true once a change has been heard after the data on screen began to
 * load, and stays true until the screen loads again. `changedAt` is the server's
 * time of the newest change, for a tooltip that says what happened. The hook
 * reloads nothing: the screen decides, usually by showing a Refresh button that
 * asks to be used.
 */
export function useEntityChanges(
    watches: readonly WatchedEntity[],
    load: LoadState,
): { readonly stale: boolean; readonly changedAt: string | undefined } {
    const [heard, setHeard] = useState<{ arrivedAt: number; at: string } | undefined>();
    const inFlightSince = useRef(Date.now());
    const [loadStartedAt, setLoadStartedAt] = useState(inFlightSince.current);
    const wasFetching = useRef(load.isFetching);

    // The start of a load is when fetching turns on; it is the data's age that
    // matters, so the start is kept until that load's data arrives.
    useEffect(() => {
        if (load.isFetching && !wasFetching.current) inFlightSince.current = Date.now();
        wasFetching.current = load.isFetching;
    }, [load.isFetching]);

    useEffect(() => {
        setLoadStartedAt(inFlightSince.current);
    }, [load.dataUpdatedAt]);

    useEntityChangeEvents(watches, (change) => {
        setHeard({ arrivedAt: Date.now(), at: change.at });
    });

    return {
        stale: isStale(heard?.arrivedAt, loadStartedAt),
        changedAt: heard?.at,
    };
}
