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

import { createContext, use, useEffect, useMemo, useRef, type ReactNode } from 'react';
import { api } from '../api/client.js';
import { EntityEventStream, type ChangeListener } from './EntityEventStream.js';
import { mayConcern, type RecordKey } from './recordKey.js';

const EntityEventsContext = createContext<EntityEventStream | undefined>(undefined);

/**
 * Holds the session's one change stream for the screens under it.
 *
 * It sits inside the signed-in shell, so the stream ends with the session.
 */
export function EntityEventsProvider({ children }: { readonly children: ReactNode }): ReactNode {
    const stream = useMemo(
        () =>
            new EntityEventStream(
                () => new EventSource('/api/events', { withCredentials: true }),
                (watches) => api.watchEntities(watches),
            ),
        [],
    );
    useEffect(() => () => stream.close(), [stream]);
    return <EntityEventsContext value={stream}>{children}</EntityEventsContext>;
}

/** An entity a screen shows, as the event subject names it: the events collection. */
export interface WatchedEntity {
    readonly component: string;
    readonly entity: string;
    /**
     * The one record a screen shows, by the entity's declared key such as
     * `{ username }`. A screen that names it hears of a change only when the
     * change may concern that record. A screen that shows many records, or does
     * not know the key's columns, leaves it out and hears every change.
     */
    readonly record?: RecordKey;
}

/**
 * Calls back when any of the entities changes on the server.
 *
 * The entity is the events collection as the services name it, such as
 * `accounts`. A screen outside a provider hears nothing, which is how it behaved
 * before there was a stream.
 */
export function useEntityChangeEvents(
    watches: readonly WatchedEntity[],
    onChange: ChangeListener,
): void {
    const stream = use(EntityEventsContext);
    const latest = useRef(onChange);
    latest.current = onChange;
    // The list is a new array on every render, so what is watched is what its
    // names say, and the subscription is renewed only when they change.
    const names = JSON.stringify(
        watches.map((watch) => [watch.component, watch.entity, watch.record ?? null]),
    );
    useEffect(() => {
        if (stream === undefined) return undefined;
        const parsed = JSON.parse(names) as [string, string, RecordKey | null][];
        const stops = parsed.map(([component, entity, record]) =>
            stream.watch(component, entity, (change) => {
                if (record !== null && !mayConcern(change.keys, record)) return;
                latest.current(change);
            }),
        );
        return () => {
            for (const stop of stops) stop();
        };
    }, [stream, names]);
}
