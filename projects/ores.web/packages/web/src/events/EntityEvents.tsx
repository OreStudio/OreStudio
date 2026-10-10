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
    const names = watches.map((watch) => `${watch.component}\u0000${watch.entity}`).join('\u0001');
    useEffect(() => {
        if (stream === undefined || names === '') return undefined;
        const stops = names.split('\u0001').map((name) => {
            const [component = '', entity = ''] = name.split('\u0000');
            return stream.watch(component, entity, (change) => {
                latest.current(change);
            });
        });
        return () => {
            for (const stop of stops) stop();
        };
    }, [stream, names]);
}
