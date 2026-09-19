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

import { createContext, use, useEffect, useMemo, useRef, useState, type ReactNode } from 'react';

/**
 * What the server says has changed, and when this browser heard it.
 *
 * One stream per session rather than one per screen: a screen opening and closing
 * must not churn connections, and a person with six lists open wants one stream.
 * Screens declare what they are watching and the server decides what to send.
 *
 * The time recorded is when this browser heard the news, on this browser's clock,
 * and that is deliberate. The event carries the server's own timestamp, but
 * comparing it against a load time would compare two clocks, and a deployment
 * where those disagree would either miss changes or report them forever. What
 * matters is only whether a change arrived after the data was loaded, and both
 * sides of that question can be answered on one clock.
 *
 * A newer event replaces the time rather than accumulating. A thousand changes
 * from one import all say the same thing — that what is on screen is old — and
 * the first says it as well as the last. That is what keeps a bulk operation from
 * becoming a thousand pieces of work.
 */

/**
 * When an entity changed, in both clocks.
 *
 * `heardAt` is this browser's clock and answers whether a change arrived after
 * the data was loaded. `serverAt` is the service's and answers which rows
 * changed, because the rows carry the service's timestamps and marking them
 * needs the same clock they were written in.
 */
interface ChangeTime {
  readonly heardAt: number;
  readonly serverAt: string;
}

/*
 * The records are not carried.
 *
 * The event declares them and the services do not fill them in, so anything built
 * on them would be a count of nothing. Which rows changed is worked out from the
 * rows themselves instead, which is the weaker account and the one that works.
 */

type ChangeTimes = ReadonlyMap<string, ChangeTime>;

interface ChangeEventsValue {
  /** When each entity last changed, by this browser's clock. */
  readonly times: ChangeTimes;
  /** Declares what the current screen is watching. */
  readonly watch: (watches: readonly string[]) => void;
  /** Forgets what has been reported, because it has now been loaded. */
  readonly clear: (component: string, entity: string) => void;
}

const ChangeEventsContext = createContext<ChangeEventsValue | undefined>(undefined);

function key(component: string, entity: string): string {
  return `${component}.${entity}`;
}

export function ChangeEventsProvider({ children }: { readonly children: ReactNode }): ReactNode {
  const [times, setTimes] = useState<ChangeTimes>(() => new Map());
  const watched = useRef<readonly string[]>([]);

  useEffect(() => {
    const source = new EventSource('/api/events');

    source.addEventListener('entity-changed', (event) => {
      const parsed = JSON.parse((event as MessageEvent<string>).data) as {
        component?: string;
        entity?: string;
        at?: string;
      };
      if (parsed.component === undefined || parsed.entity === undefined) return;
      const changed = key(parsed.component, parsed.entity);
      setTimes((current) => {
        const previous = current.get(changed);
        const next = new Map(current);
        next.set(changed, {
          heardAt: Date.now(),
          serverAt: parsed.at ?? previous?.serverAt ?? '',
        });
        return next;
      });
    });

    /*
     * Re-declared whenever the watch set changes, because the stream is one-way
     * and the server cannot ask. Sent from the effect rather than from the hook
     * that wants it so that one place owns the declaration, and sent again on
     * reconnect because a reconnected stream is a server that has forgotten.
     */
    const declare = (): void => {
      if (watched.current.length === 0) return;
      void fetch('/api/events/watch', {
        method: 'POST',
        headers: { 'content-type': 'application/json' },
        body: JSON.stringify({
          watches: watched.current.map((entry) => {
            const [component = '', entity = ''] = entry.split('.');
            return { component, entity };
          }),
        }),
      }).catch(() => undefined);
    };

    source.addEventListener('connected', declare);
    source.addEventListener('open', declare);

    return () => source.close();
  }, []);

  const value = useMemo<ChangeEventsValue>(
    () => ({
      times,
      clear: (component, entity) => {
        setTimes((current) => {
          const existing = current.get(key(component, entity));
          if (existing === undefined) return current;
          const next = new Map(current);
          next.delete(key(component, entity));
          return next;
        });
      },
      watch: (watches) => {
        watched.current = watches;
        // A screen that has just arrived is watching something new.
        void fetch('/api/events/watch', {
          method: 'POST',
          headers: { 'content-type': 'application/json' },
          body: JSON.stringify({
            watches: watches.map((entry) => {
              const [component = '', entity = ''] = entry.split('.');
              return { component, entity };
            }),
          }),
        }).catch(() => undefined);
      },
    }),
    [times],
  );

  return <ChangeEventsContext value={value}>{children}</ChangeEventsContext>;
}

/**
 * The service's timestamp of the last change to an entity.
 *
 * Which rows changed is worked out by comparing this against each row's own
 * `recorded_at`, so both sides of that comparison are written by the same clock.
 */
export function useEntityChangedAt(component: string, entity: string): string | undefined {
  const context = use(ChangeEventsContext);
  if (component.length === 0 || entity.length === 0) return undefined;
  return context?.times.get(key(component, entity))?.serverAt;
}

/**
 * Whether what a screen is showing is older than what exists.
 *
 * `loadedAt` is when the screen's data was loaded, on this browser's clock, which
 * React Query already knows. Nothing reloads on its own: the screen is told it is
 * out of date and a person decides when to bring the changes in.
 */
export interface EntityChanges {
  /** Whether what is on screen is older than what exists. */
  readonly stale: boolean;
  /** Says the changes have been loaded, so the news is no longer news. */
  readonly clear: () => void;
}

export function useEntityChanged(
  component: string,
  entity: string,
  loadedAt: number,
): boolean {
  return useEntityChanges(component, entity, loadedAt).stale;
}

/** What a screen knows about changes to the entity it is showing. */
export function useEntityChanges(
  component: string,
  entity: string,
  loadedAt: number,
): EntityChanges {
  const context = use(ChangeEventsContext);
  const key0 = key(component, entity);

  // Declared as watched for as long as the screen is up, and no longer. A screen
  // that named nothing watches nothing rather than declaring an empty watch.
  const keyed = useMemo(
    () => (component.length === 0 || entity.length === 0 ? [] : [key0]),
    [key0, component, entity],
  );
  useEffect(() => {
    if (keyed.length > 0) context?.watch(keyed);
  }, [context, keyed]);

  const empty: EntityChanges = useMemo(
    () => ({ stale: false, clear: () => undefined }),
    [],
  );

  if (context === undefined || loadedAt === 0) return empty;
  if (component.length === 0 || entity.length === 0) return empty;

  const changedAt = context.times.get(key0);
  if (changedAt === undefined || changedAt.heardAt <= loadedAt) return empty;

  return {
    stale: true,
    clear: () => context.clear(component, entity),
  };
}
