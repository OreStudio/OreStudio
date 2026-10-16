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
import type { ListedSeries } from '@ores/contracts';
import { bindings } from '../../api/bindings.js';
import { oresmdQueries } from '../../oresmd/queries.js';
import { useActingPartyId } from '../../session/SessionProvider.js';
import { useRunningFeeds } from '../synthetic/feedRuns.js';
import { activityByName, statusOf, type BindingStatus } from './bindingView.js';

/** How often the screens ask what the bindings have written. */
const ACTIVITY_EVERY_MS = 10_000;

/** What the bindings screens know about a binding beyond its row. */
export interface BindingFacts {
    /** The status of a binding, or nothing while the running feeds are not yet known. */
    readonly statusOf: (binding: {
        readonly source_name: string;
        readonly producer_kind: string;
        readonly enabled: boolean;
    }) => BindingStatus | undefined;
    /** The series of the newest observation of a binding's source, when the party holds it. */
    readonly seriesOf: (sourceName: string) => ListedSeries | undefined;
    /** When the binding's source last wrote, or empty text. */
    readonly lastTickOf: (sourceName: string) => string;
}

/** The activity of the bindings and the party's series, read together for a list or a record. */
export function useBindingFacts(): BindingFacts {
    const partyId = useActingPartyId();
    const activity = useQuery({
        queryKey: ['bindings', 'activity'],
        queryFn: bindings.activity,
        refetchInterval: ACTIVITY_EVERY_MS,
    });
    const list = useQuery({
        ...oresmdQueries.seriesList(partyId ?? ''),
        enabled: partyId !== undefined,
    });
    const running = useRunningFeeds();
    const byName = activityByName(activity.data?.activities ?? []);
    const held = new Map((list.data?.series ?? []).map((one) => [one.series_id, one]));
    const now = Date.now();
    return {
        statusOf: (binding) =>
            activity.data === undefined
                ? undefined
                : statusOf({
                      kind: binding.producer_kind,
                      enabled: binding.enabled,
                      running: running?.has(binding.source_name),
                      lastTickAt: byName.get(binding.source_name)?.last_tick_at ?? '',
                      now,
                  }),
        seriesOf: (sourceName) => held.get(byName.get(sourceName)?.series_id ?? ''),
        lastTickOf: (sourceName) => byName.get(sourceName)?.last_tick_at ?? '',
    };
}
