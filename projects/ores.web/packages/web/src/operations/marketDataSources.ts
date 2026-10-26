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
import { useQuery } from '@tanstack/react-query';
import { api, type AdapterStatus } from '../api/client.js';
import type { Translator } from '../i18n/translate.js';
import { ADAPTERS_EVERY_MS, ADAPTERS_KEY } from '../marketdata/synthetic/adapterStatus.js';

/**
 * The board the system administrator reads to keep the market data sources
 * running, from
 * doc/knowledge/journeys/operations/journey_keep-the-market-data-feeds-running.org.
 *
 * One adapter is one source. The market data service's status is the truth of
 * what a source serves, and it is the only read this screen makes: everything
 * below is that reply, counted and named. What the reply does not carry is
 * stated as a gap rather than filled from another screen's data.
 */

/**
 * How long a serving source may go without a tick before the board calls it
 * quiet, in milliseconds.
 *
 * The journey defines quiet against the interval a source's demand implies, and
 * the status does not carry that interval, so the board states one window for
 * every source and says so in its note.
 */
export const QUIET_AFTER_MS = 300_000;

/**
 * The states the adapter registry announces, in its own words, from
 * ores.marketdata.service/app/adapter_registry.hpp. The tuple is the union a new
 * state must join, so a state the read gains is a compile error here.
 */
export const SOURCE_STATES = ['announced', 'ready', 'serving', 'stale', 'rejected'] as const;
export type SourceState = (typeof SOURCE_STATES)[number];

/** The tone one state is painted with, for every state the registry emits. */
export type SourceTone = 'up' | 'muted' | 'warn' | 'down';

/** The state the read emitted, or nothing for a word the registry does not emit. */
export function knownState(state: string): SourceState | undefined {
    return SOURCE_STATES.find((known) => known === state);
}

/** A serving source ticks; one the registry calls stale or rejected is the trouble the board is for. */
export function stateTone(state: string): SourceTone {
    switch (knownState(state)) {
        case 'serving':
            return 'up';
        case 'announced':
        case 'ready':
            return 'muted';
        case 'stale':
            return 'warn';
        case 'rejected':
            return 'down';
        default:
            return 'warn';
    }
}

/** One id a provider publishes, in the vendor's own notation. */
export interface VendorId {
    readonly id_scheme: string;
    readonly external_id: string;
    readonly field: string;
}

/** One id an adapter refused, with the reason it gave. */
export interface RejectedId {
    readonly id: VendorId;
    readonly reason: string;
}

/** One source as the board reads it: the adapter, its state, and the counts of its last demand. */
export interface SourceRow {
    /** The name of the source, which is the id of its adapter. */
    readonly name: string;
    readonly provider: string;
    readonly state: string;
    /** The last sign of life the service recorded, as the wire states it. */
    readonly heartbeatAt: string;
    readonly heartbeatCounter: number;
    /** How many routes the service holds for this source. */
    readonly routes: number;
    /** How many ids the service demanded of it. */
    readonly demanded: number;
    /** How many of the demanded ids the adapter accepted, and how many it refused. */
    readonly accepted: number;
    readonly rejected: number;
    readonly ticksPerMinute: number;
    /** How long ago the adapter published its last tick, or -1 for none. */
    readonly lastTickAgeMs: number;
    /** How many of the adapter's announced ids the catalogue knows, and how many it does not. */
    readonly served: number;
    readonly unknown: number;
    readonly rejectedIds: readonly RejectedId[];
    /** Serving, demanded, and nothing has ticked inside the window. */
    readonly quiet: boolean;
    /** Demanded ids the adapter did not accept. */
    readonly notServed: number;
}

/** How the board orders a source: the one that ticked longest ago stands first. */
function ageForOrder(lastTickAgeMs: number): number {
    return lastTickAgeMs < 0 ? Number.POSITIVE_INFINITY : lastTickAgeMs;
}

/** One source, read from one adapter. */
export function sourceOf(adapter: AdapterStatus): SourceRow {
    const lastTickAgeMs = adapter.last_tick_age_ms;
    return {
        name: adapter.adapter_id,
        provider: adapter.provider_code,
        state: adapter.state,
        heartbeatAt: adapter.last_heartbeat_at,
        heartbeatCounter: adapter.heartbeat_counter,
        routes: adapter.routes_seen,
        demanded: adapter.demanded_count,
        accepted: adapter.accepted.length,
        rejected: adapter.rejected.length,
        ticksPerMinute: adapter.ticks_last_minute,
        lastTickAgeMs,
        served: adapter.served_count,
        unknown: adapter.unknown_count,
        rejectedIds: adapter.rejected.map((entry) => ({
            id: entry.id,
            reason: entry.reason,
        })),
        quiet:
            adapter.state === 'serving' &&
            adapter.demanded_count > 0 &&
            (lastTickAgeMs < 0 || lastTickAgeMs > QUIET_AFTER_MS),
        notServed: Math.max(0, adapter.demanded_count - adapter.accepted.length),
    };
}

/** Every source, the one that ticked longest ago first, so the quiet one is at the top. */
export function sourceRows(adapters: readonly AdapterStatus[]): readonly SourceRow[] {
    return adapters
        .map(sourceOf)
        .sort((left, right) => ageForOrder(right.lastTickAgeMs) - ageForOrder(left.lastTickAgeMs));
}

/** The figures the tiles state, every one of them counted from the same rows. */
export interface SourceFigures {
    readonly sources: number;
    readonly serving: number;
    readonly routes: number;
    readonly ticksPerMinute: number;
    readonly quiet: number;
    /** Demanded ids no adapter accepted: what the alert is for. */
    readonly notServed: number;
    /** Sources the service demands nothing of, so they publish nothing. */
    readonly withoutDemand: number;
    readonly stale: number;
}

export function sourceFigures(rows: readonly SourceRow[]): SourceFigures {
    return {
        sources: rows.length,
        serving: rows.filter((row) => row.state === 'serving').length,
        routes: rows.reduce((count, row) => count + row.routes, 0),
        ticksPerMinute: rows.reduce((count, row) => count + row.ticksPerMinute, 0),
        quiet: rows.filter((row) => row.quiet).length,
        notServed: rows.reduce((count, row) => count + row.notServed, 0),
        withoutDemand: rows.filter((row) => row.demanded === 0).length,
        stale: rows.filter((row) => row.state === 'stale').length,
    };
}

/**
 * How long ago a source ticked, in the units a person reads.
 *
 * The status states an age rather than an instant, so the board cannot draw it
 * as a timestamp with the exact value on hover; it states the age the service
 * measured.
 */
export function formatTickAge(ms: number, t: Translator['t']): string {
    if (ms < 0) {
        return t('operations.sources.age.never');
    }
    if (ms < 1000) {
        return t('operations.sources.age.millis', { millis: ms });
    }
    const seconds = Math.round(ms / 1000);
    if (seconds < 90) {
        return t('operations.sources.age.seconds', { seconds });
    }
    const minutes = Math.round(seconds / 60);
    if (minutes < 90) {
        return t('operations.sources.age.minutes', { minutes });
    }
    return t('operations.sources.age.hours', { hours: Math.round(minutes / 60) });
}

/** One vendor id as a person reads it: the vendor's scheme, then the id and its field. */
export function formatVendorId(id: VendorId): string {
    const named = `${id.id_scheme}:${id.external_id}`;
    return id.field === '' ? named : `${named} ${id.field}`;
}

/** The adapter status of every source, read again while the screen is open. */
export function useAdapterStatuses() {
    return useQuery({
        queryKey: ADAPTERS_KEY,
        queryFn: api.adapterStatuses,
        refetchInterval: ADAPTERS_EVERY_MS,
    });
}

/** How many readings of a trend the board keeps, which is about two minutes. */
const TREND_POINTS = 24;

/**
 * The ticks per minute each source published while the screen was open.
 *
 * The status states one minute at a time and keeps no history, so a trend is
 * the board's own readings and starts empty when the screen opens. It says
 * nothing about the minutes before the person arrived.
 */
export function useTickTrends(
    adapters: readonly AdapterStatus[] | undefined,
): ReadonlyMap<string, readonly number[]> {
    const [trends, setTrends] = useState<ReadonlyMap<string, readonly number[]>>(new Map());
    const last = useRef<readonly AdapterStatus[] | undefined>(undefined);
    useEffect(() => {
        if (adapters === undefined || adapters === last.current) {
            return;
        }
        last.current = adapters;
        setTrends((held) => {
            const next = new Map<string, readonly number[]>();
            for (const adapter of adapters) {
                const points = [...(held.get(adapter.adapter_id) ?? []), adapter.ticks_last_minute];
                next.set(adapter.adapter_id, points.slice(-TREND_POINTS));
            }
            return next;
        });
    }, [adapters]);
    return trends;
}
