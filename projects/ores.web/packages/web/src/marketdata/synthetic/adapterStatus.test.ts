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

import { describe, expect, it } from 'vitest';
import type { AdapterStatus } from '../../api/client.js';
import {
    demandedNotServed,
    demandedNotServedAll,
    servesFeeds,
    sourcesOf,
} from './adapterStatus.js';

function adapter(provider: string, state: string, demanded: number): AdapterStatus {
    return {
        adapter_id: `${provider}-sim`,
        provider_code: provider,
        state,
        last_heartbeat_at: '2026-10-25 15:00:00Z',
        heartbeat_counter: 42,
        served_count: 26,
        unknown_count: 0,
        demanded_count: demanded,
        routes_seen: 3,
        ticks_last_minute: 60,
        last_tick_age_ms: 200,
        accepted: [],
        rejected: [],
    };
}

describe('the state of a provider source', () => {
    it('serves its feeds when it is serving and was demanded at least one id', () => {
        expect(servesFeeds(sourcesOf([adapter('b', 'serving', 3)]).get('b'))).toBe(true);
    });

    it('serves no feed when it is serving and was demanded nothing', () => {
        expect(servesFeeds(sourcesOf([adapter('b', 'serving', 0)]).get('b'))).toBe(false);
    });

    it('serves no feed while it is announced, ready, stale or rejected', () => {
        for (const state of ['announced', 'ready', 'stale', 'rejected']) {
            expect(servesFeeds(sourcesOf([adapter('b', state, 3)]).get('b'))).toBe(false);
        }
    });

    it('serves no feed for a provider the service tracks no adapter for', () => {
        expect(servesFeeds(sourcesOf([]).get('b'))).toBe(false);
    });

    it('counts the ids a provider was demanded and does not get served', () => {
        const sources = sourcesOf([adapter('b', 'ready', 17), adapter('o', 'serving', 4)]);
        expect(demandedNotServed(sources.get('b'))).toBe(17);
        expect(demandedNotServed(sources.get('o'))).toBe(0);
        expect(demandedNotServedAll(sources)).toBe(17);
    });

    it('reads a provider with two sources as serving while one serves, and serves all its demand', () => {
        const sources = sourcesOf([adapter('b', 'serving', 2), adapter('b', 'ready', 3)]);
        expect(sources.get('b')).toEqual({ serving: true, demanded: 5, ticksPerMinute: 120 });
        expect(demandedNotServed(sources.get('b'))).toBe(0);
        expect(demandedNotServedAll(sources)).toBe(0);
    });

    it('adds the ticks the sources of a provider published in the last minute', () => {
        const sources = sourcesOf([adapter('b', 'serving', 1), adapter('b', 'serving', 2)]);
        expect(sources.get('b')?.ticksPerMinute).toBe(120);
    });
});
