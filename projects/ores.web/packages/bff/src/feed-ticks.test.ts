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
import { FeedTickBuffers, type TickSubscriber } from './feed-ticks.js';

function subscriber(): {
    readonly subscribe: TickSubscriber;
    readonly open: () => readonly string[];
    readonly publish: (subject: string, value: string) => void;
} {
    const listeners = new Map<
        string,
        (tick: { value: string; observation_time: string }) => void
    >();
    return {
        subscribe: (subject, onTick) => {
            listeners.set(subject, onTick);
            return () => listeners.delete(subject);
        },
        open: () => [...listeners.keys()],
        publish: (subject, value) =>
            listeners.get(subject)?.({ value, observation_time: '2026-10-11 09:00:00Z' }),
    };
}

describe('the tick buffers', () => {
    it('hold no more runs than the feeds running, and close every subscription of a stopped run', () => {
        const bus = subscriber();
        const buffers = new FeedTickBuffers(bus.subscribe);
        buffers.running(['a', 'b', 'c']);
        expect(buffers.size).toBe(3);
        buffers.running(['b']);
        expect(buffers.size).toBe(1);
        expect(bus.open()).toEqual(['synthetic.v1.ops.tick.b']);
    });

    it('keeps the newest ticks up to its capacity', () => {
        const bus = subscriber();
        const buffers = new FeedTickBuffers(bus.subscribe, 2);
        buffers.running(['a']);
        for (const value of ['1', '2', '3']) bus.publish('synthetic.v1.ops.tick.a', value);
        expect(buffers.ticks('a', 10).map((tick) => tick.value)).toEqual([2, 3]);
    });

    it('keeps the runs every session states, since the service answers what runs for the whole process', () => {
        const bus = subscriber();
        const buffers = new FeedTickBuffers(bus.subscribe);
        buffers.running(['a', 'b']);
        bus.publish('synthetic.v1.ops.tick.a', '1');
        buffers.running(['a', 'b']);
        expect(buffers.ticks('a', 10).map((tick) => tick.value)).toEqual([1]);
    });
});
