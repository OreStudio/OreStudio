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

/** A stand-in for the vendor subject a source's ticks land on. */
const on = (source: string): string[] => [
    `marketdata.v1.ops.market_tick.demo.*.fx_spot.${source}.*`,
];

describe('the tick buffers', () => {
    it('opens a subscription on each subject a source names, once', () => {
        const bus = subscriber();
        const buffers = new FeedTickBuffers(bus.subscribe);
        buffers.watch('a', on('a'));
        buffers.watch('b', on('b'));
        buffers.watch('a', on('a'));
        expect(buffers.size).toBe(2);
        expect(bus.open()).toEqual([...on('a'), ...on('b')]);
    });

    it('keeps the newest ticks up to its capacity', () => {
        const bus = subscriber();
        const buffers = new FeedTickBuffers(bus.subscribe, 2);
        buffers.watch('a', on('a'));
        for (const value of ['1', '2', '3']) bus.publish(on('a')[0]!, value);
        expect(buffers.ticks('a', 10).map((tick) => tick.value)).toEqual([2, 3]);
    });

    it('keeps the ticks it has heard when a read asks for the source again', () => {
        const bus = subscriber();
        const buffers = new FeedTickBuffers(bus.subscribe);
        buffers.watch('a', on('a'));
        bus.publish(on('a')[0]!, '1');
        buffers.watch('a', on('a'));
        expect(buffers.ticks('a', 10).map((tick) => tick.value)).toEqual([1]);
    });

    it("keeps a stopped feed's ticks, and follows a feed whose subjects change", () => {
        const bus = subscriber();
        const buffers = new FeedTickBuffers(bus.subscribe);
        buffers.watch('a', on('a'));
        bus.publish(on('a')[0]!, '1');
        buffers.watch('a', []);
        expect(bus.open()).toEqual(on('a'));
        const moved = ['marketdata.v1.ops.market_tick.ice.*.fx_spot.a.*'];
        buffers.watch('a', moved);
        expect(bus.open()).toEqual(moved);
        bus.publish(moved[0]!, '2');
        expect(buffers.ticks('a', 10).map((tick) => tick.value)).toEqual([1, 2]);
        buffers.watch('b', []);
        expect(buffers.size).toBe(2);
        expect(bus.open()).toEqual(moved);
    });

    it("keeps a source whole when a subscribe fails, and ignores the subjects' order", () => {
        const bus = subscriber();
        const refused = new Set<string>();
        const buffers = new FeedTickBuffers((subject, onTick) => {
            if (refused.has(subject)) throw new Error('not connected');
            return bus.subscribe(subject, onTick);
        });
        buffers.watch('a', ['a.2', 'a.1']);
        buffers.watch('a', ['a.1', 'a.2']);
        expect(bus.open()).toEqual(['a.1', 'a.2']);
        refused.add('b.2');
        expect(() => buffers.watch('a', ['b.1', 'b.2'])).toThrow('not connected');
        expect(bus.open()).toEqual(['a.1', 'a.2']);
        refused.clear();
        buffers.watch('a', ['b.1', 'b.2']);
        expect(bus.open()).toEqual(['b.1', 'b.2']);
    });
});
