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
import { createRateLimiter } from './rate-limit.js';

describe('createRateLimiter', () => {
  it('allows attempts up to the budget', () => {
    const limiter = createRateLimiter({ maxAttempts: 3, windowSeconds: 60, now: () => 0 });
    expect([limiter.allow('a'), limiter.allow('a'), limiter.allow('a')]).toEqual([true, true, true]);
    expect(limiter.allow('a')).toBe(false);
  });

  it('tracks callers independently', () => {
    const limiter = createRateLimiter({ maxAttempts: 1, windowSeconds: 60, now: () => 0 });
    expect(limiter.allow('a')).toBe(true);
    expect(limiter.allow('b')).toBe(true);
    expect(limiter.allow('a')).toBe(false);
  });

  it('forgets attempts once the window has passed', () => {
    let clock = 0;
    const limiter = createRateLimiter({ maxAttempts: 1, windowSeconds: 60, now: () => clock });
    expect(limiter.allow('a')).toBe(true);
    expect(limiter.allow('a')).toBe(false);
    clock = 61_000;
    expect(limiter.allow('a')).toBe(true);
  });

  it('does not grow the map for a caller that keeps trying', () => {
    const limiter = createRateLimiter({ maxAttempts: 2, windowSeconds: 60, now: () => 0 });
    for (let attempt = 0; attempt < 50; attempt += 1) {
      limiter.allow('a');
    }
    expect(limiter.trackedKeys).toBe(1);
  });

  it('sweeps callers whose attempts have aged out', () => {
    let clock = 0;
    const limiter = createRateLimiter({
      maxAttempts: 1,
      windowSeconds: 60,
      maxTrackedKeys: 3,
      now: () => clock,
    });
    for (const key of ['a', 'b', 'c']) {
      expect(limiter.allow(key)).toBe(true);
    }
    clock = 61_000;
    expect(limiter.allow('d')).toBe(true);
    expect(limiter.trackedKeys).toBe(1);
  });

  it('holds the map at the cap when no caller has aged out', () => {
    const limiter = createRateLimiter({
      maxAttempts: 1,
      windowSeconds: 60,
      maxTrackedKeys: 3,
      now: () => 0,
    });
    for (const key of ['a', 'b', 'c', 'd', 'e', 'f']) {
      expect(limiter.allow(key)).toBe(true);
    }
    expect(limiter.trackedKeys).toBe(3);
  });
});
