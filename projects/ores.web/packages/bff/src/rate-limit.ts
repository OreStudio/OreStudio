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

/**
 * A per-key rate limiter for the login route.
 *
 * The IAM service already counts failed attempts and locks an account, so this
 * is not the security boundary. It stops a stuck client or a trivial script
 * from driving that counter with unbounded traffic.
 *
 * The map is bounded. A client that never returns still leaves a key behind,
 * so a sweep drops every key whose attempts have all aged out once the map
 * reaches the tracking cap, and a key is evicted outright when the sweep
 * cannot get below it. Without that the process accumulates one entry per
 * client address it has ever seen, for as long as it runs.
 */

export interface RateLimiter {
  /** True when the caller may proceed; false when it must wait. */
  allow(key: string): boolean;
  readonly trackedKeys: number;
}

export interface RateLimiterOptions {
  readonly maxAttempts: number;
  readonly windowSeconds: number;
  /** Distinct clients tracked before the sweep runs. */
  readonly maxTrackedKeys?: number;
  readonly now?: () => number;
}

const DEFAULT_MAX_TRACKED_KEYS = 4096;

export function createRateLimiter(options: RateLimiterOptions): RateLimiter {
  const attempts = new Map<string, number[]>();
  const windowMs = options.windowSeconds * 1000;
  const maxTrackedKeys = options.maxTrackedKeys ?? DEFAULT_MAX_TRACKED_KEYS;
  const now = options.now ?? (() => Date.now());

  /** Drops every key with no attempt left inside the window. */
  function sweep(timestamp: number): void {
    for (const [key, recorded] of attempts) {
      const recent = recorded.filter((recordedAt) => timestamp - recordedAt < windowMs);
      if (recent.length === 0) {
        attempts.delete(key);
      } else {
        attempts.set(key, recent);
      }
    }
  }

  /** Drops the key whose most recent attempt is oldest. */
  function evictOldest(): void {
    let oldestKey: string | undefined;
    let oldestAt = Number.POSITIVE_INFINITY;
    for (const [key, recorded] of attempts) {
      const last = recorded[recorded.length - 1] ?? Number.NEGATIVE_INFINITY;
      if (last < oldestAt) {
        oldestAt = last;
        oldestKey = key;
      }
    }
    if (oldestKey !== undefined) {
      attempts.delete(oldestKey);
    }
  }

  return {
    allow(key) {
      const timestamp = now();
      const tracked = attempts.get(key);
      if (tracked === undefined && attempts.size >= maxTrackedKeys) {
        sweep(timestamp);
        if (attempts.size >= maxTrackedKeys) {
          evictOldest();
        }
      }
      const recent = (attempts.get(key) ?? []).filter(
        (recordedAt) => timestamp - recordedAt < windowMs,
      );
      if (recent.length >= options.maxAttempts) {
        attempts.set(key, recent);
        return false;
      }
      recent.push(timestamp);
      attempts.set(key, recent);
      return true;
    },
    get trackedKeys() {
      return attempts.size;
    },
  };
}
