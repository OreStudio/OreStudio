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
 */

export interface RateLimiter {
  /** True when the caller may proceed; false when it must wait. */
  allow(key: string): boolean;
  readonly trackedKeys: number;
}

export interface RateLimiterOptions {
  readonly maxAttempts: number;
  readonly windowSeconds: number;
  readonly now?: () => number;
}

export function createRateLimiter(options: RateLimiterOptions): RateLimiter {
  const attempts = new Map<string, number[]>();
  const windowMs = options.windowSeconds * 1000;
  const now = options.now ?? (() => Date.now());

  return {
    allow(key) {
      const timestamp = now();
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
