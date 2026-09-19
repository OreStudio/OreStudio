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

import type { RequestHeaders } from './transport.js';

/**
 * A source of request headers.
 *
 * Passing a function keeps a caller's header values current across a refresh
 * without the caller having to rebuild them, which matters for the
 * `Nats-Session-Id` and `Nats-Correlation-Id` values that must stay stable for
 * a whole operation.
 */
export type HeaderSource = RequestHeaders | (() => RequestHeaders);

/** Reads a header source into a plain record. */
export function resolveHeaders(source: HeaderSource | undefined): RequestHeaders {
  if (source === undefined) {
    return {};
  }
  return typeof source === 'function' ? source() : source;
}
