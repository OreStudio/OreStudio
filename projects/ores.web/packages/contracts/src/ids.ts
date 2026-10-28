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

import { z } from 'zod';

/**
 * The identifier shape the store actually writes.
 *
 * A row identifier is a 8-4-4-4-12 hexadecimal string and nothing more: the
 * store's random generator does not always set the RFC 4122 version and variant
 * nibbles, so a series written by the demo's feeder carries a version nibble of
 * any value. `z.uuid()` checks those nibbles, so it refuses rows the store
 * really holds and a screen that parses with it fails on a request error.
 *
 * This is the one place the shape is stated. A route or a screen that reads an
 * identifier from the store parses it with this schema and not with `z.uuid()`.
 */
export const STORE_UUID_PATTERN = /^[0-9a-fA-F]{8}-([0-9a-fA-F]{4}-){3}[0-9a-fA-F]{12}$/;

/**
 * A row identifier, in whatever nibbles the store wrote it.
 *
 * Surrounding space is trimmed because a shell or a form may hand one over
 * with it; nothing else about the value is narrowed.
 */
export const storeUuidSchema = z.string().trim().regex(STORE_UUID_PATTERN);

/** Whether a value is a row identifier as the store writes one. */
export function isStoreUuid(value: string): boolean {
    return STORE_UUID_PATTERN.test(value.trim());
}
