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
 * Primitive types shared across the protocol layer.
 *
 * The server serialises UUIDs as lowercase hyphenated strings. Timestamps are
 * not here: they have one owner, `time.ts`, which mirrors the server's wire
 * shape. See `doc/knowledge/architecture/time_architecture.org`.
 */

declare const uuidBrand: unique symbol;

/** A UUID in the canonical lowercase hyphenated form. */
export type Uuid = string & { readonly [uuidBrand]: 'Uuid' };

const UUID_PATTERN = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/;

/**
 * Validates and brands a UUID string.
 *
 * @throws {TypeError} when the input is not a canonical lowercase UUID.
 */
export function uuid(value: string): Uuid {
    if (!UUID_PATTERN.test(value)) {
        throw new TypeError(`Not a canonical UUID: ${JSON.stringify(value)}`);
    }
    return value as Uuid;
}

/** Narrows an arbitrary string to {@link Uuid} without throwing. */
export function isUuid(value: string): value is Uuid {
    return UUID_PATTERN.test(value);
}

/** The system tenant, {@code ffffffff-ffff-ffff-ffff-ffffffffffff}. */
export const SYSTEM_TENANT_ID = uuid('ffffffff-ffff-ffff-ffff-ffffffffffff');
