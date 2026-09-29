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
 * What a form proposes, and the shape a tenant code must have.
 *
 * These are proposals, not rules: the server refuses a code that does not hold
 * the shape below, so the form applies the same shape while a person types and
 * never offers a value the server will reject. The rest is the convenience the
 * old client had — a display name proposes a code, a code proposes a hostname —
 * and each one stops the moment somebody types in the field it proposed.
 */

/** The longest tenant code the server accepts. */
export const TENANT_CODE_MAX_LENGTH = 50;

/** Whether a code is one the server will accept. */
export function isTenantCode(code: string): boolean {
    return /^[a-z][a-z0-9_]{0,49}$/.test(code);
}

/**
 * The code a display name proposes.
 *
 * Lowercased; runs of spaces and hyphens become underscores; everything else
 * that is not a lowercase letter, a digit or an underscore is dropped; a
 * leading non-letter is stripped, because a code starts with a letter; and the
 * result is truncated to the length the server accepts.
 */
export function codeFromName(name: string): string {
    const slug = name
        .trim()
        .toLowerCase()
        .replace(/[ -]+/g, '_')
        .replace(/[^a-z0-9_]/g, '');
    const first = slug.search(/[a-z]/);
    return (first < 0 ? '' : slug.slice(first)).slice(0, TENANT_CODE_MAX_LENGTH);
}

/** The hostname a code proposes. A deployment serves the tenant on its code. */
export function hostnameFromCode(code: string): string {
    return code;
}

/** The administrator's address, from the name it signs in with and the tenant. */
export function emailFromPrincipal(principal: string, hostname: string): string {
    if (principal === '' || hostname === '') {
        return '';
    }
    return `${principal}@${hostname}`;
}
