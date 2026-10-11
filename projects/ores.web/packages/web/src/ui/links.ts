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

/** The link that sends mail to an address, present only when there is an address to send to. */
export function mailtoLink(address: string): { readonly href?: string } {
    const trimmed = address.trim();
    return trimmed === '' ? {} : { href: `mailto:${trimmed}` };
}

/**
 * The link that opens a web page, present only when the value is an http or
 * https address. A value that is not one, such as free text, stays text rather
 * than a link that goes nowhere. It opens in a new tab and keeps the opener
 * out of the page it opens.
 */
export function webLink(address: string): { readonly href?: string; readonly external?: boolean } {
    const trimmed = address.trim();
    try {
        const url = new URL(trimmed);
        return url.protocol === 'http:' || url.protocol === 'https:'
            ? { href: url.href, external: true }
            : {};
    } catch {
        return {};
    }
}
