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

import { useState, type ReactNode } from 'react';

/**
 * Where the browser fetches an image.
 *
 * An image of the session's own tenant is read as the session. An image of a
 * tenant read from system administration is named with that tenant, because
 * the BFF reads it inside the tenant that holds it.
 */
export function imageUrl(imageId: string, tenantCode?: string): string {
    const id = encodeURIComponent(imageId);
    return tenantCode === undefined
        ? `/api/images/${id}`
        : `/api/tenants/${encodeURIComponent(tenantCode)}/images/${id}`;
}

function initialsOf(name: string): string {
    const words = name.split(/[\s._-]+/).filter((word) => word !== '');
    return words
        .slice(0, 2)
        .map((word) => word[0]?.toUpperCase() ?? '')
        .join('');
}

/**
 * A person's picture, or their initials when they have none.
 *
 * A picture that fails to load falls back to the initials too, so a row never
 * shows a broken image. The failure is remembered for that picture only, so a
 * row that is reused for another person tries the new one.
 */
export function Avatar({
    name,
    src,
    size = 'md',
}: {
    readonly name: string;
    readonly src: string | null;
    readonly size?: 'sm' | 'md' | 'lg';
}): ReactNode {
    const box = { sm: 'h-5 w-5 text-[9px]', md: 'h-8 w-8 text-[11px]', lg: 'h-14 w-14 text-base' }[
        size
    ];
    const [failedSrc, setFailedSrc] = useState<string | null>(null);
    if (src !== null && src !== failedSrc) {
        return (
            <img
                src={src}
                alt=""
                loading="lazy"
                onError={() => setFailedSrc(src)}
                className={`${box} shrink-0 rounded-full object-cover`}
            />
        );
    }
    return (
        <span
            aria-hidden="true"
            className={`${box} flex shrink-0 items-center justify-center rounded-full bg-surface-overlay font-medium text-ink-muted`}
        >
            {initialsOf(name)}
        </span>
    );
}

/**
 * The picture of an account of the session's own tenant, named by username,
 * or its initials.
 *
 * Wherever a screen names an account it shows its picture, and most places
 * know a username rather than an image: the signed-in person, who gave a role.
 */
export function AccountPicture({
    username,
    name,
    size = 'md',
}: {
    readonly username: string;
    readonly name: string;
    readonly size?: 'sm' | 'md' | 'lg';
}): ReactNode {
    return (
        <Avatar
            name={name}
            size={size}
            src={`/api/accounts/${encodeURIComponent(username)}/picture`}
        />
    );
}
