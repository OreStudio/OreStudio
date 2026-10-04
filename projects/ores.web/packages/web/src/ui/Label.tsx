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

import type { ReactNode } from 'react';
import type { BadgePresentation } from '@ores/wire-protocol/browser';

/**
 * A value drawn as the label the shared catalogue gives it.
 *
 * The colours, the tooltip and the severity are the badge's, and the words
 * are the caller's, so a value reads the same wherever it appears. A value with
 * no badge, or a badge with no colour, is drawn as plain muted text: it is
 * still the value, and hiding it would hide the interesting case.
 */
export function Label({
    text,
    badge,
    title,
}: {
    readonly text: string;
    readonly badge: BadgePresentation | undefined;
    readonly title?: string | undefined;
}): ReactNode {
    if (badge === undefined || badge.backgroundColour === '') {
        return <span className="text-ink-muted">{text}</span>;
    }
    const tooltip = title ?? (badge.description === '' ? undefined : badge.description);
    return (
        <span
            className="inline-block rounded-full px-2 py-0.5 text-[11px] leading-tight"
            style={{ backgroundColor: badge.backgroundColour, color: badge.textColour }}
            {...(tooltip === undefined ? {} : { title: tooltip })}
        >
            {text}
        </span>
    );
}
