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
import { qaImageUrl } from '../api/qa.js';
import { attachedImages } from './images.js';

/**
 * The images the note of a step links, drawn as pictures. A click opens the
 * picture itself in a new tab. A runner shows them small, under the note, and a
 * report shows them large, with the file name under each, because a reviewer
 * reads what the tester saw.
 */
export function AttachedImages({
    scenarioId,
    notes,
    large = false,
}: {
    readonly scenarioId: string;
    readonly notes: string;
    readonly large?: boolean;
}): ReactNode {
    const names = attachedImages(notes);
    if (names.length === 0) return null;
    return (
        <div className={large ? 'space-y-3' : 'flex flex-wrap gap-2'}>
            {names.map((name) => (
                <figure key={name} className="space-y-1">
                    <a
                        href={qaImageUrl(scenarioId, name)}
                        target="_blank"
                        rel="noopener noreferrer"
                        title={name}
                    >
                        <img
                            src={qaImageUrl(scenarioId, name)}
                            alt={name}
                            loading="lazy"
                            className={
                                large
                                    ? 'max-h-[28rem] max-w-full rounded-md border border-line'
                                    : 'max-h-32 rounded-md border border-line'
                            }
                        />
                    </a>
                    {large && (
                        <figcaption className="font-mono text-[11px] text-ink-faint">
                            {name}
                        </figcaption>
                    )}
                </figure>
            ))}
        </div>
    );
}
