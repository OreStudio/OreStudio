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
import { cx } from './Primitives.js';

/**
 * A bar of presets: the horizons a read is asked over.
 *
 * The chosen preset is pressed, so a person reads the horizon in force without
 * reading a caption, and the bar is the same everywhere it appears: the grid's
 * charts and a store's plot ask the same question in the same shape.
 */
export function PresetBar<T extends string>({
    label,
    presets,
    value,
    labelOf,
    onChange,
}: {
    /** What the choice is, for a reader who cannot see the bar. */
    readonly label: string;
    readonly presets: readonly T[];
    readonly value: T;
    readonly labelOf: (preset: T) => string;
    readonly onChange: (preset: T) => void;
}): ReactNode {
    return (
        <div
            role="group"
            aria-label={label}
            className="inline-flex overflow-hidden rounded-md border border-line"
        >
            {presets.map((preset) => (
                <button
                    key={preset}
                    type="button"
                    aria-pressed={value === preset}
                    className={cx(
                        'px-3 py-1 text-sm',
                        value === preset ? 'bg-accent text-ink-inverse' : 'text-ink-muted',
                    )}
                    onClick={() => onChange(preset)}
                >
                    {labelOf(preset)}
                </button>
            ))}
        </div>
    );
}
