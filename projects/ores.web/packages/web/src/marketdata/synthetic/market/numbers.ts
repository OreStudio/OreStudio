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
 */

/**
 * A number as a person would write it: six significant figures at most and no
 * float noise, so 0.0016666666666666668 reads 0.00166667 and 1440 reads 1440.
 */
export function clean(value: number, significant = 6): string {
    if (!Number.isFinite(value)) {
        return '—';
    }
    return String(Number(value.toPrecision(significant)));
}

/** A count as a narrow column holds it: 11500 reads 11.5k. */
export function compact(value: number): string {
    return Math.abs(value) >= 1000 ? `${clean(value / 1000, 3)}k` : clean(value, 3);
}

/** A span of seconds in the unit a person would say it in: 600 reads 10 min. */
export function duration(seconds: number): string {
    if (!Number.isFinite(seconds)) {
        return '—';
    }
    if (seconds >= 3600) {
        return `${clean(seconds / 3600, 3)} h`;
    }
    if (seconds >= 60) {
        return `${clean(seconds / 60, 3)} min`;
    }
    if (seconds >= 1) {
        return `${clean(seconds, 3)} s`;
    }
    return `${clean(seconds * 1000, 3)} ms`;
}

/** A multiplier as the market screens print one: ×0.5, ×2. */
export function times(value: number, significant = 3): string {
    return `×${clean(value, significant)}`;
}
