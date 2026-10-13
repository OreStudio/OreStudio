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

/** How often a feed ticks, as the unit and the number a person reads: every 2 s, every 1.5 min. */
export function frequencyOf(ticksPerHour: number): {
    readonly unit: 's' | 'min' | 'h';
    readonly n: string;
} {
    if (ticksPerHour <= 0) {
        return { unit: 'h', n: '0' };
    }
    const seconds = 3600 / ticksPerHour;
    const [unit, value] =
        seconds < 60
            ? (['s', seconds] as const)
            : seconds < 3600
              ? (['min', seconds / 60] as const)
              : (['h', seconds / 3600] as const);
    return { unit, n: String(Math.round(value * 10) / 10) };
}
