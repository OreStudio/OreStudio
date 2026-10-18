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

import { toWireTimestamp, type WireTimestamp } from '@ores/wire-protocol';
import type { SeriesWindow } from '@ores/contracts';

/**
 * How far back each window reaches, in seconds. A month is thirty days and a
 * year is three hundred and sixty five, so a window is a length and not a
 * calendar period: a read names one instant and the server compares the
 * observation times against it. The page labels these two windows by the span
 * they reach, so the words a person reads name the length these numbers set.
 */
const WINDOW_SECONDS: Readonly<Record<Exclude<SeriesWindow, 'all'>, number>> = {
    hour: 60 * 60,
    day: 24 * 60 * 60,
    month: 30 * 24 * 60 * 60,
    year: 365 * 24 * 60 * 60,
};

/**
 * The earliest observation a window reads, or nothing when it reads all of
 * them.
 *
 * The instant is taken from @p now, the deployment's clock, because the
 * observation times were stamped against that clock and not the browser's.
 * The bound is the wire's own spelling, which the server reads as an instant.
 */
export function observedSince(window: SeriesWindow, now: number): WireTimestamp | null {
    if (window === 'all') return null;
    return toWireTimestamp(new Date(now - WINDOW_SECONDS[window] * 1000));
}
