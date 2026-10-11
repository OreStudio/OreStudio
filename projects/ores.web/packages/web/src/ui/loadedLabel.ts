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

function pad(value: number): string {
    return String(value).padStart(2, '0');
}

/**
 * A moment as an ISO 8601 timestamp in the reader's own zone, such as
 * `2026-10-11T02:13:10+01:00`.
 *
 * The offset is written, so the text names one instant wherever it is read, and
 * it is the same text in every language. The moment is the browser's own clock
 * at the time the data arrived, which is the clock the stale logic compares.
 */
export function loadedIso(at: number): string {
    const moment = new Date(at);
    const offset = -moment.getTimezoneOffset();
    const sign = offset < 0 ? '-' : '+';
    const hours = pad(Math.trunc(Math.abs(offset) / 60));
    const minutes = pad(Math.abs(offset) % 60);
    return (
        `${moment.getFullYear()}-${pad(moment.getMonth() + 1)}-${pad(moment.getDate())}` +
        `T${pad(moment.getHours())}:${pad(moment.getMinutes())}:${pad(moment.getSeconds())}` +
        `${sign}${hours}:${minutes}`
    );
}
