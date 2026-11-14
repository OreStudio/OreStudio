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
import { useOptionalSession } from '../session/SessionProvider.js';

/**
 * What the desk screens share: the as-of they send, the filter they hold in the
 * address, the party they read for, and the geometry of their tables.
 *
 * The party is one fact per screen rather than one per read, because the service
 * answers for the session's party and no read takes one: a screen that named a
 * party it did not read would be inventing it.
 */

/** The address an as-of date travels as: the whole day, as the server reads it. */
export function asOfInstant(date: string): string {
    return `${date} 23:59:59Z`;
}

/** Today, as an address holds it. */
export function todayIso(now: Date = new Date()): string {
    return now.toISOString().slice(0, 10);
}

/** One of an allowed set, or the fallback when the address states something else. */
export function oneOf<T extends string>(
    value: string | null,
    allowed: readonly T[],
    fallback: T,
): T {
    return allowed.find((candidate) => candidate === value) ?? fallback;
}

/** A table's header cell, and its data cell: the geometry every desk table shares. */
export const HEAD_CELL = 'px-2 py-1 text-right text-xs font-semibold text-ink-faint';
export const DATA_CELL = 'px-2 py-1 text-right font-mono whitespace-nowrap';

/** The party the signed-in person is reading for, or nothing outside a session. */
export function useReadParty(): string {
    const session = useOptionalSession();
    return session?.state.status === 'authenticated' ? session.state.session.party.name : '';
}
