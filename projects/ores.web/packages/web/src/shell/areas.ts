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

import type { SessionMode } from '@ores/wire-protocol/browser';

/**
 * Where the journeys live, per mode.
 *
 * The areas are what the shell's menu holds and the journeys are what the cards
 * hold, and both are the client's business: the journey *New tenant* lives
 * under *Tenants* because that is where a person looks for it. The server
 * answers what the session may do; it does not answer where a screen goes, and
 * a menu the server dictated would have to change every time a journey moved.
 *
 * So this module is the structure, and it is data rather than markup for one
 * reason: a later group adds its journeys by adding rows here, and the shell
 * that draws them does not move.
 *
 * A journey without `to` is one the catalogue holds and the tree has not built.
 * It is drawn as not built rather than hidden, because an area that is empty
 * because nothing was extracted for it and an area that is empty because the
 * work has not landed are two different things, and a reader should be able to
 * tell them apart. A journey that is not in this table is not offered at all.
 *
 * A mode with no row is a mode whose journeys no group has implemented yet.
 * The shell draws what the session's own state says, and nothing finer: the
 * difference between a privileged and a regular party user is the permissions
 * they hold, and until the session carries them the application mode declares
 * no areas rather than guessing at the ones it cannot prove.
 */
export interface ShellJourney {
    /** The catalogue's name for the journey, as a message key. */
    readonly nameKey: string;
    /** The route it opens, absent while the tree has not built it. */
    readonly to?: string;
}

export interface ShellArea {
    readonly nameKey: string;
    readonly journeys: readonly ShellJourney[];
}

export const SHELL_AREAS: Readonly<Partial<Record<SessionMode, readonly ShellArea[]>>> = {
    'system-administration': [
        {
            nameKey: 'shell.area.tenants',
            journeys: [
                { nameKey: 'shell.journey.newTenant', to: '/tenants/new' },
                { nameKey: 'shell.journey.retireTenant' },
            ],
        },
    ],
};

/** The areas this mode shows, which is none until a group implements them. */
export function areasFor(mode: SessionMode): readonly ShellArea[] {
    return SHELL_AREAS[mode] ?? [];
}

/**
 * The message that names a mode.
 *
 * The mode is a value on the wire, so it is also a message key rather than a
 * sentence: the server states the context and the interface says it in the
 * reader's language.
 */
export function modeKey(mode: SessionMode): string {
    return `shell.mode.${mode}`;
}
