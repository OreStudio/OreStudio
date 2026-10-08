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

import type { InboxRequestView } from '@ores/wire-protocol/browser';

/**
 * The words a request is drawn with.
 *
 * The server states a kind and a state as codes, and the screen names them:
 * what a kind is called and how a state reads are the interface's business.
 * A code this build does not know falls back to the code itself, because a
 * request nobody can name is still a request somebody has to answer.
 */

export type Tone = 'neutral' | 'accent' | 'warn' | 'muted' | 'up' | 'down';

/** The six states a request may be in, each with the chip the prototype draws. */
// A state is painted by what it means to the person reading it. Waiting is
// the thing to look at, a grant is the outcome that went their way, a refusal
// is the one that did not, and the rest are neither: a withdrawn or lapsed
// request is simply over.
const STATE_TONES: Readonly<Record<string, Tone>> = {
    waiting: 'warn',
    held: 'accent',
    approved: 'up',
    refused: 'down',
    withdrawn: 'muted',
    expired: 'muted',
};

/** A state's name in the person's language, or its code when it has none. */
export function stateLabel(t: (key: string) => string, stateCode: string): string {
    const key = `inbox.state.${stateCode}`;
    const label = t(key);
    return label === key ? stateCode : label;
}

export function stateTone(stateCode: string): Tone {
    return STATE_TONES[stateCode] ?? 'muted';
}

/** A request kind's name in the person's language, or its code when it has none. */
export function kindLabel(t: (key: string) => string, kindCode: string): string {
    const key = `inbox.kind.${kindCode}`;
    const label = t(key);
    return label === key ? kindCode : label;
}

/**
 * What a request asks for, as a row names it.
 *
 * A person's own requests name the roles they asked for, because IAM answers
 * the read that asks for them. A request the reader may not read the roles of
 * says nothing about what it asks for, and the kind's label stands in.
 */
export function askedFor(
    t: (key: string) => string,
    request: Pick<InboxRequestView, 'kindCode' | 'roles'>,
): string {
    const names = request.roles.map((role) => role.name).filter((name) => name !== '');
    return names.length > 0 ? names.join(', ') : kindLabel(t, request.kindCode);
}
