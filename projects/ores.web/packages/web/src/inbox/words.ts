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

/**
 * The words a request is drawn with.
 *
 * The server states a kind and a state as codes, and the screen names them:
 * what a kind is called and how a state reads are the interface's business.
 * A code this build does not know falls back to the code itself, because a
 * request nobody can name is still a request somebody has to answer.
 */

export type Tone = 'neutral' | 'accent' | 'warn' | 'muted' | 'up' | 'down';

/** The states a request may be in, each with the chip the prototype draws. */
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
    apply_failed: 'down',
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
 * The table an event of a story came from, as a person reads it.
 *
 * A model's name is not a word anybody says. The story names the source by what
 * it contributed -- the request, the answer, the notice, the role -- and falls
 * back to the model's own name for a source this build does not know, because a
 * step nobody can name is still a step somebody has to read.
 */
export function sourceLabel(t: (key: string) => string, entityType: string): string {
    const key = `inbox.story.source.${entityType}`;
    const label = t(key);
    return label === key ? entityType : label;
}

/**
 * A part's name in the person's language, or the server's own name for it.
 *
 * A part is a function that must approve: controller, finance, market risk,
 * operations. The server names each part, and a tenant may add one this build
 * has no word for, so the code is only the first thing tried.
 */
export function partLabel(t: (key: string) => string, code: string, serverName = ''): string {
    const key = `inbox.part.${code}`;
    const label = t(key);
    if (label !== key) return label;
    return serverName === '' ? code : serverName;
}

/** The state of one part of a request, as the chip draws it. */
const PART_STATE_TONES: Readonly<Record<string, Tone>> = {
    approved: 'up',
    refused: 'down',
    waiting: 'warn',
    later: 'muted',
};

export function partStateTone(stateCode: string): Tone {
    return PART_STATE_TONES[stateCode] ?? 'muted';
}

export function partStateLabel(t: (key: string) => string, stateCode: string): string {
    const key = `inbox.partState.${stateCode}`;
    const label = t(key);
    return label === key ? stateCode : label;
}

/** What a line does to its subject, in the person's language. */
export function operationLabel(t: (key: string) => string, operation: string): string {
    const key = `inbox.operation.${operation}`;
    const label = t(key);
    return label === key ? operation : label;
}
