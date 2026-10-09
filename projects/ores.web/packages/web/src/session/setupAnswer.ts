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
 * What a screen does about the account its bootstrap answer names.
 *
 * The two setup flags in that answer are settings, and settings are read through
 * a session, while the rest of the answer is read without one. So an answer read
 * before somebody signed in says "no setup" and names nobody, and a screen that
 * acted on it would put the wizard in front of a person who has just finished
 * it.
 *
 * The rule is data rather than an effect with four conditions in it, because it
 * is the part worth asserting: the bug it replaces was an effect that judged the
 * stale answer while it was asking for a fresh one, and signed a browser out the
 * instant it signed in.
 */

/** The answer, and the session it is being reconciled with. */
export interface SetupAnswerState {
    /** The account the answer's two flags were read for, or empty. */
    readonly answerAccountId: string;
    /** The account this browser is signed in as, or empty. */
    readonly signedInAs: string;
    /** Whether a fresh answer is being read right now. */
    readonly reading: boolean;
    /**
     * Whether a fresh answer has already been asked for, for this session.
     *
     * It is what makes the rest of the answer judgeable. Until it is set, the
     * answer on screen was read for whatever the browser held before this
     * session, and no verdict is taken from it.
     */
    readonly asked: boolean;
    /**
     * Whether the answer says the request carried a session cookie.
     *
     * This is what makes `sign-out` mean something, read from an answer that
     * was asked for: a cookie the deployment does not know is evidence that the
     * browser's session is over. The same field read from an answer taken
     * before the sign-in says nothing, because the cookie it describes is the
     * one the sign-in replaced.
     */
    readonly answerSessionPresent: boolean;
}

/**
 * `current` when the answer is about the session in hand, `wait` while a fresh
 * one is on its way, `ask` when it has not been asked for yet, and `sign-out`
 * when the deployment says it does not know the cookie the browser presented.
 */
export type SetupAnswerStep = 'current' | 'wait' | 'ask' | 'sign-out';

export function setupAnswerStep(state: SetupAnswerState): SetupAnswerStep {
    if (state.answerAccountId === state.signedInAs) {
        return 'current';
    }
    /*
     * Nothing is judged while an answer is being read. The screen keeps the
     * answer it asked to replace, so judging it here reads the stale fact as a
     * verdict about the browser, which is how a fresh sign-in was undone.
     */
    if (state.reading) {
        return 'wait';
    }
    /*
     * Only an answer that was asked for is judged, and the asking is what makes
     * the evidence in it a fact about the session in hand rather than a fact
     * about whatever cookie the browser carried when the screen last read it.
     *
     * This is the whole of the bug the rule kept being loosened for. The
     * wizard reads the deployment again before it signs in, and a browser that
     * arrived holding a cookie the deployment does not know gets an answer
     * saying a cookie was presented that names nobody. Signing in then replaces
     * that cookie, and the answer on screen describes the one it replaced: read
     * as a verdict about the session that has just been opened, it says the
     * deployment does not know the browser, and the browser signs out of the
     * account it has this instant been given. Asking first means the verdict is
     * only ever reached from an answer read for the session it is about.
     */
    if (!state.asked) {
        return 'ask';
    }
    /*
     * The deployment was shown a cookie and named nobody, and that answer is
     * the one asked for about the session in hand: it does not know this
     * browser, and the browser belongs at the sign-in form.
     */
    if (state.answerSessionPresent && state.answerAccountId === '') {
        return 'sign-out';
    }
    return 'current';
}
