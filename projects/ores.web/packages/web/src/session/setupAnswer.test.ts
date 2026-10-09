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

import { describe, expect, it } from 'vitest';
import { setupAnswerStep, type SetupAnswerState } from './setupAnswer.js';

const ACCOUNT = '11111111-1111-1111-1111-111111111111';

function state(overrides: Partial<SetupAnswerState> = {}): SetupAnswerState {
    return {
        answerAccountId: ACCOUNT,
        signedInAs: ACCOUNT,
        reading: false,
        asked: false,
        authenticated: true,
        ...overrides,
    };
}

describe('the answer a screen acts on', () => {
    it('is settled when it names the account in hand', () => {
        expect(setupAnswerStep(state())).toBe('current');
    });

    it('waits rather than judging the answer it is replacing', () => {
        // A fresh sign-in: the answer on screen was read for nobody, and a fresh
        // one is already on its way. Judging the stale one here is what used to
        // sign a browser out the instant it signed in.
        expect(setupAnswerStep(state({ answerAccountId: '', asked: true, reading: true }))).toBe(
            'wait',
        );
        expect(setupAnswerStep(state({ answerAccountId: '', reading: true }))).toBe('wait');
    });

    it('asks once for an answer about the session that has just signed in', () => {
        expect(setupAnswerStep(state({ answerAccountId: '', signedInAs: ACCOUNT }))).toBe('ask');
    });

    it('signs out when a settled answer does not know the browser', () => {
        expect(
            setupAnswerStep(state({ answerAccountId: '', signedInAs: ACCOUNT, asked: true })),
        ).toBe('sign-out');
    });

    it('leaves a visitor alone, because there is no session to reconcile', () => {
        // Anonymous with an answer that names an account: the answer is being
        // replaced, and there is nobody to sign out.
        expect(
            setupAnswerStep(
                state({
                    answerAccountId: ACCOUNT,
                    signedInAs: '',
                    asked: true,
                    authenticated: false,
                }),
            ),
        ).toBe('current');
    });
});
