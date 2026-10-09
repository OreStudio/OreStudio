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
        answerSessionPresent: true,
        signedInAs: ACCOUNT,
        reading: false,
        asked: false,
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
        // Signed in, and the answer on screen is the one read before the cookie
        // existed: it names nobody and claims no cookie was presented.
        expect(
            setupAnswerStep(
                state({ answerAccountId: '', answerSessionPresent: false, signedInAs: ACCOUNT }),
            ),
        ).toBe('ask');
    });

    it('asks rather than signing out over a stale answer that claims a cookie was presented', () => {
        // The regression a first-run walk hit. The browser arrived holding a
        // cookie the deployment no longer knows, so the wizard's read before
        // signing in answered "a cookie was presented and it names nobody".
        // Signing in replaced that cookie, and the answer describing the one it
        // replaced was judged as a verdict about the session just opened, which
        // signed the browser out of the account it had that instant been given.
        expect(
            setupAnswerStep(
                state({
                    answerAccountId: '',
                    answerSessionPresent: true,
                    signedInAs: ACCOUNT,
                    asked: false,
                    reading: false,
                }),
            ),
        ).toBe('ask');
    });

    it('signs out when the deployment says it does not know the cookie', () => {
        expect(
            setupAnswerStep(
                state({
                    answerAccountId: '',
                    answerSessionPresent: true,
                    signedInAs: ACCOUNT,
                    asked: true,
                }),
            ),
        ).toBe('sign-out');
    });

    it('never signs out over an answer that claims no cookie was presented', () => {
        // The regression this rule exists for. The journey reads the deployment
        // again while creating its administrator, so the answer on screen was
        // read before the sign-in that follows it existed. Signing out on that
        // answer tore down the session the person had just been given, and the
        // deployment then asked them to sign in to the account they had just
        // made.
        expect(
            setupAnswerStep(
                state({
                    answerAccountId: '',
                    answerSessionPresent: false,
                    signedInAs: ACCOUNT,
                    asked: true,
                    reading: false,
                }),
            ),
        ).toBe('current');
    });

    it('leaves a visitor alone, because there is no session to reconcile', () => {
        expect(
            setupAnswerStep(state({ answerAccountId: ACCOUNT, signedInAs: '', asked: true })),
        ).toBe('current');
    });
});
