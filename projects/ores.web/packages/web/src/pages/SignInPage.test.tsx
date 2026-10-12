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
import { ApiFailure } from '../api/transport.js';
import { partyChoiceRefused } from './SignInPage.js';

const failure = (code: 'session-expired' | 'forbidden', message: string): ApiFailure =>
    new ApiFailure(code === 'session-expired' ? 401 : 403, { code, message } as never);

describe('a refused choice of party', () => {
    it('begins the sign-in again and says it timed out, when the session ended', () => {
        const refused = partyChoiceRefused(
            failure('session-expired', 'Your session has ended. Sign in again.'),
            'Your sign-in timed out.',
        );

        expect(refused).toEqual({ beginAgain: true, message: 'Your sign-in timed out.' });
    });

    it('keeps the choice open and says what came back, for any other refusal', () => {
        const refused = partyChoiceRefused(failure('forbidden', 'Not allowed.'), 'timed out');

        expect(refused).toEqual({ beginAgain: false, message: 'Not allowed.' });
    });

    it('says an error that is not an API failure as it is', () => {
        expect(partyChoiceRefused(new Error('Network down'), 'timed out')).toEqual({
            beginAgain: false,
            message: 'Network down',
        });
    });
});
