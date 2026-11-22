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
import { renderToStaticMarkup } from 'react-dom/server';
import { ApiFailure } from '../api/transport.js';
import type { page } from '../api/servers.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { partyChoiceRefused, ServerSwitch } from './SignInPage.js';

const failure = (code: 'session-expired' | 'forbidden', message: string): ApiFailure =>
    new ApiFailure(code === 'session-expired' ? 401 : 403, { code, message } as never);

function at(port: string, path = '/signin'): page {
    return { protocol: 'http:', hostname: 'localhost', port, pathname: path, search: '', hash: '' };
}

function render(node: React.ReactNode): string {
    return renderToStaticMarkup(<TranslationProvider>{node}</TranslationProvider>);
}

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

describe('the way to the other server, on the sign-in screen', () => {
    it('names the server that served the page, and links to the other one', () => {
        const html = render(<ServerSwitch at={at('20402')} />);

        expect(html).toContain('Served by the BFF');
        expect(html).toContain('href="http://localhost:20401/signin"');
        expect(html).toContain('data-server="cpp"');
    });

    it('goes back to the BFF from the C++ server', () => {
        const html = render(<ServerSwitch at={at('20401', '/market-data/quotes')} />);

        expect(html).toContain('Served by the C++ server');
        expect(html).toContain('href="http://localhost:20402/market-data/quotes"');
        expect(html).toContain('data-server="bff"');
    });

    it('says nothing on a page that neither server serves', () => {
        expect(render(<ServerSwitch at={at('5173')} />)).toBe('');
    });
});
