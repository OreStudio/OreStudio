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
import { TranslationProvider } from '../i18n/Provider.js';
import { PartyChoice } from './PartyChoice.js';

const PARTIES = [
    { id: 'uk', name: 'Acme UK' },
    { id: 'us', name: 'Acme US' },
];

function render(current?: string): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <PartyChoice parties={PARTIES} onChoose={() => undefined} current={current} />
        </TranslationProvider>,
    );
}

describe('the party choice', () => {
    it('offers every party by name', () => {
        const html = render();

        expect(html).toContain('Acme UK');
        expect(html).toContain('Acme US');
    });

    it('marks the party the session acts for and does not offer it again', () => {
        const html = render('us');

        expect(html).toContain('current');
        expect(html.match(/disabled=""/g)).toHaveLength(1);
    });

    it('marks none when no party is current, as at sign-in', () => {
        expect(render()).not.toContain('disabled=""');
    });
});
