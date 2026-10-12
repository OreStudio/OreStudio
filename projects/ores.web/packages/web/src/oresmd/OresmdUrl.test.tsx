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

import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';
import { TranslationProvider } from '../i18n/Provider.js';
import { OresmdUrl } from './OresmdUrl.js';

const draw = (uri: string): string =>
    renderToStaticMarkup(
        <TranslationProvider>
            <OresmdUrl uri={uri} />
        </TranslationProvider>,
    );

describe('OresmdUrl', () => {
    it('draws the URL as text with a button that opens the editor', () => {
        const html = draw('oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD');
        expect(html).toContain(
            'oresmd://fx/EUR?type=quote&amp;instrument=fx_spot&amp;quote=rate&amp;ccy=USD',
        );
        expect(html).toContain('>Open in the editor<');
    });

    it('draws nothing for a feed with no URL', () => {
        expect(draw('')).toBe('');
    });

    it('keeps the editor closed until the button is pressed', () => {
        expect(draw('oresmd://ir/EUR?type=series')).not.toContain('role="dialog"');
    });
});
