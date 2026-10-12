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
import type { OresmdParseResult } from '@ores/contracts';
import { TranslationProvider } from '../i18n/Provider.js';
import { OresmdView } from './OresmdView.js';
import { SCHEMA } from './testSchema.js';

const URL0 = 'oresmd://ir/EUR?type=quote&instrument=capfloor&quote=rate&cap_floor=Cap&term=5Y';

const QUOTE: OresmdParseResult = {
    success: true,
    message: '',
    kind: 'quote',
    instrument: 'capfloor',
    quote: 'rate',
    asset: 'ir',
    subject: 'EUR',
    fields: [
        { name: 'cap_floor', text: 'Cap' },
        { name: 'term', text: '5Y' },
    ],
};

const view = (parts: OresmdParseResult, url = URL0): string =>
    renderToStaticMarkup(
        <TranslationProvider>
            <OresmdView schema={SCHEMA} parts={parts} url={url} />
        </TranslationProvider>,
    );

describe('OresmdView', () => {
    it('shows the parts as labels over values, as the record details do, and has no input', () => {
        const html = view(QUOTE);
        expect(html).toContain('<dl ');
        expect(html).toContain('<dt class="text-xs text-ink-faint">Kind</dt>');
        for (const label of [
            'Instrument type',
            'Quote type',
            'Asset class',
            'index_name (path)',
            'cap_floor',
            'term',
        ]) {
            expect(html).toContain(`>${label}</dt>`);
        }
        for (const tag of ['<input', '<select', '<datalist', '<textarea']) {
            expect(html).not.toContain(tag);
        }
    });

    it('shows each value, the kind in words and the path under its field name', () => {
        const html = view(QUOTE);
        expect(html).toContain('>Quote</dd>');
        expect(html).toContain('>capfloor</dd>');
        expect(html).toContain('>EUR</dd>');
        expect(html).toContain('>Cap</dd>');
        expect(html).toContain('>5Y</dd>');
    });

    it('shows the URL under the values in the codec spelling', () => {
        expect(view(QUOTE)).toContain(`>${URL0.replaceAll('&', '&amp;')}</pre>`);
    });

    it('shows a fixing with its index family and no quote type', () => {
        const html = view(
            {
                ...QUOTE,
                kind: 'fixing',
                instrument: 'ibor',
                quote: '',
                fields: [{ name: 'name', text: 'EURIBOR' }],
                subject: 'EUR',
            },
            'oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR',
        );
        expect(html).toContain('>Index family</dt>');
        expect(html).not.toContain('>Quote type</dt>');
        expect(html).toContain('ccy (path)');
    });

    it('shows the reason in place of the values when the codec refused the URL', () => {
        const html = view({
            ...QUOTE,
            success: false,
            message: 'a query out of order',
            fields: [],
        });
        expect(html).toContain('The URL was refused: a query out of order');
        expect(html).not.toContain('<dl ');
        expect(html).toContain('<pre');
    });
});
