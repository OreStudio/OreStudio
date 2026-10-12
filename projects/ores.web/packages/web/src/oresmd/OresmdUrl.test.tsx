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

import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { describe, expect, it, vi } from 'vitest';
import type { CurveSeriesList, OresmdParseResult } from '@ores/contracts';
import { TranslationProvider } from '../i18n/Provider.js';
import { OresmdUrl } from './OresmdUrl.js';
import { SCHEMA } from './testSchema.js';

vi.mock('../session/SessionProvider.js', () => ({ useActingPartyId: () => 'party-1' }));

const URI = 'oresmd://ir/EUR?type=series&instrument=capfloor&quote=rate&cap_floor=Cap';
const FIXING = 'oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M';

const parts = (kind: string): OresmdParseResult => ({
    success: true,
    message: '',
    kind,
    instrument: kind === 'fixing' ? 'ibor' : 'capfloor',
    quote: kind === 'fixing' ? '' : 'rate',
    asset: 'ir',
    subject: 'EUR',
    fields: kind === 'fixing' ? [] : [{ name: 'cap_floor', text: 'Cap' }],
});

/** The series list of the party: one capfloor series, with the axes given. */
const listOf = (axes: string[]): CurveSeriesList => ({
    truncated: false,
    series: [
        {
            series_id: 's1',
            axes,
            identity: {
                asset: 'ir',
                scope: 'EUR',
                instrument_type: 'capfloor',
                quote_type: 'rate',
                party_id: 'party-1',
                fields: [{ name: 'cap_floor', text: 'Cap' }],
            },
        },
    ],
});

function draw(
    uri: string,
    options: {
        readonly parsed?: OresmdParseResult;
        readonly list?: CurveSeriesList;
        readonly onApply?: () => void;
    } = {},
): string {
    const client = new QueryClient({
        defaultOptions: { queries: { retry: false, retryOnMount: false } },
    });
    client.setQueryData(['oresmd-schema'], SCHEMA);
    if (options.parsed !== undefined) client.setQueryData(['oresmd-parse', uri], options.parsed);
    if (options.list !== undefined)
        client.setQueryData(['market-series-list', 'party-1'], options.list);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter>
                    <OresmdUrl
                        uri={uri}
                        {...(options.onApply === undefined ? {} : { onApply: options.onApply })}
                    />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('OresmdUrl', () => {
    it('is a link to the Evolution page when the party has the series and it declares an axis', () => {
        const html = draw(URI, { parsed: parts('series'), list: listOf(['term']) });
        expect(html).toContain(`href="/market-data/evolution?uri=${encodeURIComponent(URI)}"`);
    });

    it('is a link for a quote too, because a quote names the series of its datum', () => {
        expect(draw(URI, { parsed: parts('quote'), list: listOf(['term']) })).toContain('<a ');
    });

    it('is a link to the series page for a series with no axis, such as one spot rate', () => {
        const html = draw(URI, { parsed: parts('series'), list: listOf([]) });
        expect(html).toContain(`href="/market-data/series?uri=${encodeURIComponent(URI)}"`);
        expect(html).not.toContain('/market-data/evolution');
    });

    it('is plain text for a series the party does not have', () => {
        const none: CurveSeriesList = { truncated: false, series: [] };
        expect(draw(URI, { parsed: parts('series'), list: none })).not.toContain('<a ');
    });

    it('is plain text for a fixing, because a fixing has no page', () => {
        const html = draw(FIXING, { parsed: parts('fixing'), list: listOf(['term']) });
        expect(html).not.toContain('<a ');
        expect(html).toContain('EURIBOR');
    });

    it('is plain text until the server and the list have answered, and when the codec refuses it', () => {
        expect(draw(URI)).not.toContain('<a ');
        expect(draw(URI, { parsed: parts('series') })).not.toContain('<a ');
        const refused = { ...parts('series'), success: false, message: 'no' };
        expect(draw(URI, { parsed: refused, list: listOf(['term']) })).not.toContain('<a ');
    });

    it('draws an Inspect button for a screen that keeps no URL, and Edit for one that stores it', () => {
        expect(draw(URI, { parsed: parts('series') })).toContain('>Inspect</button>');
        const editable = draw(URI, { parsed: parts('series'), onApply: () => undefined });
        expect(editable).toContain('>Edit</button>');
        expect(editable).not.toContain('>Inspect</button>');
    });

    it('draws nothing for a feed with no URL, and keeps the dialog closed', () => {
        expect(draw('')).toBe('');
        expect(draw(URI, { parsed: parts('series') })).not.toContain('role="dialog"');
    });
});
