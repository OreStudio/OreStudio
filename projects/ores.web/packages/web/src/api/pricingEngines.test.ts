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

import { afterEach, describe, expect, it, vi } from 'vitest';
import { fileNameOf } from './transport.js';
import { oreDocumentPath, pricingEngines } from './pricingEngines.js';

/**
 * The download of a pricing engine configuration's ORE document.
 *
 * The browser asks the route for the document and saves what comes back under
 * the name the server states, so both halves are asserted here: the address the
 * request goes to, and the name taken from the response.
 */

const XML = `<?xml version="1.0"?>
<PricingEngines>
  <GlobalParameters />
</PricingEngines>
`;

afterEach(() => {
    vi.unstubAllGlobals();
});

describe('the address of one configuration document', () => {
    it('names the configuration and the document', () => {
        expect(oreDocumentPath('abc')).toBe('/api/reporting/pricing-engines/abc/ore-document');
    });

    it('encodes the id, so it cannot leave the route', () => {
        expect(oreDocumentPath('../books')).toBe(
            '/api/reporting/pricing-engines/..%2Fbooks/ore-document',
        );
    });
});

describe('the name a response offers its file under', () => {
    it('reads the name the server stated', () => {
        expect(fileNameOf('attachment; filename="pricingengine.xml"')).toBe('pricingengine.xml');
    });

    it('reads a name the simple form cannot carry', () => {
        expect(
            fileNameOf('attachment; filename="document"; filename*=UTF-8\'\'pricing%20engine.xml'),
        ).toBe('pricing engine.xml');
    });

    it('states nothing when the server states no name', () => {
        expect(fileNameOf(null)).toBe('');
        expect(fileNameOf('attachment')).toBe('');
    });
});

describe('the export of one configuration', () => {
    it('asks the route for the document and answers the file the server named', async () => {
        const asked: string[] = [];
        vi.stubGlobal('fetch', async (path: string) => {
            asked.push(path);
            return new Response(XML, {
                status: 200,
                headers: {
                    'content-type': 'application/xml',
                    'content-disposition': 'attachment; filename="pricingengine.xml"',
                },
            });
        });

        const file = await pricingEngines.oreDocument('abc', 'stated.xml');

        expect(asked).toEqual(['/api/reporting/pricing-engines/abc/ore-document']);
        expect(file).toEqual({ name: 'pricingengine.xml', content: XML });
    });

    it('states the name the dialog gave when the server states none', async () => {
        vi.stubGlobal('fetch', async () => new Response(XML, { status: 200 }));

        const file = await pricingEngines.oreDocument('abc', 'stated.xml');

        expect(file.name).toBe('stated.xml');
    });

    it('states the refusal in the server words rather than saving a document', async () => {
        vi.stubGlobal(
            'fetch',
            async () =>
                new Response(
                    JSON.stringify({
                        code: 'invalid-request',
                        message: 'No file type names the pricing_engines document.',
                    }),
                    { status: 400, headers: { 'content-type': 'application/json' } },
                ),
        );

        await expect(pricingEngines.oreDocument('abc', 'stated.xml')).rejects.toThrow(
            'No file type names the pricing_engines document.',
        );
    });
});
