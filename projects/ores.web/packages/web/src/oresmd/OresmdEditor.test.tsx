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
import { describe, expect, it } from 'vitest';
import { TranslationProvider } from '../i18n/Provider.js';
import { EMPTY, type EditorState } from './editor.js';
import { Composer, OresmdEditorDialog } from './OresmdEditor.js';
import { SCHEMA } from './testSchema.js';

function dialog(options: {
    readonly url?: string;
    readonly seed?: boolean;
    readonly failure?: string;
}): string {
    const client = new QueryClient({
        defaultOptions: { queries: { retry: false, retryOnMount: false } },
    });
    if (options.failure !== undefined) {
        const query = client.getQueryCache().build(client, { queryKey: ['oresmd-schema'] });
        query.setState({
            ...query.state,
            status: 'error',
            error: new Error(options.failure),
            fetchStatus: 'idle',
        });
    } else if (options.seed === true) {
        client.setQueryData(['oresmd-schema'], SCHEMA);
    }
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <OresmdEditorDialog
                    url={options.url ?? ''}
                    onApply={() => undefined}
                    onClose={() => undefined}
                />
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

function composer(state: EditorState): string {
    const client = new QueryClient({
        defaultOptions: { queries: { retry: false, retryOnMount: false } },
    });
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <Composer schema={SCHEMA} state={state} onChange={() => undefined} />
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

const CAPFLOOR: EditorState = {
    kind: 'quote',
    instrument: 'capfloor',
    quote: 'rate',
    subject: 'EUR',
    values: { term: '5Y', cap_floor: 'Cap' },
};

describe('the oresmd editor dialog', () => {
    it('is a dialog with the title, and says it is loading until the schema arrives', () => {
        const html = dialog({});
        expect(html).toContain('role="dialog"');
        expect(html).toContain('aria-label="oresmd URL"');
        expect(html).toContain('Loading');
    });

    it('says what failed when the schema cannot be read', () => {
        expect(dialog({ failure: 'Cannot reach the server.' })).toContain(
            'Cannot reach the server.',
        );
    });

    it('opens a new URL on the kinds, the instruments by asset class and the quote types', () => {
        const html = dialog({ seed: true });
        for (const kind of ['Quote', 'Series', 'Fixing'])
            expect(html).toContain(`>${kind}</button>`);
        expect(html).toContain('<optgroup label="inflation">');
        expect(html).toContain('<optgroup label="ir">');
        expect(html).toContain('<option value="capfloor">capfloor</option>');
        expect(html).toContain('<option value="price">price</option>');
    });

    it('says to choose an instrument and a quote, and offers neither Copy nor Apply yet', () => {
        const html = dialog({ seed: true });
        expect(html).toContain('Choose an instrument and a quote type.');
        expect(html).toMatch(/<button[^>]*disabled=""[^>]*>Copy<\/button>/);
        expect(html).toMatch(/<button[^>]*disabled=""[^>]*>Apply<\/button>/);
    });

    it('waits for the URL it was given before it draws the composer', () => {
        const html = dialog({ seed: true, url: 'oresmd://ir/EUR?type=quote' });
        expect(html).toContain('Loading');
        expect(html).not.toContain('<optgroup');
    });
});

describe('the editor without an Apply', () => {
    it('offers Copy and Cancel and no Apply when the screen gives no onApply', () => {
        const client = new QueryClient();
        client.setQueryData(['oresmd-schema'], SCHEMA);
        const html = renderToStaticMarkup(
            <QueryClientProvider client={client}>
                <TranslationProvider>
                    <OresmdEditorDialog url="" onClose={() => undefined} />
                </TranslationProvider>
            </QueryClientProvider>,
        );
        expect(html).toContain('>Copy<');
        expect(html).toContain('>Cancel<');
        expect(html).not.toContain('>Apply<');
    });
});

describe('the composer', () => {
    it('draws the path and the other fields of the instrument in key order', () => {
        const html = composer(CAPFLOOR);
        expect(html).toContain('Asset class: ir');
        expect(html.indexOf('index_name (path)')).toBeLessThan(html.indexOf('>cap_floor<'));
        expect(html.indexOf('>cap_floor<')).toBeLessThan(html.indexOf('>term<'));
        expect(html).toContain('value="EUR"');
    });

    it('offers a closed set as a select and a free field as an input', () => {
        const html = composer(CAPFLOOR);
        expect(html).toContain('<option value="Cap" selected="">Cap</option>');
        expect(html).toContain('<option value="Floor">Floor</option>');
        expect(html).toContain('value="5Y"');
    });

    it('names an input by its own label, and keeps the tags out of the name', () => {
        const html = composer(CAPFLOOR);
        const id = /<label for="([^"]+)" class="font-mono text-ink">term<\/label>/.exec(html)?.[1];
        expect(id).toBeDefined();
        expect(html).toContain(`id="${id}"`);
        expect(html).not.toMatch(/<label[^>]*>[^<]*<[^>]*>[^<]*Required/);
    });

    it('marks a field required or optional, and a coordinate', () => {
        const html = composer(CAPFLOOR);
        expect(html).toContain('>Required<');
        expect(html).toContain('>Optional<');
        expect(html).toContain('>Coordinate<');
    });

    it('holds no coordinate in a series', () => {
        const html = composer({ ...CAPFLOOR, kind: 'series' });
        expect(html).not.toContain('>term<');
        expect(html).not.toContain('>Coordinate<');
    });

    it('draws a fixing from the index family, with no quote type', () => {
        const html = composer({ ...EMPTY, kind: 'fixing', instrument: 'ibor' });
        expect(html).toContain('Index family');
        expect(html).not.toContain('Quote type');
        expect(html).toContain('ccy (path)');
        expect(html).toContain('>name<');
        expect(html).toContain('>tenor<');
    });

    it('offers the currencies the system keeps for a currency field, and nothing for another', () => {
        const fixing = composer({ ...EMPTY, kind: 'fixing', instrument: 'ibor' });
        const list = /list="([^"]+)"/.exec(fixing)?.[1];
        expect(list).toBeDefined();
        expect(fixing).toContain(`<datalist id="${list}">`);
        const capfloor = composer(CAPFLOOR);
        expect(capfloor).not.toContain('<datalist');
        expect(capfloor).not.toContain(' list=');
    });

    it('draws no fields until an instrument is chosen', () => {
        expect(composer(EMPTY)).not.toContain('Asset class');
    });
});
