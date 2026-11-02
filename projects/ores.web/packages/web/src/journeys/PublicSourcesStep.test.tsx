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
 */
import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it, vi } from 'vitest';
import type { PublicRatesProvider } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { decisionOf, everyDecided, PublicSourcesStep, sourceChanges } from './PublicSourcesStep.js';

function publisher(code: string, over: Partial<PublicRatesProvider> = {}): PublicRatesProvider {
    return {
        code,
        version: 1,
        name: code.toUpperCase(),
        kind: 'reference',
        description: '',
        terms_url: `https://${code}.example/terms`,
        terms_credit: `Source: ${code}`,
        allows_storage: true,
        terms_accepted: false,
        terms_accepted_by: '',
        terms_accepted_at: null,
        feeds: [],
        series: [],
        ...over,
    };
}

describe('a public source decision', () => {
    it('is the one made in this run, else accepted when the terms already are', () => {
        expect(decisionOf(publisher('ecb'), {})).toBeUndefined();
        expect(decisionOf(publisher('ecb', { terms_accepted: true }), {})).toBe('accepted');
        expect(decisionOf(publisher('ecb', { terms_accepted: true }), { ecb: 'declined' })).toBe(
            'declined',
        );
    });

    it('counts every publisher that states terms, and none that states no terms', () => {
        const all = [publisher('ecb'), publisher('imf'), publisher('free', { terms_url: '' })];
        expect(everyDecided(all, { ecb: 'accepted' })).toBe(false);
        expect(everyDecided(all, { ecb: 'accepted', imf: 'declined' })).toBe(true);
    });
});

describe('what Continue sends from the public sources', () => {
    const sources = [
        publisher('ecb', { version: 3 }),
        publisher('imf', { version: 2, terms_accepted: true }),
        publisher('boc', { version: 5 }),
    ];

    it('sends nothing until a way is chosen', () => {
        expect(sourceChanges(sources, undefined, {})).toBeUndefined();
    });

    it('lets a resumed run go on at once when every publisher is already accepted', () => {
        const accepted = sources.map((p) => ({ ...p, terms_accepted: true }));
        expect(sourceChanges(accepted, undefined, {})).toEqual({ accept: [], decline: [] });
    });

    it('accepts, at the version shown, every publisher not yet accepted when all are accepted', () => {
        expect(sourceChanges(sources, 'all', { boc: 'declined' })).toEqual({
            accept: [
                { code: 'ecb', version: 3 },
                { code: 'boc', version: 5 },
            ],
            decline: [],
        });
    });

    it('sends nothing to accept when every publisher is already accepted', () => {
        const accepted = sources.map((p) => ({ ...p, terms_accepted: true }));
        expect(sourceChanges(accepted, 'all', {})).toEqual({ accept: [], decline: [] });
    });

    it('waits for every publisher when each is decided in turn', () => {
        expect(sourceChanges(sources, 'each', { ecb: 'accepted' })).toBeUndefined();
    });

    it('withdraws only an acceptance that exists when each is decided in turn', () => {
        expect(
            sourceChanges(sources, 'each', { ecb: 'declined', imf: 'declined', boc: 'accepted' }),
        ).toEqual({ accept: [{ code: 'boc', version: 5 }], decline: ['imf'] });
    });
});

describe('the public sources step', () => {
    it('says it is loading until the publishers arrive', () => {
        const html = renderToStaticMarkup(
            <TranslationProvider>
                <PublicSourcesStep
                    server={{
                        publicSources: vi.fn(async () => [publisher('ecb')]),
                        acceptAllSourceTerms: vi.fn(async () => []),
                        declineSourceTerms: vi.fn(async () => undefined),
                    }}
                    onReady={vi.fn()}
                />
            </TranslationProvider>,
        );
        expect(html).toContain('Loading');
    });
});
