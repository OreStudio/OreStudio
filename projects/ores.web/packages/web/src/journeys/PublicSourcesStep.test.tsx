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
import { decisionOf, everyDecided, PublicSourcesStep } from './PublicSourcesStep.js';

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

describe('the public sources step', () => {
    it('says it is loading until the publishers arrive', () => {
        const html = renderToStaticMarkup(
            <TranslationProvider>
                <PublicSourcesStep
                    server={{
                        publicSources: vi.fn(async () => [publisher('ecb')]),
                        acceptSourceTerms: vi.fn(async () => undefined),
                        declineSourceTerms: vi.fn(async () => undefined),
                    }}
                    onDecided={vi.fn()}
                />
            </TranslationProvider>,
        );
        expect(html).toContain('Loading');
    });
});
