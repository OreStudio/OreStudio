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
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import type { AccountAccess } from '@ores/wire-protocol/browser';
import type { SimulatedSource } from '../../api/client.js';
import { TranslationProvider } from '../../i18n/Provider.js';
import { enFlat } from '../../i18n/locales/en.js';
import { interpolate } from '../../i18n/translate.js';
import { SourceStrip, stopEffects } from './sourceStrip.js';

const bloomberg: SimulatedSource = {
    source_name: 'bloomberg-a',
    provider_code: 'bloomberg',
    stopped: false,
    served_feeds: 26,
    held_surfaces: 14,
    surfaces_only_held_here: 3,
};

function access(codes: string[]): AccountAccess {
    return {
        roles: [
            {
                roleId: '33333333-3333-3333-3333-333333333333' as AccountAccess['roles'][number]['roleId'],
                name: 'Operator',
                description: '',
                permissionCodes: codes,
                givenBy: 'priya',
                givenAt: '2026-10-04',
                reasonCode: 'access.new_joiner',
                commentary: '',
            },
        ],
    };
}

function render(sources: SimulatedSource[], codes: string[]): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['simulated-sources'], sources);
    client.setQueryData(['my-access'], access(codes));
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <SourceStrip provider="bloomberg" />
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe("a provider's simulated source", () => {
    it('says what a stop stops, what runs on, and that it lasts', () => {
        const t = (key: string, values?: Record<string, string | number>): string =>
            interpolate(enFlat[key] ?? key, values ?? {});
        const lines = stopEffects(bloomberg, t);
        expect(lines[0]).toBe('The 26 feeds it serves for the store stop.');
        expect(lines[1]).toBe(
            'It releases the 14 vol surfaces it holds: 3 stop with it, and 11 run on for other sources.',
        );
        expect(lines).toContain('Feeds started by hand keep running.');
        expect(lines.at(-1)).toContain('a restart of the service included');
    });

    it('leaves out the surfaces when the source holds none', () => {
        const t = (key: string, values?: Record<string, string | number>): string =>
            interpolate(enFlat[key] ?? key, values ?? {});
        const lines = stopEffects(
            { ...bloomberg, held_surfaces: 0, surfaces_only_held_here: 0 },
            t,
        );
        expect(lines.some((line) => line.includes('vol surfaces'))).toBe(false);
    });

    it('shows a serving source with what it holds and a Stop for an operator', () => {
        const html = render([bloomberg], ['synthetic::feeds:run']);
        expect(html).toContain('bloomberg-a');
        expect(html).toContain('>Serving<');
        expect(html).toContain('26 feeds served · 14 vol surfaces held');
        expect(html).toContain('>Stop source<');
        expect(html).not.toContain('>Start source<');
    });

    it('shows a stopped source with a Start, and no button for a reader', () => {
        const stopped = { ...bloomberg, stopped: true };
        expect(render([stopped], ['synthetic::feeds:run'])).toContain('>Start source<');
        const reader = render([stopped], []);
        expect(reader).toContain('>Stopped<');
        expect(reader).not.toContain('>Start source<');
    });

    it('draws nothing for a provider with no simulated source', () => {
        expect(render([{ ...bloomberg, provider_code: 'ice' }], ['synthetic::feeds:run'])).toBe('');
    });
});
