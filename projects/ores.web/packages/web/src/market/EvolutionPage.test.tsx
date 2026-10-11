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
import { MemoryRouter } from 'react-router';
import type { CurveEvolution, CurveSnapshot } from '@ores/contracts';
import { TranslationProvider } from '../i18n/Provider.js';
import { EvolutionPage, MatrixPanel, NowPanel } from './EvolutionPage.js';

/**
 * The evolution screen, as a reader sees it.
 *
 * The panels take their data as properties, so they render without a server.
 * What is checked is the screen's own contract: a node no point states is a hole
 * and never a zero, each node states how it changed, the age and the source
 * show only where they apply, and the page opens on its form with nothing read.
 */

function render(node: React.ReactNode): string {
    return renderToStaticMarkup(
        <QueryClientProvider client={new QueryClient()}>
            <TranslationProvider>
                <MemoryRouter>{node}</MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

const evolution: CurveEvolution = {
    coordinate_fields: ['expiry', 'strike_label'],
    nodes: [
        { coordinates: ['1Y', 'ATM'], status: 'persistent' },
        { coordinates: ['1Y', '25RR'], status: 'added' },
        { coordinates: ['2Y', 'ATM'], status: 'never_quoted' },
    ],
    instants: [
        { as_of: '2026-10-01 00:00:00Z', values: ['0.080', '', ''] },
        { as_of: '2026-10-02 00:00:00Z', values: ['0.081', '0.011', ''] },
    ],
};

const snapshot: CurveSnapshot = {
    as_of: '2026-10-10 12:00:00Z',
    coordinate_fields: ['expiry', 'strike_label'],
    nodes: [
        { coordinates: ['1Y', 'ATM'], status: '' },
        { coordinates: ['2Y', 'ATM'], status: '' },
    ],
    values: ['0.080', ''],
    recorded_at: ['2026-10-10 09:00:00Z', '1970-01-01 00:00:00Z'],
    provenance: [
        {
            source_kind: 'derived',
            modified_by: '',
            change_reason_code: '',
            change_commentary: '',
            recorded_at: '2026-10-10 09:00:00Z',
            derivation_config_id: '12345678-aaaa-bbbb-cccc-1234567890ab',
            derivation_config_version: 3,
            source_as_of: '2026-10-10 08:00:00Z',
        },
        {
            source_kind: '',
            modified_by: '',
            change_reason_code: '',
            change_commentary: '',
            recorded_at: '1970-01-01 00:00:00Z',
            derivation_config_id: '',
            derivation_config_version: 0,
            source_as_of: '1970-01-01 00:00:00Z',
        },
    ],
    oldest_age_seconds: 10800,
    spread_seconds: 0,
    warning: false,
};

describe('MatrixPanel', () => {
    it('draws a row for each node and a column for each instant', () => {
        const html = render(<MatrixPanel evolution={evolution} />);
        expect(html).toContain('1Y / ATM');
        expect(html).toContain('1Y / 25RR');
        expect(html).toContain('2026-10-01 00:00');
        expect(html).toContain('2026-10-02 00:00');
        expect(html).toContain('0.081');
    });

    it('states how each node changed', () => {
        const html = render(<MatrixPanel evolution={evolution} />);
        expect(html).toContain('Persistent');
        expect(html).toContain('Added');
        expect(html).toContain('Never quoted');
    });

    it('draws a missing value as a hole and not as a zero', () => {
        const html = render(<MatrixPanel evolution={evolution} />);
        expect(html).toContain('title="No point"');
        expect(html).not.toContain('>0<');
        expect(html).not.toContain('0.000');
    });

    it('counts the nodes that hold no value in the range', () => {
        expect(render(<MatrixPanel evolution={evolution} />)).toContain(
            '1 nodes hold no value at any instant of this range.',
        );
    });

    it('says so when the object declares no node or holds no instant', () => {
        expect(
            render(<MatrixPanel evolution={{ ...evolution, nodes: [], instants: [] }} />),
        ).toContain('The object declares no node.');
        expect(render(<MatrixPanel evolution={{ ...evolution, instants: [] }} />)).toContain(
            'The object holds no instant in this range.',
        );
    });
});

describe('NowPanel', () => {
    it('states the age of a value and leaves a hole without one', () => {
        const html = render(<NowPanel snapshot={snapshot} provenance={false} />);
        expect(html).toContain('3h');
        expect(html).toContain('title="No point"');
        expect(html).toContain('The oldest value is 3h old.');
    });

    it('shows the source and the recipe only when provenance was asked', () => {
        const without = render(<NowPanel snapshot={snapshot} provenance={false} />);
        expect(without).not.toContain('Derived');
        const asked = render(<NowPanel snapshot={snapshot} provenance />);
        expect(asked).toContain('Derived');
        expect(asked).toContain('12345678 v3');
    });

    it('says in words when the values mix several market states, and says nothing otherwise', () => {
        const html = render(
            <NowPanel snapshot={{ ...snapshot, warning: true }} provenance={false} />,
        );
        expect(html).toContain('These values come from several market states.');
        expect(html).toContain('The oldest is 3h old');
        const quiet = render(<NowPanel snapshot={snapshot} provenance={false} />);
        expect(quiet).not.toContain('several market states');
    });
});

describe('EvolutionPage', () => {
    it('opens on its form and reads nothing', () => {
        const html = render(<EvolutionPage />);
        expect(html).toContain('Watch a curve or surface evolve');
        expect(html).toContain('Asset class');
        expect(html).toContain('Show where each value came from');
        expect(html).not.toContain('Over the range');
        expect(html).not.toContain('Reading the object');
    });
});
