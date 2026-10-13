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
import type { CurveSnapshot } from '@ores/contracts';
import { TranslationProvider } from '../i18n/Provider.js';
import { SurfacePanel } from './SurfacePanel.js';

/**
 * The surface panel, as a reader sees it.
 *
 * It renders without a server because it takes the snapshot as a property. What is checked
 * is what a reader would notice: the heatmap carries the declared rows and columns, a hole is
 * a dash with no colour, a line breaks at a hole, and a shape that is not a surface is said
 * in words or left to the chart it has.
 */

function render(node: React.ReactNode): string {
    return renderToStaticMarkup(
        <QueryClientProvider client={new QueryClient()}>
            <TranslationProvider>{node}</TranslationProvider>
        </QueryClientProvider>,
    );
}

function snapshotOf(
    axes: readonly string[],
    nodes: readonly (readonly string[])[],
    values: readonly string[],
): CurveSnapshot {
    return {
        as_of: '2026-10-10 12:00:00Z',
        coordinate_fields: [...axes],
        nodes: nodes.map((coordinates) => ({ coordinates: [...coordinates], status: '' })),
        values: [...values],
        recorded_at: values.map(() => '2026-10-10 09:00:00Z'),
        provenance: [],
        oldest_age_seconds: 0,
        spread_seconds: 0,
        warning: false,
    };
}

const GRID = snapshotOf(
    ['expiry', 'strike_label'],
    [
        ['1M', 'ATM'],
        ['1M', '25RR'],
        ['1M', '25BF'],
        ['1Y', 'ATM'],
        ['1Y', '25RR'],
        ['1Y', '25BF'],
    ],
    ['0.080', '0.010', '0.004', '0.100', '0.012', ''],
);

describe('SurfacePanel', () => {
    it('draws the heatmap with the declared rows and columns and the values', () => {
        const html = render(<SurfacePanel snapshot={GRID} />);
        expect(html).toContain('expiry / strike_label');
        for (const label of ['1M', '1Y', 'ATM', '25RR', '25BF']) {
            expect(html).toContain(`>${label}<`);
        }
        expect(html).toContain('0.100');
    });

    it('shades a value and leaves a hole as a dash with no colour', () => {
        const html = render(<SurfacePanel snapshot={GRID} />);
        // Five cells hold a value, so five carry a colour.
        expect(html.match(/color-mix/g)).toHaveLength(5);
        expect(html).toContain('title="No point"');
        expect(html).not.toContain('>0<');
    });

    it('draws one slice for each row and names it in the legend', () => {
        const html = render(<SurfacePanel snapshot={GRID} />);
        expect(html.match(/<polyline/g)).toHaveLength(2);
        expect(html).toContain('Slices');
        expect(html).toContain('One line for each row, across the columns');
    });

    it('breaks a slice at a hole', () => {
        const gap = snapshotOf(
            ['a', 'b'],
            [
                ['x', 'p'],
                ['x', 'q'],
                ['x', 'r'],
                ['x', 's'],
            ],
            ['0.1', '', '0.3', '0.4'],
        );
        const html = render(<SurfacePanel snapshot={gap} />);
        // One lone point before the hole, and one line after it.
        expect(html.match(/<circle/g)).toHaveLength(1);
        expect(html.match(/<polyline/g)).toHaveLength(1);
    });

    it('offers the two colour scales', () => {
        const html = render(<SurfacePanel snapshot={GRID} />);
        expect(html).toContain('The whole surface');
        expect(html).toContain('Each column');
    });

    it('says a slice is not always a smile', () => {
        const html = render(<SurfacePanel snapshot={GRID} />);
        expect(html).toContain('not always a smile');
    });

    it('leaves a shape with one axis to the chart it has', () => {
        const curve = snapshotOf(['tenor'], [['1Y'], ['2Y']], ['0.1', '0.2']);
        expect(render(<SurfacePanel snapshot={curve} />)).toBe('');
    });

    it('says a shape with more than two axes is not drawn', () => {
        const cube = snapshotOf(['a', 'b', 'c'], [['x', 'y', 'z']], ['0.1']);
        const html = render(<SurfacePanel snapshot={cube} />);
        expect(html).toContain('A shape with 3 axes is not drawn as a surface');
        expect(html).not.toContain('<table');
    });
});
