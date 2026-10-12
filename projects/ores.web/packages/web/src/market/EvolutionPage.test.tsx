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
import type { CurveEvolution, CurveSeriesList, CurveSnapshot } from '@ores/contracts';
import { TranslationProvider } from '../i18n/Provider.js';
import { EvolutionPage, MatrixPanel, NowPanel, SeriesPicker } from './EvolutionPage.js';

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
        ).toContain('No series with this identity is visible in the party you work for.');
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

const LIST: CurveSeriesList = {
    truncated: false,
    series: [
        {
            series_id: '77777777-7777-4777-8777-777777777777',
            identity: {
                asset: 'fx',
                scope: 'EUR',
                instrument_type: 'fx_option',
                quote_type: 'rate_lnvol',
                party_id: '88888888-8888-4888-8888-888888888888',
                fields: [{ name: 'ccy', text: 'USD' }],
            },
            axes: ['expiry', 'strike_label'],
        },
        {
            series_id: '99999999-9999-4999-8999-999999999999',
            identity: {
                asset: 'fx',
                scope: 'EUR',
                instrument_type: 'fx_spot',
                quote_type: 'rate',
                party_id: '88888888-8888-4888-8888-888888888888',
                fields: [{ name: 'ccy', text: 'GBP' }],
            },
            axes: [],
        },
    ],
};

function picker(props: Partial<Parameters<typeof SeriesPicker>[0]>): string {
    return render(
        <SeriesPicker
            list={LIST}
            loading={false}
            failure={undefined}
            picked=""
            onPick={() => undefined}
            {...props}
        />,
    );
}

describe('NowPanel adjusting', () => {
    const adjust = {
        identity: {
            asset: 'fx',
            scope: 'EUR',
            instrument_type: 'fx_option',
            quote_type: 'rate_lnvol',
            party_id: '',
            fields: [],
        },
        onChanged: () => undefined,
    };
    const withManualNode: CurveSnapshot = {
        ...snapshot,
        provenance: [
            { ...snapshot.provenance[0]!, source_kind: 'manual', modified_by: 'alice' },
            snapshot.provenance[1]!,
        ],
    };

    it('offers nothing to adjust to a person who may not write', () => {
        const html = render(<NowPanel snapshot={withManualNode} provenance />);
        expect(html).not.toContain('Adjust');
        expect(html).not.toContain('Clear');
    });

    it('offers an adjust button on each node to a person who may write', () => {
        const html = render(<NowPanel snapshot={withManualNode} provenance adjust={adjust} />);
        expect(html.match(/>Adjust</g)).toHaveLength(2);
    });

    it('offers to clear only the nodes a person keyed', () => {
        const html = render(<NowPanel snapshot={withManualNode} provenance adjust={adjust} />);
        expect(html.match(/>Clear</g)).toHaveLength(1);
        expect(html).toContain('Manual');
    });

    it('offers no clear when no node is manual', () => {
        const html = render(<NowPanel snapshot={snapshot} provenance adjust={adjust} />);
        expect(html).not.toContain('>Clear<');
    });
});

describe('SeriesPicker', () => {
    it('lists the series the party can see, named by their identity', () => {
        const html = picker({});
        expect(html).toContain('fx / EUR / fx_option / rate_lnvol · ccy=USD');
        expect(html).toContain('Choose a series, or type its identity below');
    });

    it('lists a series with no shape and does not let it be chosen', () => {
        const html = picker({});
        expect(html).toContain('fx / EUR / fx_spot / rate · ccy=GBP (no shape declared)');
        expect(html).toMatch(/<option[^>]*value="99999999[^>]*disabled/);
        expect(html).not.toMatch(/<option[^>]*value="77777777[^>]*disabled/);
    });

    it('says why a list can be empty, and does not read as an error', () => {
        const html = picker({ list: { series: [], truncated: false } });
        expect(html).toContain('Your party can see no series.');
        expect(html).toContain('may not be listed until its identity is projected');
        expect(html).not.toContain('role="alert"');
    });

    it('says when the list was cut, so it is not read as complete', () => {
        const html = picker({ list: { ...LIST, truncated: true } });
        expect(html).toContain('The list holds the first 2 series your party can see.');
    });

    it('shows a refused read as the reason, and a read in progress as such', () => {
        expect(picker({ failure: 'You do not have access to this.' })).toContain(
            'You do not have access to this.',
        );
        expect(picker({ list: undefined, loading: true })).toContain(
            'Reading the series you can see...',
        );
    });
});

const PARTY_A = 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa';
const PARTY_B = 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb';

function renderWith(client: QueryClient, node: React.ReactNode): string {
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter>{node}</MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('EvolutionPage', () => {
    it('never shows the list of the party the session has left', () => {
        // A switch of party leaves the cache as it was, so party A's list is still there.
        const client = new QueryClient();
        client.setQueryData(['market-series-list', PARTY_A], LIST);

        const asA = renderWith(client, <EvolutionPage partyId={PARTY_A} />);
        expect(asA).toContain('fx / EUR / fx_option / rate_lnvol · ccy=USD');

        const asB = renderWith(client, <EvolutionPage partyId={PARTY_B} />);
        expect(asB).not.toContain('fx / EUR / fx_option / rate_lnvol · ccy=USD');
    });

    it('opens on its form and reads nothing', () => {
        const html = render(<EvolutionPage partyId={PARTY_A} />);
        expect(html).toContain('Watch a curve or surface evolve');
        expect(html).toContain('Asset class');
        expect(html).toContain('Show where each value came from');
        expect(html).not.toContain('Over the range');
        expect(html).not.toContain('Reading the object');
    });
});
