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
import type { AdapterStatus, FeedFolder } from '../../api/client.js';
import { servedTotals, sourcesOf } from './adapterStatus.js';
import {
    collectionOf,
    feedPageOf,
    filterOf,
    folderChain,
    folderNode,
    loadsFrom,
    madeChildren,
    mayOpen,
    parseKey,
    providerKey,
    providerNodes,
} from './feedTree.js';
import { providersOf } from './providers.js';
import { stateOf, totalOf, totalsOf } from './totals.js';

function folder(
    id: string,
    name: string,
    kind: string,
    provider: string,
    fx = 0,
    ir = 0,
): FeedFolder {
    return {
        id,
        parent_id: 'root',
        name,
        kind,
        provider_code: provider,
        fx_feeds: fx,
        ir_feeds: ir,
        has_children: kind !== 'instrument_type',
    };
}

const bloomberg = folder('b', 'Bloomberg', 'collection', 'bloomberg', 26, 29);
const samples = folder('s', '2016 ORE Samples', 'collection', 'ore', 13, 4);
const more = folder('m', 'Bloomberg Extra', 'collection', 'bloomberg', 1, 0);

/** One adapter that serves every id it was demanded, so its provider's feeds run. */
function serving(provider: string): AdapterStatus {
    return {
        adapter_id: `${provider}-sim`,
        provider_code: provider,
        state: 'serving',
        last_heartbeat_at: '2026-10-25 15:00:00Z',
        heartbeat_counter: 4,
        served_count: 2,
        unknown_count: 0,
        demanded_count: 2,
        routes_seen: 1,
        ticks_last_minute: 0,
        last_tick_age_ms: -1,
        accepted: [],
        rejected: [],
    };
}

const SERVED = sourcesOf([serving('bloomberg')]);

describe('the totals under a node', () => {
    it('adds the folders of each kind and reads their state from the provider source', () => {
        const totals = totalsOf([bloomberg, samples]);
        expect(totalOf(totals)).toBe(72);
        expect(stateOf(servedTotals(totals, SERVED.get('bloomberg')))).toBe('running');
        expect(stateOf(servedTotals(totals, SERVED.get('ore')))).toBe('stopped');
        expect(stateOf(servedTotals(totalsOf([bloomberg]), SERVED.get('bloomberg')))).toBe(
            'running',
        );
        expect(stateOf(servedTotals(totalsOf([bloomberg]), SERVED.get('ore')))).toBe('stopped');
        expect(stateOf(servedTotals(totalsOf([]), SERVED.get('bloomberg')))).toBe('empty');
    });
});

describe('the providers of the collections', () => {
    it('groups collections by provider code, in code order', () => {
        const entries = providersOf([samples, bloomberg, more]);
        expect(entries.map((entry) => entry.code)).toEqual(['bloomberg', 'ore']);
        expect(entries[0]?.collections).toHaveLength(2);
        expect(totalOf(entries[0]?.totals ?? totalsOf([]))).toBe(56);
    });
});

describe('the feed tree', () => {
    it('has a node for each provider', () => {
        const nodes = providerNodes([bloomberg, samples]);
        expect(nodes.map((node) => node.key)).toEqual(['provider:bloomberg', 'provider:ore']);
        expect(filterOf(nodes[0])).toEqual({ provider: 'bloomberg' });
    });

    it('opens a provider with one collection onto the folders under it', () => {
        const [node] = providerNodes([bloomberg]);
        expect(node && madeChildren(node, [bloomberg])).toBeUndefined();
        expect(node && loadsFrom(node)).toBe('b');
    });

    it('opens a provider with several collections onto the collections', () => {
        const [node] = providerNodes([bloomberg, more]);
        const kids = node === undefined ? undefined : madeChildren(node, [bloomberg, more]);
        expect(kids?.map((kid) => kid.name)).toEqual(['Bloomberg', 'Bloomberg Extra']);
    });

    it('narrows a folder by its id, and a leaf has nothing to open', () => {
        const leaf = folderNode(folder('i', 'Spot', 'instrument_type', 'ore'));
        expect(filterOf(leaf)).toEqual({ folder: 'i' });
        expect(mayOpen(leaf)).toBe(false);
        expect(mayOpen(folderNode(bloomberg))).toBe(true);
        expect(filterOf(undefined)).toEqual({});
    });

    it('takes a key apart', () => {
        expect(parseKey(providerKey('ore'))).toEqual({ kind: 'provider', code: 'ore' });
        expect(parseKey('folder:abc')).toEqual({ kind: 'folder', id: 'abc' });
        expect(parseKey('scope:system')).toBeUndefined();
        expect(parseKey('folder:')).toBeUndefined();
        expect(parseKey('')).toBeUndefined();
    });

    it('lists the folders from a feed up to its collection, outermost first', () => {
        const rows = [
            { id: 'r', parent_id: '', name: 'Synthetic', kind: 'root' },
            { id: 'b', parent_id: 'r', name: 'Bloomberg', kind: 'collection' },
            { id: 'a', parent_id: 'b', name: 'FX', kind: 'asset_class' },
        ];
        expect(folderChain(rows, 'a').map((row) => row.name)).toEqual(['Bloomberg', 'FX']);
        expect(folderChain(rows, 'missing')).toEqual([]);
    });

    it('opens a feed on the page of its kind', () => {
        expect(feedPageOf({ kind: 'fx_spot', id: 'x/1' })).toBe('/market-data/synthetic/fx/x%2F1');
        expect(feedPageOf({ kind: 'ir_curve', id: 'y' })).toBe('/market-data/synthetic/ir/y');
    });

    it('names the collection a person is looking at', () => {
        const rows = [
            { id: 'r', parent_id: '', name: 'Synthetic', kind: 'root' },
            { id: 'b', parent_id: 'r', name: 'Bloomberg', kind: 'collection' },
            { id: 'a', parent_id: 'b', name: 'FX', kind: 'asset_class' },
        ];
        const held = [bloomberg, samples, more];
        expect(collectionOf(parseKey('provider:ore'), held, rows)).toBe('s');
        expect(collectionOf(parseKey('provider:bloomberg'), held, rows)).toBe('');
        expect(collectionOf(parseKey('folder:a'), held, rows)).toBe('b');
        expect(collectionOf(undefined, held, rows)).toBe('');
    });
});
