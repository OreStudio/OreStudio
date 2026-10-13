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
import type { FeedFolder } from '../api/client.js';
import {
    folderChain,
    folderKey,
    folderNode,
    holdsFeeds,
    loadsFrom,
    madeChildren,
    mayOpen,
    parseKey,
    providerKey,
    scopeKey,
    scopeNodes,
} from './feedTree.js';

function folder(over: Partial<FeedFolder>): FeedFolder {
    return {
        id: 'c',
        parent_id: 'r',
        name: 'Collection',
        kind: 'collection',
        scope: 'party',
        provider_code: '',
        binding_mode: 'sandboxed',
        fx_feeds: 0,
        ir_feeds: 0,
        running_feeds: 0,
        has_children: true,
        ...over,
    };
}

const bloomberg = folder({ id: 'b', name: 'Bloomberg', provider_code: 'bloomberg', fx_feeds: 26 });
const ore = folder({ id: 'o', name: '2016 ORE Samples', fx_feeds: 13 });
const refinitiv = folder({
    id: 's1',
    name: 'Refinitiv',
    scope: 'system',
    provider_code: 'refinitiv',
    fx_feeds: 10,
});
const refinitivTwo = folder({
    id: 's2',
    name: 'Refinitiv more',
    scope: 'system',
    provider_code: 'refinitiv',
    fx_feeds: 5,
});
const collections = [bloomberg, ore, refinitiv, refinitivTwo];

describe('the feed tree', () => {
    it('writes a node as a key and reads the key back', () => {
        expect(parseKey(scopeKey('system'))).toEqual({ kind: 'scope', scope: 'system' });
        expect(parseKey(providerKey('test', 'bloomberg'))).toEqual({
            kind: 'provider',
            scope: 'test',
            code: 'bloomberg',
        });
        expect(parseKey(folderKey('abc'))).toEqual({ kind: 'folder', id: 'abc' });
    });

    it('refuses a key it did not write', () => {
        for (const key of ['', 'scope:other', 'scope:system:x', 'folder:', 'folder:a:b', 'nope']) {
            expect(parseKey(key), key).toBeUndefined();
        }
    });

    it('makes the two scopes, system first, from the collections', () => {
        const [system, test] = scopeNodes(collections);
        expect(system?.key).toBe('scope:system');
        expect(system?.folderIds).toEqual(['s1', 's2']);
        expect(test?.folderIds).toEqual(['b', 'o']);
        expect(test?.totals.fx).toBe(39);
    });

    it('puts a provider node under the system scope and a collection under the test scope', () => {
        const [system, test] = scopeNodes(collections);
        const providers = madeChildren(system as never, collections) ?? [];
        expect(providers.map((node) => node.key)).toEqual(['provider:system:refinitiv']);
        expect(providers[0]?.totals.fx).toBe(15);
        const held = madeChildren(test as never, collections) ?? [];
        expect(held.map((node) => node.key)).toEqual(['folder:b', 'folder:o']);
        expect(held[0]?.role).toBe('collection');
    });

    it('holds the collections of a provider that has several, and loads from the one it has', () => {
        const [system] = scopeNodes(collections);
        const provider = (madeChildren(system as never, collections) ?? [])[0];
        const kids = madeChildren(provider as never, collections) ?? [];
        expect(kids.map((node) => node.key)).toEqual(['folder:s1', 'folder:s2']);
        expect(loadsFrom(provider as never)).toBeUndefined();

        const lone = scopeNodes([refinitiv])[0];
        const only = (madeChildren(lone as never, [refinitiv]) ?? [])[0];
        expect(madeChildren(only as never, [refinitiv])).toBeUndefined();
        expect(loadsFrom(only as never)).toBe('s1');
    });

    it('loads the children of a folder, and lists the feeds of a folder that holds them', () => {
        const collection = folderNode(bloomberg);
        const kind = folderNode(folder({ id: 'k', kind: 'asset_class', parent_id: 'b' }));
        const leaf = folderNode(folder({ id: 'l', kind: 'instrument_type', has_children: false }));
        expect(loadsFrom(collection)).toBe('b');
        expect(loadsFrom(kind)).toBe('k');
        expect(loadsFrom(leaf)).toBeUndefined();
        expect(holdsFeeds(leaf)).toBe(true);
        expect(holdsFeeds(kind)).toBe(false);
        expect(mayOpen(leaf)).toBe(false);
        expect(mayOpen(collection)).toBe(true);
    });

    it('names the folders from a feed up to its collection, without the root', () => {
        const rows = [
            { id: 'r', parent_id: '', name: 'Synthetic', kind: 'root' },
            { id: 'c', parent_id: 'r', name: 'Bloomberg', kind: 'collection' },
            { id: 'a', parent_id: 'c', name: 'FX', kind: 'asset_class' },
            { id: 'i', parent_id: 'a', name: 'Spot', kind: 'instrument_type' },
        ];
        expect(folderChain(rows, 'i').map((row) => row.name)).toEqual(['Bloomberg', 'FX', 'Spot']);
        expect(folderChain(rows, 'nowhere')).toEqual([]);
    });

    it('ends a chain that loops', () => {
        const rows = [
            { id: 'a', parent_id: 'b', name: 'A', kind: 'asset_class' },
            { id: 'b', parent_id: 'a', name: 'B', kind: 'asset_class' },
        ];
        expect(folderChain(rows, 'a').length).toBeLessThanOrEqual(16);
    });
});
