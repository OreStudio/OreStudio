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
    GENERIC_PROVIDER,
    providerName,
    providerOf,
    providersOf,
    scopeOf,
    scopesOf,
    stateOf,
    totalOf,
    totalsOf,
} from './feedOverview.js';

function collection(over: Partial<FeedFolder>): FeedFolder {
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

const bloomberg = collection({
    id: 'b',
    name: 'Bloomberg',
    provider_code: 'bloomberg',
    binding_mode: 'bound',
    fx_feeds: 26,
    ir_feeds: 33,
    running_feeds: 20,
});
const ore = collection({ id: 'o', name: '2016 ORE Samples', fx_feeds: 13, ir_feeds: 4 });
const system = collection({
    id: 's',
    name: 'Refinitiv',
    scope: 'system',
    provider_code: 'refinitiv',
    fx_feeds: 10,
    running_feeds: 10,
});

describe('the feed overview', () => {
    it('puts a system collection in the system scope and any other in the test scope', () => {
        expect(scopeOf(system)).toBe('system');
        expect(scopeOf(bloomberg)).toBe('test');
        expect(scopeOf(collection({ scope: 'tenant' }))).toBe('test');
    });

    it('stands a collection with no provider for the generic provider', () => {
        expect(providerOf(ore)).toBe(GENERIC_PROVIDER);
        expect(providerOf(bloomberg)).toBe('bloomberg');
    });

    it('adds the feeds of the collections in each scope', () => {
        const [systemScope, testScope] = scopesOf([bloomberg, ore, system]);
        expect(systemScope?.scope).toBe('system');
        expect(totalOf(systemScope?.totals ?? { fx: 0, ir: 0, running: 0 })).toBe(10);
        expect(testScope?.collections.map((c) => c.id)).toEqual(['b', 'o']);
        expect(testScope?.totals).toEqual({ fx: 39, ir: 37, running: 20 });
    });

    it('lists the scopes even when one holds nothing', () => {
        const scopes = scopesOf([bloomberg]);
        expect(scopes.map((entry) => entry.scope)).toEqual(['system', 'test']);
        expect(scopes[0]?.collections).toEqual([]);
        expect(stateOf(scopes[0]?.totals ?? { fx: 0, ir: 0, running: 0 })).toBe('empty');
    });

    it('groups the collections of a scope by provider, in order of code', () => {
        const providers = providersOf([ore, bloomberg]);
        expect(providers.map((entry) => entry.code)).toEqual(['bloomberg', 'generic']);
        expect(providers[0]?.bound).toBe(true);
        expect(providers[1]?.bound).toBe(false);
        expect(providers[1]?.collections.map((c) => c.id)).toEqual(['o']);
    });

    it('joins collections of one provider into one entry', () => {
        const second = collection({ id: 'b2', provider_code: 'bloomberg', fx_feeds: 4 });
        const providers = providersOf([bloomberg, second]);
        expect(providers).toHaveLength(1);
        expect(providers[0]?.totals.fx).toBe(30);
    });

    it('says running when all run, stopped when none, and partial between', () => {
        expect(stateOf({ fx: 2, ir: 1, running: 3 })).toBe('running');
        expect(stateOf({ fx: 2, ir: 1, running: 0 })).toBe('stopped');
        expect(stateOf({ fx: 2, ir: 1, running: 1 })).toBe('partial');
        expect(stateOf({ fx: 0, ir: 0, running: 0 })).toBe('empty');
    });

    it('sums folders', () => {
        expect(totalsOf([bloomberg, ore])).toEqual({ fx: 39, ir: 37, running: 20 });
    });

    it('names a provider from the catalogue, or from its code with a capital', () => {
        const catalogue = new Map([['ice', 'ICE Data Services']]);
        expect(providerName('ice', catalogue)).toBe('ICE Data Services');
        expect(providerName('bloomberg', catalogue)).toBe('Bloomberg');
        expect(providerName(GENERIC_PROVIDER, catalogue)).toBe('Generic');
    });
});
