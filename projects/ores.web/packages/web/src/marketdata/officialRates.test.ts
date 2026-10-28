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

import { describe, expect, it } from 'vitest';
import type { OfficialFixings, OfficialProvider } from '../api/client.js';
import { currenciesOf, pairValues, planPair, seriesOf } from './officialRates.js';

function provider(code: string, pivot: string, quotes: readonly string[]): OfficialProvider {
    return {
        code,
        version: 3,
        name: code.toUpperCase(),
        description: '',
        terms_url: `https://${code}.example/terms`,
        terms_credit: '',
        allows_storage: true,
        terms_accepted: true,
        terms_accepted_by: 'super_admin',
        terms_accepted_at: null,
        sources: [],
        series: quotes.map((quote) => ({
            id: `${code}-${quote}`,
            oresmd_uri: `oresmd://fx/${pivot}?type=fixing&index=fx&source=${code.toUpperCase()}&ccy=${quote}`,
            base: pivot,
            quote,
        })),
    };
}

const ecb = provider('ecb', 'EUR', ['USD', 'GBP']);
const boc = provider('boc', 'CAD', ['EUR', 'USD']);

describe('planPair', () => {
    it('gives a pair whose base is the pivot as published', () => {
        expect(planPair(ecb, 'EUR', 'USD')?.route).toBe('direct');
    });

    it('gives a pair whose quote is the pivot as its inverse', () => {
        expect(planPair(ecb, 'USD', 'EUR')?.route).toBe('inverse');
    });

    it('crosses two fixings when neither leg is the pivot', () => {
        const plan = planPair(boc, 'EUR', 'USD');
        expect(plan?.route).toBe('cross');
        expect(seriesOf(plan === undefined ? [] : [plan])).toEqual(['boc-USD', 'boc-EUR']);
    });

    it('refuses a pair the provider does not quote, and a currency against itself', () => {
        expect(planPair(ecb, 'EUR', 'JPY')).toBeUndefined();
        expect(planPair(ecb, 'EUR', 'EUR')).toBeUndefined();
    });
});

describe('pairValues', () => {
    const fixings: OfficialFixings = {
        'ecb-USD': [
            { date: '2026-10-26', value: '1.1600' },
            { date: '2026-10-27', value: '1.1500' },
        ],
        'boc-EUR': [
            { date: '2026-10-26', value: '1.6000' },
            { date: '2026-10-27', value: '1.6100' },
        ],
        'boc-USD': [{ date: '2026-10-27', value: '1.4000' }],
    };

    it('reads a direct pair as stored', () => {
        const plan = planPair(ecb, 'EUR', 'USD');
        expect(plan && pairValues(plan, fixings).get('2026-10-27')).toBeCloseTo(1.15, 10);
    });

    it('inverts a pair quoted the other way', () => {
        const plan = planPair(ecb, 'USD', 'EUR');
        expect(plan && pairValues(plan, fixings).get('2026-10-27')).toBeCloseTo(1 / 1.15, 10);
    });

    it('crosses through the pivot and leaves out a date missing a leg', () => {
        const plan = planPair(boc, 'EUR', 'USD');
        const values = plan === undefined ? new Map<string, number>() : pairValues(plan, fixings);
        expect(values.get('2026-10-27')).toBeCloseTo(1.4 / 1.61, 10);
        expect(values.has('2026-10-26')).toBe(false);
    });
});

describe('currenciesOf', () => {
    it('lists every currency once, the pivots included, sorted', () => {
        expect(currenciesOf([ecb, boc])).toEqual(['CAD', 'EUR', 'GBP', 'USD']);
    });
});
