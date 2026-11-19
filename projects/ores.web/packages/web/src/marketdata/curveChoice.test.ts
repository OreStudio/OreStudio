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
import {
    besideChoices,
    currenciesOf,
    curvesOf,
    differentialOf,
    pairOf,
    type CurveOption,
} from './curveChoice.js';

function curve(
    id: string,
    currency: string,
    role: string,
    discountId = '',
    indexTenor = '',
): CurveOption {
    return { id, name: id, currency, role, indexTenor, discountId };
}

const USD_SOFR = curve('USD-SOFR', 'USD', 'FUNDING', '', '1D');
const USD_LIBOR_3M = curve('USD-LIBOR-3M', 'USD', 'PROJECTION', 'USD-SOFR', '3M');
const USD_LIBOR_1M = curve('USD-LIBOR-1M', 'USD', 'PROJECTION', 'USD-SOFR', '1M');
const EUR_ESTR = curve('EUR-ESTR', 'EUR', 'FUNDING', '', '1D');
const EUR_EURIBOR_6M = curve('EUR-EURIBOR-6M', 'EUR', 'PROJECTION', 'EUR-ESTR', '6M');
const ALL = [USD_LIBOR_3M, EUR_EURIBOR_6M, USD_SOFR, USD_LIBOR_1M, EUR_ESTR];

describe('the curves the analytics offers', () => {
    it('lists each currency once, in code order', () => {
        expect(currenciesOf(ALL)).toEqual(['EUR', 'USD']);
    });

    it("lists one currency's curves, its funding curve first", () => {
        expect(curvesOf(ALL, 'USD').map((c) => c.id)).toEqual([
            'USD-SOFR',
            'USD-LIBOR-1M',
            'USD-LIBOR-3M',
        ]);
    });

    it('offers the same currency beside a curve, the curve it discounts off first', () => {
        const choices = besideChoices(ALL, USD_LIBOR_3M, false);
        expect(choices.same.map((c) => c.id)).toEqual(['USD-SOFR', 'USD-LIBOR-1M']);
        expect(choices.other).toEqual([]);
    });

    it("offers other currencies' overnight curves only when asked, and only beside an overnight curve", () => {
        expect(besideChoices(ALL, USD_SOFR, false).other).toEqual([]);
        expect(besideChoices(ALL, USD_SOFR, true).other.map((c) => c.id)).toEqual(['EUR-ESTR']);
        expect(besideChoices(ALL, USD_LIBOR_3M, true).other).toEqual([]);
    });
});

describe('the pairs the analytics explains', () => {
    it('teaches the funding and projection lesson within a currency, in either order', () => {
        expect(pairOf(USD_LIBOR_3M, USD_SOFR)).toEqual({
            funding: USD_SOFR,
            projection: USD_LIBOR_3M,
        });
        expect(pairOf(USD_SOFR, USD_LIBOR_3M)?.funding).toBe(USD_SOFR);
        expect(pairOf(USD_LIBOR_1M, USD_LIBOR_3M)).toBeUndefined();
    });

    it('shows the differential only for two currencies overnight against overnight', () => {
        expect(differentialOf(USD_SOFR, EUR_ESTR)).toEqual({ left: USD_SOFR, right: EUR_ESTR });
        expect(differentialOf(USD_SOFR, USD_LIBOR_3M)).toBeUndefined();
        expect(differentialOf(USD_SOFR, EUR_EURIBOR_6M)).toBeUndefined();
        expect(differentialOf(USD_SOFR, undefined)).toBeUndefined();
    });
});
