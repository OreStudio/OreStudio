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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
import type { Convention } from '../domain/convention.js';
import type { CrossCurrencyFixFloatFixedLeg } from '../domain/cross_currency_fix_float_fixed_leg.js';
import type { CrossCurrencyFixFloatFloatLeg } from '../domain/cross_currency_fix_float_float_leg.js';
import type { CrossCurrencyFixFloatSwapTerms } from '../domain/cross_currency_fix_float_swap_terms.js';
import type { ConventionChange } from './convention_protocol.js';
import type { CrossCurrencyFixFloatFixedLegChange } from './cross_currency_fix_float_fixed_leg_protocol.js';
import type { CrossCurrencyFixFloatFloatLegChange } from './cross_currency_fix_float_float_leg_protocol.js';
import type { CrossCurrencyFixFloatSwapTermsChange } from './cross_currency_fix_float_swap_terms_protocol.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CrossCurrencyFixFloatConventionKey {
    id: string;
}

export interface CrossCurrencyFixFloatConventionItem {
    key: CrossCurrencyFixFloatConventionKey;
    convention: Convention | null;
    fixed_leg: CrossCurrencyFixFloatFixedLeg[];
    float_leg: CrossCurrencyFixFloatFloatLeg[];
    swap_terms: CrossCurrencyFixFloatSwapTerms[];
}

export interface CrossCurrencyFixFloatConventionWrite {
    change: ConventionChange;
    fixed_leg: CrossCurrencyFixFloatFixedLegChange[];
    float_leg: CrossCurrencyFixFloatFloatLegChange[];
    swap_terms: CrossCurrencyFixFloatSwapTermsChange[];
}

export interface GetCrossCurrencyFixFloatConventionRequest {
    key: CrossCurrencyFixFloatConventionKey;
}

export interface GetCrossCurrencyFixFloatConventionResponse {
    result: Result;
    item: CrossCurrencyFixFloatConventionItem | null;
}

export interface GetManyCrossCurrencyFixFloatConventionsRequest {
    keys: CrossCurrencyFixFloatConventionKey[];
}

export interface GetManyCrossCurrencyFixFloatConventionsResponse {
    result: Result;
    items: CrossCurrencyFixFloatConventionItem[];
}

export interface PutCrossCurrencyFixFloatConventionRequest {
    item: CrossCurrencyFixFloatConventionWrite;
    intent: ChangeIntent;
}

export interface PutCrossCurrencyFixFloatConventionResponse {
    result: Result;
    item: CrossCurrencyFixFloatConventionItem | null;
}

export interface PutManyCrossCurrencyFixFloatConventionsRequest {
    items: CrossCurrencyFixFloatConventionWrite[];
    intent: ChangeIntent;
}

export interface PutManyCrossCurrencyFixFloatConventionsResponse {
    result: Result;
    items: CrossCurrencyFixFloatConventionItem[];
}

export const subjects = {
    get_cross_currency_fix_float_convention_request:
        'refdata.v1.cross_currency_fix_float_conventions.get',
    get_many_cross_currency_fix_float_conventions_request:
        'refdata.v1.cross_currency_fix_float_conventions.get_many',
    put_cross_currency_fix_float_convention_request:
        'refdata.v1.cross_currency_fix_float_conventions.put',
    put_many_cross_currency_fix_float_conventions_request:
        'refdata.v1.cross_currency_fix_float_conventions.put_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_cross_currency_fix_float_convention_request: true,
    get_many_cross_currency_fix_float_conventions_request: true,
    put_cross_currency_fix_float_convention_request: true,
    put_many_cross_currency_fix_float_conventions_request: true,
} as const;
