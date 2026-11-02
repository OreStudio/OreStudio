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
import type { CrossCurrencyBasisFlatLeg } from '../domain/cross_currency_basis_flat_leg.js';
import type { CrossCurrencyBasisSpreadLeg } from '../domain/cross_currency_basis_spread_leg.js';
import type { CrossCurrencyBasisSwapTerms } from '../domain/cross_currency_basis_swap_terms.js';
import type { ConventionChange } from './convention_protocol.js';
import type { CrossCurrencyBasisFlatLegChange } from './cross_currency_basis_flat_leg_protocol.js';
import type { CrossCurrencyBasisSpreadLegChange } from './cross_currency_basis_spread_leg_protocol.js';
import type { CrossCurrencyBasisSwapTermsChange } from './cross_currency_basis_swap_terms_protocol.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CrossCurrencyBasisConventionKey {
    id: string;
}

export interface CrossCurrencyBasisConventionItem {
    key: CrossCurrencyBasisConventionKey;
    convention: Convention | null;
    flat_leg: CrossCurrencyBasisFlatLeg[];
    spread_leg: CrossCurrencyBasisSpreadLeg[];
    swap_terms: CrossCurrencyBasisSwapTerms[];
}

export interface CrossCurrencyBasisConventionWrite {
    change: ConventionChange;
    flat_leg: CrossCurrencyBasisFlatLegChange[];
    spread_leg: CrossCurrencyBasisSpreadLegChange[];
    swap_terms: CrossCurrencyBasisSwapTermsChange[];
}

export interface GetCrossCurrencyBasisConventionRequest {
    key: CrossCurrencyBasisConventionKey;
}

export interface GetCrossCurrencyBasisConventionResponse {
    result: Result;
    item: CrossCurrencyBasisConventionItem | null;
}

export interface GetManyCrossCurrencyBasisConventionsRequest {
    keys: CrossCurrencyBasisConventionKey[];
}

export interface GetManyCrossCurrencyBasisConventionsResponse {
    result: Result;
    items: CrossCurrencyBasisConventionItem[];
}

export interface PutCrossCurrencyBasisConventionRequest {
    item: CrossCurrencyBasisConventionWrite;
    intent: ChangeIntent;
}

export interface PutCrossCurrencyBasisConventionResponse {
    result: Result;
    item: CrossCurrencyBasisConventionItem | null;
}

export interface PutManyCrossCurrencyBasisConventionsRequest {
    items: CrossCurrencyBasisConventionWrite[];
    intent: ChangeIntent;
}

export interface PutManyCrossCurrencyBasisConventionsResponse {
    result: Result;
    items: CrossCurrencyBasisConventionItem[];
}

export const subjects = {
    get_cross_currency_basis_convention_request: 'refdata.v1.cross_currency_basis_conventions.get',
    get_many_cross_currency_basis_conventions_request:
        'refdata.v1.cross_currency_basis_conventions.get_many',
    put_cross_currency_basis_convention_request: 'refdata.v1.cross_currency_basis_conventions.put',
    put_many_cross_currency_basis_conventions_request:
        'refdata.v1.cross_currency_basis_conventions.put_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_cross_currency_basis_convention_request: true,
    get_many_cross_currency_basis_conventions_request: true,
    put_cross_currency_basis_convention_request: true,
    put_many_cross_currency_basis_conventions_request: true,
} as const;
