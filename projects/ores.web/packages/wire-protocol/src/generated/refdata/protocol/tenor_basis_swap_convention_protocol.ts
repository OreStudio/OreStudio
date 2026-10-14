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
import type { TenorBasisSwapLongShort } from '../domain/tenor_basis_swap_long_short.js';
import type { TenorBasisSwapPayReceive } from '../domain/tenor_basis_swap_pay_receive.js';
import type { ConventionChange } from './convention_protocol.js';
import type { TenorBasisSwapPayReceiveChange } from './tenor_basis_swap_pay_receive_protocol.js';
import type { TenorBasisSwapLongShortChange } from './tenor_basis_swap_long_short_protocol.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TenorBasisSwapConventionKey {
    id: string;
}

export interface TenorBasisSwapConventionItem {
    key: TenorBasisSwapConventionKey;
    convention: Convention | null;
    pay_receive: TenorBasisSwapPayReceive[];
    long_short: TenorBasisSwapLongShort[];
}

export interface TenorBasisSwapConventionWrite {
    change: ConventionChange;
    pay_receive: TenorBasisSwapPayReceiveChange[];
    long_short: TenorBasisSwapLongShortChange[];
}

export interface GetTenorBasisSwapConventionRequest {
    key: TenorBasisSwapConventionKey;
}

export interface GetTenorBasisSwapConventionResponse {
    result: Result;
    item: TenorBasisSwapConventionItem | null;
}

export interface GetManyTenorBasisSwapConventionsRequest {
    keys: TenorBasisSwapConventionKey[];
}

export interface GetManyTenorBasisSwapConventionsResponse {
    result: Result;
    items: TenorBasisSwapConventionItem[];
}

export interface PutTenorBasisSwapConventionRequest {
    item: TenorBasisSwapConventionWrite;
    intent: ChangeIntent;
}

export interface PutTenorBasisSwapConventionResponse {
    result: Result;
    item: TenorBasisSwapConventionItem | null;
}

export interface PutManyTenorBasisSwapConventionsRequest {
    items: TenorBasisSwapConventionWrite[];
    intent: ChangeIntent;
}

export interface PutManyTenorBasisSwapConventionsResponse {
    result: Result;
    items: TenorBasisSwapConventionItem[];
}

export const subjects = {
    get_tenor_basis_swap_convention_request: 'refdata.v1.tenor_basis_swap_conventions.get',
    get_many_tenor_basis_swap_conventions_request:
        'refdata.v1.tenor_basis_swap_conventions.get_many',
    put_tenor_basis_swap_convention_request: 'refdata.v1.tenor_basis_swap_conventions.put',
    put_many_tenor_basis_swap_conventions_request:
        'refdata.v1.tenor_basis_swap_conventions.put_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_tenor_basis_swap_convention_request: true,
    get_many_tenor_basis_swap_conventions_request: true,
    put_tenor_basis_swap_convention_request: true,
    put_many_tenor_basis_swap_conventions_request: true,
} as const;
