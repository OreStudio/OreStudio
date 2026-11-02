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
import type { CommodityFutureAveragingTerms } from '../domain/commodity_future_averaging_terms.js';
import type { CommodityFutureContractTerms } from '../domain/commodity_future_contract_terms.js';
import type { CommodityFutureExpiryAnchor } from '../domain/commodity_future_expiry_anchor.js';
import type { CommodityFutureOffPeakIndex } from '../domain/commodity_future_off_peak_index.js';
import type { CommodityFutureOptionExpiry } from '../domain/commodity_future_option_expiry.js';
import type { Convention } from '../domain/convention.js';
import type { ConventionChange } from './convention_protocol.js';
import type { CommodityFutureAveragingTermsChange } from './commodity_future_averaging_terms_protocol.js';
import type { CommodityFutureContractTermsChange } from './commodity_future_contract_terms_protocol.js';
import type { CommodityFutureExpiryAnchorChange } from './commodity_future_expiry_anchor_protocol.js';
import type { CommodityFutureOffPeakIndexChange } from './commodity_future_off_peak_index_protocol.js';
import type { CommodityFutureOptionExpiryChange } from './commodity_future_option_expiry_protocol.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CommodityFutureConventionKey {
    id: string;
}

export interface CommodityFutureConventionItem {
    key: CommodityFutureConventionKey;
    convention: Convention | null;
    averaging_terms: CommodityFutureAveragingTerms[];
    contract_terms: CommodityFutureContractTerms[];
    expiry_anchor: CommodityFutureExpiryAnchor[];
    off_peak_index: CommodityFutureOffPeakIndex[];
    option_expiry: CommodityFutureOptionExpiry[];
}

export interface CommodityFutureConventionWrite {
    change: ConventionChange;
    averaging_terms: CommodityFutureAveragingTermsChange[];
    contract_terms: CommodityFutureContractTermsChange[];
    expiry_anchor: CommodityFutureExpiryAnchorChange[];
    off_peak_index: CommodityFutureOffPeakIndexChange[];
    option_expiry: CommodityFutureOptionExpiryChange[];
}

export interface GetCommodityFutureConventionRequest {
    key: CommodityFutureConventionKey;
}

export interface GetCommodityFutureConventionResponse {
    result: Result;
    item: CommodityFutureConventionItem | null;
}

export interface GetManyCommodityFutureConventionsRequest {
    keys: CommodityFutureConventionKey[];
}

export interface GetManyCommodityFutureConventionsResponse {
    result: Result;
    items: CommodityFutureConventionItem[];
}

export interface PutCommodityFutureConventionRequest {
    item: CommodityFutureConventionWrite;
    intent: ChangeIntent;
}

export interface PutCommodityFutureConventionResponse {
    result: Result;
    item: CommodityFutureConventionItem | null;
}

export interface PutManyCommodityFutureConventionsRequest {
    items: CommodityFutureConventionWrite[];
    intent: ChangeIntent;
}

export interface PutManyCommodityFutureConventionsResponse {
    result: Result;
    items: CommodityFutureConventionItem[];
}

export const subjects = {
    get_commodity_future_convention_request: 'refdata.v1.commodity_future_conventions.get',
    get_many_commodity_future_conventions_request:
        'refdata.v1.commodity_future_conventions.get_many',
    put_commodity_future_convention_request: 'refdata.v1.commodity_future_conventions.put',
    put_many_commodity_future_conventions_request:
        'refdata.v1.commodity_future_conventions.put_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_commodity_future_convention_request: true,
    get_many_commodity_future_conventions_request: true,
    put_commodity_future_convention_request: true,
    put_many_commodity_future_conventions_request: true,
} as const;
