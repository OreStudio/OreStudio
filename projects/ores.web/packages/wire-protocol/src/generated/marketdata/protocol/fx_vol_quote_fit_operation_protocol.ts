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
/**
 * @brief One quoted point of a source's surface: its expiry, its strike label as quoted, such as
 * ATM, 25RR or 10BF, and its volatility.
 */
export interface FxVolQuote {
    expiry: string;
    label: string;
    volatility: number;
}

/**
 * @brief The quotes of one source, such as a vendor or the consensus of others, fitted on their
 * own.
 */
export interface FxVolQuoteSource {
    name: string;
    quotes: FxVolQuote[];
}

/**
 * @brief One point of a source's smile by delta: its expiry, its delta label, such as 25P, ATM or
 * 10C, and its volatility.
 */
export interface FxVolDeltaPoint {
    expiry: string;
    strike: string;
    volatility: number;
}

/**
 * @brief One source's smile by delta, expiry by expiry, or why it could not be fitted.
 */
export interface FxVolSourceFit {
    name: string;
    fitted: boolean;
    /**
     * @brief Why the source was not fitted; empty when it was.
     */
    message: string;
    /**
     * @brief The volatility at each delta of each fitted expiry, shortest expiry first and each
     * expiry's strikes from the put wing to the call wing.
     */
    points: FxVolDeltaPoint[];
    /**
     * @brief Each quoted expiry the fit left out, with why, such as "10Y: no pillar quoted".
     */
    unfitted_expiries: string[];
}

/**
 * @brief Reads the quotes of each source as a smile by delta: fits SSVI to the source's ATM, risk
 * reversals and butterflies in the pair's FX option convention, on the caller's party's published
 * forward set, and answers the volatility at 10P, 25P, ATM, 25C and 10C of each expiry. Writes
 * nothing.
 */
export interface FitFxVolQuotesRequest {
    /**
     * @brief The pair, such as EUR/USD.
     */
    pair_code: string;
    /**
     * @brief The instant the forward set is read at.
     */
    as_of: string;
    sources: FxVolQuoteSource[];
}

/**
 * @brief Each source's smile by delta, or why the request was refused.
 */
export interface FitFxVolQuotesResponse {
    success: boolean;
    message: string;
    /**
     * @brief The FX option convention the quotes were read in, such as EUR-USD-FXOPTION.
     */
    convention_id: string;
    /**
     * @brief The convention's delta type, such as PaSpot, which the deltas of the answer are in.
     */
    delta_type: string;
    fits: FxVolSourceFit[];
}

export const subjects = {
    fit_fx_vol_quotes_request: 'marketdata.v1.ops.fit_fx_vol_quotes',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    fit_fx_vol_quotes_request: true,
} as const;
