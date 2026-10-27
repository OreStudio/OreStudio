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
 * @brief One pillar of a published set.
 *
 * The pillar is the coordinate the datum stands at -- the term of an interest
 * rate curve -- and the value is the text the publish wrote, exact. The kind
 * says what the value denotes, so a reader that shows a rate and the discount
 * factor it implies knows which of the two it was given.
 */
export interface PublishedCurvePoint {
    pillar: string;
    value: string;
    value_kind: string;
    oresmd_uri: string;
    observation_datetime: string;
}

/**
 * @brief One published set, or the candidate that waits to become one.
 *
 * The state is a function of the record's own facts and is stated here rather
 * than stored: `withdrawn` when the set was taken out of use, `superseded` when
 * a later set replaced it, `published` when a person applied a request and the
 * set is in use, and `candidate` when the request still waits. A candidate
 * holds no publisher and no published instant, so a reader cannot mistake it
 * for the set a desk prices from.
 *
 * A set read from the list carries its point count and no points; a set read
 * for one curve carries its points, because the screen draws them.
 */
export interface PublishedCurveSet {
    publish_id: string;
    request_id: string;
    curve: string;
    series_id: string;
    series_uri: string;
    bootstrap_config_id: string;
    party: string;
    party_id: string;
    as_of: string;
    published_by: string;
    published_at: string | null;
    state_code: string;
    point_count: number;
    withdrawn_by: string;
    withdrawn_at: string | null;
    withdrawn_reason_code: string;
    withdrawal_request_id: string;
    superseded_at: string | null;
    superseded_by_request_id: string;
    superseded_as_of: string | null;
    points: PublishedCurvePoint[];
}

/**
 * @brief The published sets of the tenant, newest first, and the candidates.
 *
 * A reader sees the sets in use by default. A candidate is a set nobody has
 * approved and an ended set is one that was in use and is not any more, so
 * neither is drawn unless the caller asks for it: a candidate read as the set
 * a desk prices from would be a lie, and an ended set is reached on purpose.
 *
 * The total is the size of the whole filtered set and not of the page, so a
 * screen can page without a second read.
 */
export interface ListPublishedCurvesRequest {
    party_id: string;
    search: string;
    include_candidates: boolean;
    include_ended: boolean;
    request_id_one_of: string[];
    offset: number;
    limit: number;
}

/**
 * @brief The rows of the published curve list.
 *
 * The order is the screen's and not the caller's: the sets in use come first,
 * newest published instant first, and the candidates follow in as-of order, so
 * a set nobody approved never stands above a set a desk prices from.
 */
export interface ListPublishedCurvesResponse {
    sets: PublishedCurveSet[];
    total: number;
    success: boolean;
    message: string;
}

/**
 * @brief The last sets published for one series, newest last, with their points.
 *
 * The sets come back oldest first, so a reader draws them in the order they
 * happened and colours the newest one strongest. A candidate of the same curve
 * is included when it is asked for, with no published instant, so the
 * comparison tab reads the baseline and the candidate from one answer.
 */
export interface GetPublishedCurveSetsRequest {
    series_id: string;
    limit: number;
    include_candidates: boolean;
}

/**
 * @brief One curve's sets, oldest first, each with the points it holds.
 *
 * An ended set is returned like any other published set, because it was
 * published; its state says that it is not the set in use.
 */
export interface GetPublishedCurveSetsResponse {
    sets: PublishedCurveSet[];
    published_count: number;
    success: boolean;
    message: string;
}

/**
 * @brief Takes a published set out of use, as a new version of its record.
 *
 * The set is not deleted and its values are not cleared: the record keeps the
 * publisher, the published instant and the set's points, and gains the account
 * that ended it, the instant and the reason. A set that already ended cannot be
 * ended again, and a set that was never published cannot be withdrawn, because
 * a candidate is not in use.
 *
 * A withdrawal names no request when the tenant lets a trader act alone, and
 * names one when a decision stands behind the act. A request authorises one
 * withdrawal, so a request that already ended a set ends nothing else.
 */
export interface WithdrawCurveRequest {
    publish_id: string;
    reason_code: string;
    comment: string;
    approval_request_id: string;
}

/**
 * @brief The instant and the account the record now holds.
 *
 * The answer states the withdrawal the record carries so a screen can show the
 * ended set without reading it again.
 */
export interface WithdrawCurveResponse {
    withdrawn_at: string;
    withdrawn_by: string;
    success: boolean;
    message: string;
}

export const subjects = {
    list_published_curves_request: 'marketdata.v1.ops.list_published_curves',
    get_published_curve_sets_request: 'marketdata.v1.ops.get_published_curve_sets',
    withdraw_curve_request: 'marketdata.v1.ops.withdraw_curve',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_published_curves_request: true,
    get_published_curve_sets_request: true,
    withdraw_curve_request: true,
} as const;
