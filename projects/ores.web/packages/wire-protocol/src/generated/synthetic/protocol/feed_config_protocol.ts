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
 * @brief Request to start one feed on demand, keyed by config_id.
 *
 * One kind-agnostic request shape for every asset class: the server resolves
 * the config (of whichever kind it is), its children, and the refdata context
 * from config_id and builds the feed via the producer factory -- the IR curve
 * pattern, applied uniformly. The client never names a source_name or supplies
 * producer parameters; a config's own identity is the whole request. Replaces
 * the per-kind start requests (market_feed_config's client-supplied-params
 * pass-through and ir_curve_feed_config's config_id request).
 */
export interface StartFeedRequest {
    config_id: string;
}

/**
 * @brief Whether the feed started, and why not when it did not.
 */
export interface StartFeedResponse {
    success: boolean;
    message: string;
}

/**
 * @brief Request to stop running feed(s), identified by config_id or source_name.
 *
 * config_id is preferred: it is resolved server-side to the config's
 * source_name, the same way start resolves it -- the client never needs to
 * know the "synthetic.<pair>" / "ir_curve.<ccy>.<idx>" naming conventions. If
 * both config_id and source_name are empty, stops all running feeds of every
 * kind.
 */
export interface StopFeedRequest {
    /** @brief Preferred: resolved server-side to source_name. */
    config_id: string;
    /** @brief Used only if config_id is empty; empty too stops every feed. */
    source_name: string;
}

/**
 * @brief Whether the feed stopped, and why not when it did not.
 */
export interface StopFeedResponse {
    success: boolean;
    message: string;
}

/**
 * @brief How far another provider's fixing of the pair stands from the level a feed took.
 */
export interface ProviderDispersion {
    /** @brief The other provider. */
    provider_code: string;
    /** @brief The other provider's item, as the oresmd URI that names it. */
    item: string;
    /** @brief The distance from the level the feed took, in basis points of that level. */
    basis_points: number;
}

/**
 * @brief How a feed's daily variance started: the model, and whether history fitted it.
 */
export interface VarianceStart {
    /** @brief "constant" or "gjr_garch". */
    model: string;
    /** @brief The approved variance calibration the run took its shape from. */
    calibration_id: string;
    /** @brief The version of that calibration the run read. */
    calibration_version: number;
    /** @brief True when the calibration was fitted from fixings; false when it is an authored shape. */
    fitted: boolean;
    /** @brief The reaction to a day's move. */
    alpha: number;
    /** @brief The extra reaction to a fall of the pair. */
    gamma: number;
    /** @brief The carry-over of a day's variance. */
    beta: number;
    /** @brief alpha + beta + gamma times the mixture's p_minus, below one. */
    persistence: number;
    /** @brief The first day's variance over the long-run level: above one when history ended turbulent. */
    initial_ratio: number;
    /** @brief How many daily returns the fit read; zero when nothing was fitted. */
    returns: number;
    /** @brief Why the authored shape was kept, when it was. */
    note: string;
}

/**
 * @brief Where a feed's starting value came from, or that it kept its fixed level.
 *
 * The record belongs to the run, not to the config: a feed that stops and
 * starts again records where the new run started.
 */
export interface FeedStart {
    /** @brief True when a fixing set the level; false when the feed kept its fixed level. */
    from_fixing: boolean;
    /** @brief The fixing's item, as the oresmd URI that names it. */
    item: string;
    /** @brief The provider that published the fixing. */
    provider_code: string;
    /** @brief The provider's own id of the item, or empty when no mapping names it. */
    vendor_id: string;
    /** @brief The fixing's value as the publisher wrote it, in decimal text. */
    value: string;
    /** @brief True when the item quotes the feed's pair the other way round. */
    inverted: boolean;
    /** @brief The date of the fixing, which is the publisher's own time for a daily fixing. */
    fixing_date: string | null;
    /** @brief When the service stored the fixing. */
    received_at: string | null;
    /** @brief How many calendar days of fixings the window covered. */
    window_days: number;
    /** @brief How many fixings of the window measured the spread. */
    used: number;
    /** @brief The measured hourly spread, when the window had one. */
    hourly_spread: number | null;
    /** @brief Why the feed kept its fixed level, or why its spread was not measured. */
    note: string;
    /** @brief How many days the fixing is older than the date the feed read on or before. */
    age_days: number | null;
    /** @brief The spread of the window's business-day log returns, in the feed's orientation. */
    daily_spread: number | null;
    /** @brief The mean business-day log return, times 252. */
    annual_drift: number | null;
    /** @brief The smallest business-day log return in the window. */
    min_return: number | null;
    /** @brief The largest business-day log return in the window. */
    max_return: number | null;
    /** @brief How many returns lie more than three spreads from zero. */
    jumps: number | null;
    /** @brief The other providers' fixings of the pair, and how far each stands from the level. */
    dispersion: ProviderDispersion[];
    /** @brief How the daily variance started, for a feed whose variance is modelled. */
    variance: VarianceStart | null;
}

/**
 * @brief One feed the service runs now, and where its starting value came from.
 *
 * A feed kind that starts from no published value carries no start, so it reads
 * as running and says nothing of where it started.
 */
export interface RunningFeed {
    /** @brief The source name the run publishes under. */
    source_name: string;
    /** @brief Where the run's starting value came from; absent when the kind has no such start. */
    start: FeedStart | null;
}

/**
 * @brief Request the feeds that run now, every kind, with where each started.
 */
export interface ListFeedsRequest {}

/**
 * @brief The feeds the service runs now, each with where it started, and whether
 * the read succeeded.
 */
export interface ListFeedsResponse {
    success: boolean;
    /** @brief The feeds that run, in source name order. */
    running_feeds: RunningFeed[];
}

/**
 * @brief The policy a class calibrates under: its own row, or the defaults when it has none.
 */
export interface CalibrationPolicySummary {
    /** @brief True when the class has a policy row; false when it calibrates on the defaults. */
    from_policy_row: boolean;
    /** @brief How many calendar days of fixings measure the spread. */
    window_days: number;
    /** @brief How many calendar days of fixings a variance calibration's input covers. */
    variance_window_days: number;
    /** @brief manual, a person approves every variance fit; auto, the policy approves each fit that passes its rules. */
    variance_approval_mode: string;
    /** @brief The fewest returns the policy approves a fit from. */
    variance_min_returns: number;
    /** @brief Whether the policy approves only a fit whose starts agree. */
    variance_require_starts_agree: boolean;
    /** @brief The lowest persistence the policy approves. */
    variance_min_persistence: number;
    /** @brief The highest persistence the policy approves. */
    variance_max_persistence: number;
    /** @brief The lowest average reaction, alpha plus half of gamma, the policy approves. */
    variance_min_reaction: number;
    /** @brief True when an item that quotes the pair the other way round is read as one over it. */
    allow_inverse: boolean;
    /** @brief What a feed does when no candidate has a fixing: keep_fixed or refuse. */
    fallback_code: string;
    /** @brief The accepted providers, first preferred. Empty means every provider, by code. */
    provider_order: string[];
}

/**
 * @brief One candidate of a feed's resolution, in rank order, and why it was or was not taken.
 */
export interface CandidateOutcome {
    /** @brief The candidate's item, as the oresmd URI that names it. */
    item: string;
    /** @brief The provider that publishes the item. */
    provider_code: string;
    /** @brief True when the item quotes the pair the other way round. */
    inverted: boolean;
    /** @brief The candidate's place in the resolution, first tried first. */
    rank: number;
    /** @brief chosen, skipped or not_reached. */
    state: string;
    /** @brief Why the candidate was chosen, skipped or not reached. */
    reason: string;
}

/**
 * @brief What a calibration run made of a feed's daily variance: the input it captured and the
 * calibration it fitted from it, stored when the run is applied.
 */
export interface VarianceCalibrationSummary {
    /** @brief The stored input; empty when the run was not applied. */
    input_id: string;
    /** @brief The stored calibration; empty when the run was not applied. */
    calibration_id: string;
    /** @brief "gjr_garch_qmle" for a fit, "authored" for an authored shape. */
    method: string;
    /** @brief "proposed" when the fit was made, "rejected" when it could not be. */
    state: string;
    /** @brief Why the fit was rejected; empty for a proposed one. */
    state_reason: string;
    /** @brief The fixing item the input was read from. */
    item: string;
    /** @brief The provider of that item. */
    provider_code: string;
    /** @brief True when the item quotes the pair the other way round. */
    inverted: boolean;
    /** @brief The window the input covers, in calendar days, from the policy. */
    window_days: number;
    /** @brief The last date of the window, as YYYY-MM-DD. */
    window_end: string;
    /** @brief The fixing dates the window held. */
    point_count: number;
    /** @brief The points whose return the fit used. */
    used_count: number;
    /** @brief The fitted reaction to a day's move; absent when rejected. */
    alpha: number | null;
    /** @brief The fitted extra reaction to a fall; absent when rejected. */
    gamma: number | null;
    /** @brief The fitted weight of yesterday's variance; absent when rejected. */
    beta: number | null;
    /** @brief alpha + gamma p_minus + beta; absent when rejected. */
    persistence: number | null;
    /** @brief The first day's variance over the long-run level; absent when rejected. */
    starting_variance_ratio: number | null;
    /** @brief How many starts the fit ran from. */
    starts: number;
    /** @brief True when every start reached the same optimum. */
    starts_agreed: boolean;
}

/**
 * @brief How one feed of the class resolved: the item it starts from, or its fallback or
 * refusal, and the exceptions it states against the policy.
 */
export interface CalibratedFeed {
    /** @brief The feed's config id. */
    config_id: string;
    /** @brief The source name the feed publishes under. */
    source_name: string;
    /** @brief The base currency of the feed's pair. */
    base_currency_code: string;
    /** @brief The quote currency of the feed's pair. */
    quote_currency_code: string;
    /** @brief True when the feed names its own items instead of deriving them from the policy. */
    explicit_items: boolean;
    /** @brief The feed's own window, when it overrides the policy's. */
    window_days_override: number | null;
    /** @brief True when no candidate has a fixing and the policy refuses to start the feed. */
    refused: boolean;
    /** @brief Where the feed starts: the item and its fixing, or why it keeps its fixed level. */
    start: FeedStart;
    /** @brief Every candidate the feed read, in rank order, with its outcome. */
    candidates: CandidateOutcome[];
    /** @brief True when the feed runs now, so a stored calibration applies at its next start. */
    running: boolean;
    /**
     * @brief What an applied calibration did to the feed: updated, unchanged, kept, refused or
     * failed. Empty when the calibration was not applied.
     */
    outcome: string;
    /** @brief The words of the outcome: the change written, or why nothing was written. */
    outcome_message: string;
    /** @brief The variance calibration of a GJR-GARCH feed that starts from a fixing; absent otherwise. */
    variance: VarianceCalibrationSummary | null;
    /** @brief The calibration the feed's config names and runs, as stored; absent when it names none. */
    current_variance: VarianceCalibrationSummary | null;
}

/**
 * @brief Resolve the feeds of one asset class under its policy, without starting any.
 *
 * Every feed of the class is resolved, or the one feed the request names. An applied
 * calibration writes the resolved level and deviations as a new version of each feed config
 * they change, under the reason system.calibration.
 */
export interface CalibrateRequest {
    /** @brief The asset class to calibrate, as its refdata code. */
    asset_class_code: string;
    /** @brief The one feed config to calibrate; empty calibrates every feed of the class. */
    config_id: string;
    /** @brief True to write the calibration; false only reports what it would be. */
    apply: boolean;
}

/**
 * @brief The policy the class calibrated under and the outcome of each of its feeds.
 */
export interface CalibrateResponse {
    success: boolean;
    message: string;
    /** @brief The policy the class calibrated under. */
    policy: CalibrationPolicySummary;
    /** @brief Every feed of the class, in source name order. */
    feeds: CalibratedFeed[];
}

/**
 * @brief Approve or reject one proposed variance calibration, with the reviewer's reason.
 *
 * Approving supersedes the pair's earlier approval and points every feed of the pair that named
 * it at the new one, each a new version of its config; rejecting leaves the feeds alone. Only a
 * proposed calibration can be reviewed, and each decision is a new version of the calibration.
 */
export interface ReviewVarianceCalibrationRequest {
    /** @brief The proposed calibration to decide. */
    calibration_id: string;
    /** @brief "approve" or "reject". */
    decision: string;
    /** @brief Why: kept as the calibration's state reason. Required. */
    reason: string;
}

/**
 * @brief What the review did: the calibration's new state, the approval it superseded, and the
 * feeds it pointed at the new calibration.
 */
export interface ReviewVarianceCalibrationResponse {
    /** @brief True when the decision was written. */
    success: boolean;
    /** @brief Why the decision was refused, or what it did. */
    message: string;
    /** @brief Why the review was refused: "invalid", "not_found" or "decided"; empty when it was not. */
    refusal: string;
    /** @brief The calibration's state after the review. */
    state: string;
    /** @brief The approval the new one superseded; empty when there was none. */
    superseded_id: string;
    /** @brief The source names of the feeds now pointed at the approved calibration. */
    repointed_feeds: string[];
}

export const subjects = {
    start_feed_request: 'synthetic.v1.ops.start_feed',
    stop_feed_request: 'synthetic.v1.ops.stop_feed',
    list_feeds_request: 'synthetic.v1.feed_configs.list',
    calibrate_request: 'synthetic.v1.ops.calibrate_feeds',
    review_variance_calibration_request: 'synthetic.v1.ops.review_variance_calibration',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    start_feed_request: true,
    stop_feed_request: true,
    list_feeds_request: true,
    calibrate_request: true,
    review_variance_calibration_request: true,
} as const;
