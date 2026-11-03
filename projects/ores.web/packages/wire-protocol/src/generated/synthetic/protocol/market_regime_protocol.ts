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
 * @brief Ask what the market-wide arrival factor is doing now.
 */
export interface GetMarketRegimeRequest {}

/**
 * @brief One reading of the endogenous half of the factor: the shared heat.
 */
export interface MarketRegimeSample {
    /** @brief When the service took the reading. */
    at: string;
    /** @brief The shared heat then, in units of a rate: what any event adds and decay fades. */
    heat: number;
    /** @brief The ticks a second every running feed's law asked for then: the market's lambda(t).
     */
    intensity: number;
}

/**
 * @brief The factor in force: its state, its multiplier and whether a scenario pinned it.
 *
 * The heat is the endogenous half, and it is the internal state jump and decay
 * act on: a screen that wants to show the process rather than its outcome reads
 * these readings, which the service takes a few times a second as it fades the
 * heat. The jump and the decay are carried beside them, so a reader can say what
 * the curve means without knowing the configuration.
 */
export interface GetMarketRegimeResponse {
    success: boolean;
    message: string;
    /** @brief "calm" or "active". */
    state: string;
    /** @brief The multiplier every series is scaled by now. */
    multiplier: number;
    /** @brief Whether a scenario pinned the state rather than the chain choosing it. */
    pinned: boolean;
    /** @brief The heat in force now. */
    heat: number;
    /** @brief The heat one event adds, in units of a rate. */
    jump: number;
    /** @brief How fast the heat fades, in units of one over a second. */
    decay: number;
    /** @brief The heat's readings, oldest first, over the window the service keeps. */
    samples: MarketRegimeSample[];
    /** @brief How fast market time runs now: the multiplier plus the heat's share. */
    pace: number;
    /** @brief The market's operational time since the run started. */
    tau: number;
    /** @brief The summed mean rate of the running series, a second: what the heat is shared over.
     */
    population_rate: number;
    /** @brief The heat's loop gain, jump over decay; the heat settles while it is below one. */
    loop_gain: number;
    /** @brief The seed every random stream of the run is keyed by. */
    run_seed: number;
    /** @brief The simulated seconds one unit of market time spans; zero when not reported. */
    speed: number;
    /** @brief How often a calm market turns active, per second; zero when not reported. */
    wake_rate: number;
    /** @brief How often an active market turns calm, per second; zero when not reported. */
    calm_rate: number;
    /** @brief How far activity tilts a price's shape toward its tails; zero is off. */
    tail_loading: number;
    /** @brief The pace of an active market; the calm pace follows from it. Zero when not reported.
     */
    active_multiplier: number;
}

/**
 * @brief Reads the scheduler's recent decisions and its own numbers.
 */
export interface GetMarketTraceRequest {
    /** @brief One feed's events and the market's, or every event when empty. */
    source_name: string;
    /** @brief How far back to read, in seconds of the run. */
    seconds: number;
    /** @brief The most events to return; the newest are kept. */
    limit: number;
}

/** @brief One decision of the scheduler's loop. */
export interface MarketTraceEvent {
    /** @brief "tick", "transition", "pin", "unpin", "add", "remove" or "configure". */
    kind: string;
    /** @brief The feed it concerns, or empty for a market event. */
    source_name: string;
    at: string;
    /** @brief The market's operational time then. */
    tau: number;
    /** @brief For a tick: the gap the feed's law drew, in its own operational time. */
    operational_gap: number;
    /** @brief For a tick: the wall seconds since the feed last ticked. */
    wall_gap: number;
    /** @brief For a tick: the law's intensity integrated over the gap; Exp(1) when the feed
     * follows its law. */
    compensated_gap: number;
    heat: number;
    /** @brief The regime's multiplier; the pace less it is the heat's share. */
    multiplier: number;
    /** @brief How fast market time ran after the decision. */
    pace: number;
    /** @brief "calm" or "active", after the decision. */
    state: string;
}

/** @brief One running feed and the state of its arrival law now. */
export interface MarketSeriesState {
    source_name: string;
    /** @brief "periodic", "poisson" or "hawkes". */
    mode: string;
    /** @brief The configured mean rate, a second. */
    rate: number;
    sensitivity: number;
    /** @brief The rate it ticks at now: its law's intensity, its sensitivity and the pace. */
    effective_rate: number;
    /** @brief For a periodic law: the fraction of its first gap it started at. */
    phase: number;
    /** @brief For a Hawkes law: the excess intensity now, above the baseline. */
    excess: number;
    /** @brief For a Hawkes law: the branching ratio; zero for the other laws. */
    branching_ratio: number;
    /** @brief For a Hawkes law: the decay, per unit of operational time; zero otherwise. */
    decay: number;
}

export interface GetMarketTraceResponse {
    success: boolean;
    message: string;
    run_seed: number;
    /** @brief Ticks a second, smoothed over about a second. */
    ticks_per_second: number;
    /** @brief Entries in the scheduler's heap. */
    heap_size: number;
    /** @brief How late the loop fires a tick, smoothed, in seconds. */
    lateness_seconds: number;
    /** @brief The latest tick since the last read, in seconds. */
    max_lateness_seconds: number;
    /** @brief The feeds the scheduler runs. */
    series: number;
    /** @brief The decisions in the window, oldest first. */
    events: MarketTraceEvent[];
    /** @brief Every running feed and its law now, in name order. */
    series_states: MarketSeriesState[];
}

/**
 * @brief Reads the ticks of the last stretch of the run, slot by slot and asset class by
 * asset class: an hour by the second, or ten minutes by the tenth of a second.
 */
export interface GetMarketHistoryRequest {
    /** @brief "second" or "tenth". */
    resolution: string;
    /** @brief How far back to read, in seconds of the run. */
    seconds: number;
}

/** @brief One slot of the history. */
export interface MarketHistoryBin {
    at: string;
    /** @brief The ticks of each group in the slot, by the group's index. */
    counts: number[];
    /** @brief The mean pace over the slot; zero when the loop took no reading in it. */
    pace: number;
    /** @brief The share of the slot the market was active. */
    active_share: number;
}

export interface GetMarketHistoryResponse {
    success: boolean;
    message: string;
    /** @brief The length of a slot, in seconds. */
    slot_seconds: number;
    /** @brief The groups the counts are by: the feeds' asset classes. */
    groups: string[];
    /** @brief The slots with a tick or a reading in them, oldest first. */
    bins: MarketHistoryBin[];
}

/**
 * @brief Pin the market state, or hand it back to the chain.
 */
export interface PinMarketRegimeRequest {
    /**
     * @brief What to force: "auto" hands control back to the chain, "calm" and
     * "active" pin that state.
     */
    mode: string;
}

/**
 * @brief The state in force after the request.
 */
export interface PinMarketRegimeResponse {
    success: boolean;
    message: string;
    /** @brief The state in force now, as the read reports it. */
    state: string;
    /** @brief The multiplier in force now. */
    multiplier: number;
    /** @brief Whether the state is pinned after this request. */
    pinned: boolean;
    /** @brief The rest of the regime after the request, as the read reports it. */
    heat: number;
    jump: number;
    decay: number;
    samples: MarketRegimeSample[];
    pace: number;
    tau: number;
    population_rate: number;
    loop_gain: number;
    run_seed: number;
    speed: number;
    wake_rate: number;
    calm_rate: number;
    tail_loading: number;
    active_multiplier: number;
}

/**
 * @brief Set how the market moves, from now on: the chain's two rates, the heat's
 * jump and decay, the market speed, and the tail loading.
 *
 * Numbers the market cannot run on are refused with success false and a message that
 * names the number. The reply is the regime after the request, as a pin replies.
 */
export interface ConfigureMarketRequest {
    /** @brief How often a calm market turns active, per second. */
    wake_rate: number;
    /** @brief How often an active market turns calm, per second. */
    calm_rate: number;
    /** @brief The heat one event adds, in units of a rate. */
    jump: number;
    /** @brief How fast the heat fades, in units of one over a second. */
    decay: number;
    /** @brief The simulated seconds one unit of market time spans. */
    speed: number;
    /** @brief How far activity tilts a price's shape toward its tails; zero is off. */
    tail_loading: number;
}

export const subjects = {
    get_market_regime_request: 'synthetic.v1.ops.get_market_regime',
    get_market_trace_request: 'synthetic.v1.ops.get_market_trace',
    get_market_history_request: 'synthetic.v1.ops.get_market_history',
    pin_market_regime_request: 'synthetic.v1.ops.pin_market_regime',
    configure_market_request: 'synthetic.v1.ops.configure_market',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_market_regime_request: true,
    get_market_trace_request: true,
    get_market_history_request: true,
    pin_market_regime_request: true,
    configure_market_request: true,
} as const;
