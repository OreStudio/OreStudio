/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_SYNTHETIC_API_MESSAGING_MARKET_REGIME_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_MARKET_REGIME_PROTOCOL_HPP

#include <chrono>
#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace ores::synthetic::messaging {

/**
 * @brief Ask what the market-wide arrival factor is doing now.
 */
struct get_market_regime_request {
    using response_type = struct get_market_regime_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.get_market_regime";
    /// @brief Whether the caller's session is required: it is, so the read is attributable.
    static constexpr bool requires_session = true;
};

/**
 * @brief One reading of the endogenous half of the factor: the shared heat.
 */
struct market_regime_sample {
    /** @brief When the service took the reading. */
    std::chrono::system_clock::time_point at;
    /** @brief The shared heat then, in units of a rate: what any event adds and decay fades. */
    double heat = 0.0;
};

/**
 * @brief The factor in force: its state, its multiplier and whether a scenario pinned it.
 *
 * The heat is the endogenous half, and it is the internal state jump and decay
 * act on: a screen that wants to show the process rather than its outcome reads
 * these readings, which the service takes a few times a second as it fades the
 * heat. The jump and the decay are carried beside them, so a reader can say what
 * the curve means without knowing the configuration.
 */
struct get_market_regime_response {
    bool success = false;
    std::string message;
    /** @brief "calm" or "active". */
    std::string state;
    /** @brief The multiplier every series is scaled by now. */
    double multiplier = 1.0;
    /** @brief Whether a scenario pinned the state rather than the chain choosing it. */
    bool pinned = false;
    /** @brief The heat in force now. */
    double heat = 0.0;
    /** @brief The heat one event adds, in units of a rate. */
    double jump = 0.0;
    /** @brief How fast the heat fades, in units of one over a second. */
    double decay = 0.0;
    /** @brief The heat's readings, oldest first, over the window the service keeps. */
    std::vector<market_regime_sample> samples;
    /** @brief How fast market time runs now: the multiplier plus the heat's share. */
    double pace = 1.0;
    /** @brief The market's operational time since the run started. */
    double tau = 0.0;
    /** @brief The summed mean rate of the running series, a second: what the heat is shared over.
     */
    double population_rate = 0.0;
    /** @brief The heat's loop gain, jump over decay; the heat settles while it is below one. */
    double loop_gain = 0.0;
    /** @brief The seed every random stream of the run is keyed by. */
    std::uint64_t run_seed = 0;
    /** @brief The simulated seconds one unit of market time spans. */
    double speed = 1.0;
};

/**
 * @brief Pin the market state, or hand it back to the chain.
 */
/**
 * @brief Reads the scheduler's recent decisions and its own numbers.
 */
struct get_market_trace_request {
    using response_type = struct get_market_trace_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.get_market_trace";
    static constexpr bool requires_session = true;
    /** @brief One feed's events and the market's, or every event when empty. */
    std::string source_name;
    /** @brief How far back to read, in seconds of the run. */
    double seconds = 120.0;
    /** @brief The most events to return; the newest are kept. */
    std::uint32_t limit = 5000;
};

/** @brief One decision of the scheduler's loop. */
struct market_trace_event {
    /** @brief "tick", "transition", "pin", "unpin", "add" or "remove". */
    std::string kind;
    /** @brief The feed it concerns, or empty for a market event. */
    std::string source_name;
    std::chrono::system_clock::time_point at;
    /** @brief The market's operational time then. */
    double tau = 0.0;
    /** @brief For a tick: the gap the feed's law drew, in its own operational time. */
    double operational_gap = 0.0;
    /** @brief For a tick: the wall seconds since the feed last ticked. */
    double wall_gap = 0.0;
    /** @brief For a tick: the law's intensity integrated over the gap; Exp(1) when the feed
     * follows its law. */
    double compensated_gap = 0.0;
    double heat = 0.0;
    /** @brief The regime's multiplier; the pace less it is the heat's share. */
    double multiplier = 1.0;
    /** @brief How fast market time ran after the decision. */
    double pace = 1.0;
    /** @brief "calm" or "active", after the decision. */
    std::string state;
};

/** @brief One running feed and the state of its arrival law now. */
struct market_series_state {
    std::string source_name;
    /** @brief "periodic", "poisson" or "hawkes". */
    std::string mode;
    /** @brief The configured mean rate, a second. */
    double rate = 0.0;
    double sensitivity = 1.0;
    /** @brief The rate it ticks at now: its law's intensity, its sensitivity and the pace. */
    double effective_rate = 0.0;
    /** @brief For a periodic law: the fraction of its first gap it started at. */
    double phase = 0.0;
    /** @brief For a Hawkes law: the excess intensity now, above the baseline. */
    double excess = 0.0;
};

struct get_market_trace_response {
    bool success = false;
    std::string message;
    std::uint64_t run_seed = 0;
    /** @brief Ticks a second, smoothed over about a second. */
    double ticks_per_second = 0.0;
    /** @brief Entries in the scheduler's heap. */
    std::uint64_t heap_size = 0;
    /** @brief How late the loop fires a tick, smoothed, in seconds. */
    double lateness_seconds = 0.0;
    /** @brief The latest tick since the last read, in seconds. */
    double max_lateness_seconds = 0.0;
    /** @brief The feeds the scheduler runs. */
    std::uint64_t series = 0;
    /** @brief The decisions in the window, oldest first. */
    std::vector<market_trace_event> events;
    /** @brief Every running feed and its law now, in name order. */
    std::vector<market_series_state> series_states;
};

struct pin_market_regime_request {
    using response_type = struct pin_market_regime_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.pin_market_regime";
    static constexpr bool requires_session = true;
    /**
     * @brief What to force: "auto" hands control back to the chain, "calm" and
     * "active" pin that state.
     */
    std::string mode;
};

/**
 * @brief The state in force after the request.
 */
struct pin_market_regime_response {
    bool success = false;
    std::string message;
    /** @brief The state in force now, as the read reports it. */
    std::string state;
    /** @brief The multiplier in force now. */
    double multiplier = 1.0;
    /** @brief Whether the state is pinned after this request. */
    bool pinned = false;
    /** @brief The rest of the regime after the request, as the read reports it. */
    double heat = 0.0;
    double jump = 0.0;
    double decay = 0.0;
    std::vector<market_regime_sample> samples;
    double pace = 1.0;
    double tau = 0.0;
    double population_rate = 0.0;
    double loop_gain = 0.0;
    std::uint64_t run_seed = 0;
    double speed = 1.0;
};

}

#endif
