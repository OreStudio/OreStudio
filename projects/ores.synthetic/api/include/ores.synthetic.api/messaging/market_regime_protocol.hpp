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
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_SYNTHETIC_API_MESSAGING_MARKET_REGIME_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_MARKET_REGIME_PROTOCOL_HPP

#include "ores.synthetic.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::synthetic::messaging {

/**
 * @brief Ask what the market-wide arrival factor is doing now.
 */
struct get_market_regime_request {
    using response_type = struct get_market_regime_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.get_market_regime";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const get_market_regime_request&,
                           const get_market_regime_request&) = default;
};

/**
 * @brief One reading of the endogenous half of the factor: the shared heat.
 */
struct market_regime_sample {
    /** @brief When the service took the reading. */
    std::chrono::system_clock::time_point at;
    /** @brief The shared heat then, in units of a rate: what any event adds and decay fades. */
    double heat = 0.0;
    /** @brief The ticks a second every running feed's law asked for then: the market's lambda(t).
     */
    double intensity = 0.0;

    friend bool operator==(const market_regime_sample&, const market_regime_sample&) = default;
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
    /** @brief The simulated seconds one unit of market time spans; zero when not reported. */
    double speed = 0.0;
    /** @brief How often a calm market turns active, per second; zero when not reported. */
    double wake_rate = 0.0;
    /** @brief How often an active market turns calm, per second; zero when not reported. */
    double calm_rate = 0.0;
    /** @brief How far activity tilts a price's shape toward its tails; zero is off. */
    double tail_loading = 0.0;
    /** @brief The pace of an active market; the calm pace follows from it. Zero when not reported.
     */
    double active_multiplier = 0.0;

    friend bool operator==(const get_market_regime_response&,
                           const get_market_regime_response&) = default;
};

/**
 * @brief Reads the scheduler's recent decisions and its own numbers.
 */
struct get_market_trace_request {
    using response_type = struct get_market_trace_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.get_market_trace";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief One feed's events and the market's, or every event when empty. */
    std::string source_name;
    /** @brief How far back to read, in seconds of the run. */
    double seconds = 120.0;
    /** @brief The most events to return; the newest are kept. */
    std::uint32_t limit = 5000;

    friend bool operator==(const get_market_trace_request&,
                           const get_market_trace_request&) = default;
};

/** @brief One decision of the scheduler's loop. */
struct market_trace_event {
    /** @brief "tick", "transition", "pin", "unpin", "add", "remove" or "configure". */
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

    friend bool operator==(const market_trace_event&, const market_trace_event&) = default;
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
    /** @brief For a Hawkes law: the branching ratio; zero for the other laws. */
    double branching_ratio = 0.0;
    /** @brief For a Hawkes law: the decay, per unit of operational time; zero otherwise. */
    double decay = 0.0;

    friend bool operator==(const market_series_state&, const market_series_state&) = default;
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

    friend bool operator==(const get_market_trace_response&,
                           const get_market_trace_response&) = default;
};

/**
 * @brief Reads the ticks of the last stretch of the run, slot by slot and asset class by
 * asset class: an hour by the second, or ten minutes by the tenth of a second.
 */
struct get_market_history_request {
    using response_type = struct get_market_history_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.get_market_history";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief "second" or "tenth". */
    std::string resolution = "second";
    /** @brief How far back to read, in seconds of the run. */
    double seconds = 3600.0;

    friend bool operator==(const get_market_history_request&,
                           const get_market_history_request&) = default;
};

/** @brief One slot of the history. */
struct market_history_bin {
    std::chrono::system_clock::time_point at;
    /** @brief The ticks of each group in the slot, by the group's index. */
    std::vector<std::uint32_t> counts;
    /** @brief The mean pace over the slot; zero when the loop took no reading in it. */
    double pace = 0.0;
    /** @brief The share of the slot the market was active. */
    double active_share = 0.0;

    friend bool operator==(const market_history_bin&, const market_history_bin&) = default;
};

struct get_market_history_response {
    bool success = false;
    std::string message;
    /** @brief The length of a slot, in seconds. */
    double slot_seconds = 0.0;
    /** @brief The groups the counts are by: the feeds' asset classes. */
    std::vector<std::string> groups;
    /** @brief The slots with a tick or a reading in them, oldest first. */
    std::vector<market_history_bin> bins;

    friend bool operator==(const get_market_history_response&,
                           const get_market_history_response&) = default;
};

/**
 * @brief Pin the market state, or hand it back to the chain.
 */
struct pin_market_regime_request {
    using response_type = struct pin_market_regime_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.pin_market_regime";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief What to force: "auto" hands control back to the chain, "calm" and
     * "active" pin that state.
     */
    std::string mode;

    friend bool operator==(const pin_market_regime_request&,
                           const pin_market_regime_request&) = default;
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
    double speed = 0.0;
    double wake_rate = 0.0;
    double calm_rate = 0.0;
    double tail_loading = 0.0;
    double active_multiplier = 0.0;

    friend bool operator==(const pin_market_regime_response&,
                           const pin_market_regime_response&) = default;
};

/**
 * @brief Set how the market moves, from now on: the chain's two rates, the heat's
 * jump and decay, the market speed, and the tail loading.
 *
 * Numbers the market cannot run on are refused with success false and a message that
 * names the number. The reply is the regime after the request, as a pin replies.
 */
struct configure_market_request {
    using response_type = struct pin_market_regime_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.configure_market";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief How often a calm market turns active, per second. */
    double wake_rate = 0.0;
    /** @brief How often an active market turns calm, per second. */
    double calm_rate = 0.0;
    /** @brief The heat one event adds, in units of a rate. */
    double jump = 0.0;
    /** @brief How fast the heat fades, in units of one over a second. */
    double decay = 0.0;
    /** @brief The simulated seconds one unit of market time spans. */
    double speed = 0.0;
    /** @brief How far activity tilts a price's shape toward its tails; zero is off. */
    double tail_loading = 0.0;

    friend bool operator==(const configure_market_request&,
                           const configure_market_request&) = default;
};

/**
 * @brief The members of get_market_regime_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_market_regime_request>) {
    return ::ores::utility::codec::field_list{};
}

/**
 * @brief The members of market_regime_sample, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_regime_sample>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"at", &market_regime_sample::at},
        ::ores::utility::codec::field{"heat", &market_regime_sample::heat},
        ::ores::utility::codec::field{"intensity", &market_regime_sample::intensity},
    };
}

/**
 * @brief The members of get_market_regime_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_market_regime_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &get_market_regime_response::success},
        ::ores::utility::codec::field{"message", &get_market_regime_response::message},
        ::ores::utility::codec::field{"state", &get_market_regime_response::state},
        ::ores::utility::codec::field{"multiplier", &get_market_regime_response::multiplier},
        ::ores::utility::codec::field{"pinned", &get_market_regime_response::pinned},
        ::ores::utility::codec::field{"heat", &get_market_regime_response::heat},
        ::ores::utility::codec::field{"jump", &get_market_regime_response::jump},
        ::ores::utility::codec::field{"decay", &get_market_regime_response::decay},
        ::ores::utility::codec::field{"samples", &get_market_regime_response::samples},
        ::ores::utility::codec::field{"pace", &get_market_regime_response::pace},
        ::ores::utility::codec::field{"tau", &get_market_regime_response::tau},
        ::ores::utility::codec::field{"population_rate",
                                      &get_market_regime_response::population_rate},
        ::ores::utility::codec::field{"loop_gain", &get_market_regime_response::loop_gain},
        ::ores::utility::codec::field{"run_seed", &get_market_regime_response::run_seed},
        ::ores::utility::codec::field{"speed", &get_market_regime_response::speed},
        ::ores::utility::codec::field{"wake_rate", &get_market_regime_response::wake_rate},
        ::ores::utility::codec::field{"calm_rate", &get_market_regime_response::calm_rate},
        ::ores::utility::codec::field{"tail_loading", &get_market_regime_response::tail_loading},
        ::ores::utility::codec::field{"active_multiplier",
                                      &get_market_regime_response::active_multiplier},
    };
}

/**
 * @brief The members of get_market_trace_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_market_trace_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"source_name", &get_market_trace_request::source_name},
        ::ores::utility::codec::field{"seconds", &get_market_trace_request::seconds},
        ::ores::utility::codec::field{"limit", &get_market_trace_request::limit},
    };
}

/**
 * @brief The members of market_trace_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_trace_event>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"kind", &market_trace_event::kind},
        ::ores::utility::codec::field{"source_name", &market_trace_event::source_name},
        ::ores::utility::codec::field{"at", &market_trace_event::at},
        ::ores::utility::codec::field{"tau", &market_trace_event::tau},
        ::ores::utility::codec::field{"operational_gap", &market_trace_event::operational_gap},
        ::ores::utility::codec::field{"wall_gap", &market_trace_event::wall_gap},
        ::ores::utility::codec::field{"compensated_gap", &market_trace_event::compensated_gap},
        ::ores::utility::codec::field{"heat", &market_trace_event::heat},
        ::ores::utility::codec::field{"multiplier", &market_trace_event::multiplier},
        ::ores::utility::codec::field{"pace", &market_trace_event::pace},
        ::ores::utility::codec::field{"state", &market_trace_event::state},
    };
}

/**
 * @brief The members of market_series_state, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_series_state>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"source_name", &market_series_state::source_name},
        ::ores::utility::codec::field{"mode", &market_series_state::mode},
        ::ores::utility::codec::field{"rate", &market_series_state::rate},
        ::ores::utility::codec::field{"sensitivity", &market_series_state::sensitivity},
        ::ores::utility::codec::field{"effective_rate", &market_series_state::effective_rate},
        ::ores::utility::codec::field{"phase", &market_series_state::phase},
        ::ores::utility::codec::field{"excess", &market_series_state::excess},
        ::ores::utility::codec::field{"branching_ratio", &market_series_state::branching_ratio},
        ::ores::utility::codec::field{"decay", &market_series_state::decay},
    };
}

/**
 * @brief The members of get_market_trace_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_market_trace_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &get_market_trace_response::success},
        ::ores::utility::codec::field{"message", &get_market_trace_response::message},
        ::ores::utility::codec::field{"run_seed", &get_market_trace_response::run_seed},
        ::ores::utility::codec::field{"ticks_per_second",
                                      &get_market_trace_response::ticks_per_second},
        ::ores::utility::codec::field{"heap_size", &get_market_trace_response::heap_size},
        ::ores::utility::codec::field{"lateness_seconds",
                                      &get_market_trace_response::lateness_seconds},
        ::ores::utility::codec::field{"max_lateness_seconds",
                                      &get_market_trace_response::max_lateness_seconds},
        ::ores::utility::codec::field{"series", &get_market_trace_response::series},
        ::ores::utility::codec::field{"events", &get_market_trace_response::events},
        ::ores::utility::codec::field{"series_states", &get_market_trace_response::series_states},
    };
}

/**
 * @brief The members of get_market_history_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_market_history_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"resolution", &get_market_history_request::resolution},
        ::ores::utility::codec::field{"seconds", &get_market_history_request::seconds},
    };
}

/**
 * @brief The members of market_history_bin, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_history_bin>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"at", &market_history_bin::at},
        ::ores::utility::codec::field{"counts", &market_history_bin::counts},
        ::ores::utility::codec::field{"pace", &market_history_bin::pace},
        ::ores::utility::codec::field{"active_share", &market_history_bin::active_share},
    };
}

/**
 * @brief The members of get_market_history_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_market_history_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &get_market_history_response::success},
        ::ores::utility::codec::field{"message", &get_market_history_response::message},
        ::ores::utility::codec::field{"slot_seconds", &get_market_history_response::slot_seconds},
        ::ores::utility::codec::field{"groups", &get_market_history_response::groups},
        ::ores::utility::codec::field{"bins", &get_market_history_response::bins},
    };
}

/**
 * @brief The members of pin_market_regime_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<pin_market_regime_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"mode", &pin_market_regime_request::mode},
    };
}

/**
 * @brief The members of pin_market_regime_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<pin_market_regime_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &pin_market_regime_response::success},
        ::ores::utility::codec::field{"message", &pin_market_regime_response::message},
        ::ores::utility::codec::field{"state", &pin_market_regime_response::state},
        ::ores::utility::codec::field{"multiplier", &pin_market_regime_response::multiplier},
        ::ores::utility::codec::field{"pinned", &pin_market_regime_response::pinned},
        ::ores::utility::codec::field{"heat", &pin_market_regime_response::heat},
        ::ores::utility::codec::field{"jump", &pin_market_regime_response::jump},
        ::ores::utility::codec::field{"decay", &pin_market_regime_response::decay},
        ::ores::utility::codec::field{"samples", &pin_market_regime_response::samples},
        ::ores::utility::codec::field{"pace", &pin_market_regime_response::pace},
        ::ores::utility::codec::field{"tau", &pin_market_regime_response::tau},
        ::ores::utility::codec::field{"population_rate",
                                      &pin_market_regime_response::population_rate},
        ::ores::utility::codec::field{"loop_gain", &pin_market_regime_response::loop_gain},
        ::ores::utility::codec::field{"run_seed", &pin_market_regime_response::run_seed},
        ::ores::utility::codec::field{"speed", &pin_market_regime_response::speed},
        ::ores::utility::codec::field{"wake_rate", &pin_market_regime_response::wake_rate},
        ::ores::utility::codec::field{"calm_rate", &pin_market_regime_response::calm_rate},
        ::ores::utility::codec::field{"tail_loading", &pin_market_regime_response::tail_loading},
        ::ores::utility::codec::field{"active_multiplier",
                                      &pin_market_regime_response::active_multiplier},
    };
}

/**
 * @brief The members of configure_market_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<configure_market_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"wake_rate", &configure_market_request::wake_rate},
        ::ores::utility::codec::field{"calm_rate", &configure_market_request::calm_rate},
        ::ores::utility::codec::field{"jump", &configure_market_request::jump},
        ::ores::utility::codec::field{"decay", &configure_market_request::decay},
        ::ores::utility::codec::field{"speed", &configure_market_request::speed},
        ::ores::utility::codec::field{"tail_loading", &configure_market_request::tail_loading},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_market_regime_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::get_market_regime_request& v);
    static ores::synthetic::messaging::get_market_regime_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_market_regime_request& v);
    static ores::synthetic::messaging::get_market_regime_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::market_regime_sample> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::market_regime_sample& v);
    static ores::synthetic::messaging::market_regime_sample read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::market_regime_sample& v);
    static ores::synthetic::messaging::market_regime_sample
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_market_regime_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::get_market_regime_response& v);
    static ores::synthetic::messaging::get_market_regime_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_market_regime_response& v);
    static ores::synthetic::messaging::get_market_regime_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::get_market_trace_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::get_market_trace_request& v);
    static ores::synthetic::messaging::get_market_trace_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::get_market_trace_request& v);
    static ores::synthetic::messaging::get_market_trace_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::market_trace_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::market_trace_event& v);
    static ores::synthetic::messaging::market_trace_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::market_trace_event& v);
    static ores::synthetic::messaging::market_trace_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::market_series_state> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::market_series_state& v);
    static ores::synthetic::messaging::market_series_state read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::market_series_state& v);
    static ores::synthetic::messaging::market_series_state
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_market_trace_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::get_market_trace_response& v);
    static ores::synthetic::messaging::get_market_trace_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_market_trace_response& v);
    static ores::synthetic::messaging::get_market_trace_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_market_history_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::get_market_history_request& v);
    static ores::synthetic::messaging::get_market_history_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_market_history_request& v);
    static ores::synthetic::messaging::get_market_history_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::market_history_bin> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::market_history_bin& v);
    static ores::synthetic::messaging::market_history_bin read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::market_history_bin& v);
    static ores::synthetic::messaging::market_history_bin
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_market_history_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::get_market_history_response& v);
    static ores::synthetic::messaging::get_market_history_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_market_history_response& v);
    static ores::synthetic::messaging::get_market_history_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::pin_market_regime_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::pin_market_regime_request& v);
    static ores::synthetic::messaging::pin_market_regime_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::pin_market_regime_request& v);
    static ores::synthetic::messaging::pin_market_regime_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::pin_market_regime_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::pin_market_regime_response& v);
    static ores::synthetic::messaging::pin_market_regime_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::pin_market_regime_response& v);
    static ores::synthetic::messaging::pin_market_regime_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::configure_market_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::configure_market_request& v);
    static ores::synthetic::messaging::configure_market_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::configure_market_request& v);
    static ores::synthetic::messaging::configure_market_request
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
