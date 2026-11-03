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
#ifndef ORES_SYNTHETIC_API_MESSAGING_FEED_CONFIG_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_FEED_CONFIG_PROTOCOL_HPP

#include "ores.synthetic.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::synthetic::messaging {

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
struct start_feed_request {
    using response_type = struct start_feed_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.start_feed";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string config_id;

    friend bool operator==(const start_feed_request&, const start_feed_request&) = default;
};

/**
 * @brief Whether the feed started, and why not when it did not.
 */
struct start_feed_response {
    bool success = false;
    std::string message;

    friend bool operator==(const start_feed_response&, const start_feed_response&) = default;
};

/**
 * @brief Request to stop running feed(s), identified by config_id or source_name.
 *
 * config_id is preferred: it is resolved server-side to the config's
 * source_name, the same way start resolves it -- the client never needs to
 * know the "synthetic.<pair>" / "ir_curve.<ccy>.<idx>" naming conventions. If
 * both config_id and source_name are empty, stops all running feeds of every
 * kind.
 */
struct stop_feed_request {
    using response_type = struct stop_feed_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.stop_feed";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief Preferred: resolved server-side to source_name. */
    std::string config_id;
    /** @brief Used only if config_id is empty; empty too stops every feed. */
    std::string source_name;

    friend bool operator==(const stop_feed_request&, const stop_feed_request&) = default;
};

/**
 * @brief Whether the feed stopped, and why not when it did not.
 */
struct stop_feed_response {
    bool success = false;
    std::string message;

    friend bool operator==(const stop_feed_response&, const stop_feed_response&) = default;
};

/**
 * @brief How far another provider's fixing of the pair stands from the level a feed took.
 */
struct provider_dispersion {
    /** @brief The other provider. */
    std::string provider_code;
    /** @brief The other provider's item, as the oresmd URI that names it. */
    std::string item;
    /** @brief The distance from the level the feed took, in basis points of that level. */
    double basis_points = 0.0;

    friend bool operator==(const provider_dispersion&, const provider_dispersion&) = default;
};

/**
 * @brief How a feed's daily variance started: the model, and whether history fitted it.
 */
struct variance_start {
    /** @brief "constant" or "gjr_garch". */
    std::string model;
    /** @brief The approved variance calibration the run took its shape from. */
    std::string calibration_id;
    /** @brief The version of that calibration the run read. */
    int calibration_version = 0;
    /** @brief True when the calibration was fitted from fixings; false when it is an authored
     * shape. */
    bool fitted = false;
    /** @brief The reaction to a day's move. */
    double alpha = 0.0;
    /** @brief The extra reaction to a fall of the pair. */
    double gamma = 0.0;
    /** @brief The carry-over of a day's variance. */
    double beta = 0.0;
    /** @brief alpha + beta + gamma times the mixture's p_minus, below one. */
    double persistence = 0.0;
    /** @brief The first day's variance over the long-run level: above one when history ended
     * turbulent. */
    double initial_ratio = 1.0;
    /** @brief How many daily returns the fit read; zero when nothing was fitted. */
    int returns = 0;
    /** @brief Why the authored shape was kept, when it was. */
    std::string note;

    friend bool operator==(const variance_start&, const variance_start&) = default;
};

/**
 * @brief Where a feed's starting value came from, or that it kept its fixed level.
 *
 * The record belongs to the run, not to the config: a feed that stops and
 * starts again records where the new run started.
 */
struct feed_start {
    /** @brief True when a fixing set the level; false when the feed kept its fixed level. */
    bool from_fixing = false;
    /** @brief The fixing's item, as the oresmd URI that names it. */
    std::string item;
    /** @brief The provider that published the fixing. */
    std::string provider_code;
    /** @brief The provider's own id of the item, or empty when no mapping names it. */
    std::string vendor_id;
    /** @brief The fixing's value as the publisher wrote it, in decimal text. */
    std::string value;
    /** @brief True when the item quotes the feed's pair the other way round. */
    bool inverted = false;
    /** @brief The date of the fixing, which is the publisher's own time for a daily fixing. */
    std::optional<std::chrono::year_month_day> fixing_date;
    /** @brief When the service stored the fixing. */
    std::optional<std::chrono::system_clock::time_point> received_at;
    /** @brief How many calendar days of fixings the window covered. */
    int window_days = 0;
    /** @brief How many fixings of the window measured the spread. */
    int used = 0;
    /** @brief The measured hourly spread, when the window had one. */
    std::optional<double> hourly_spread;
    /** @brief Why the feed kept its fixed level, or why its spread was not measured. */
    std::string note;
    /** @brief How many days the fixing is older than the date the feed read on or before. */
    std::optional<int> age_days;
    /** @brief The spread of the window's business-day log returns, in the feed's orientation. */
    std::optional<double> daily_spread;
    /** @brief The mean business-day log return, times 252. */
    std::optional<double> annual_drift;
    /** @brief The smallest business-day log return in the window. */
    std::optional<double> min_return;
    /** @brief The largest business-day log return in the window. */
    std::optional<double> max_return;
    /** @brief How many returns lie more than three spreads from zero. */
    std::optional<int> jumps;
    /** @brief The other providers' fixings of the pair, and how far each stands from the level. */
    std::vector<provider_dispersion> dispersion;
    /** @brief How the daily variance started, for a feed whose variance is modelled. */
    std::optional<variance_start> variance;

    friend bool operator==(const feed_start&, const feed_start&) = default;
};

/**
 * @brief One feed the service runs now, and where its starting value came from.
 *
 * A feed kind that starts from no published value carries no start, so it reads
 * as running and says nothing of where it started.
 */
struct running_feed {
    /** @brief The source name the run publishes under. */
    std::string source_name;
    /** @brief Where the run's starting value came from; absent when the kind has no such start. */
    std::optional<feed_start> start;

    friend bool operator==(const running_feed&, const running_feed&) = default;
};

/**
 * @brief Request the feeds that run now, every kind, with where each started.
 */
struct list_feeds_request {
    using response_type = struct list_feeds_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.feed_configs.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const list_feeds_request&, const list_feeds_request&) = default;
};

/**
 * @brief The feeds the service runs now, each with where it started, and whether
 * the read succeeded.
 */
struct list_feeds_response {
    bool success = false;
    /** @brief The feeds that run, in source name order. */
    std::vector<running_feed> running_feeds;

    friend bool operator==(const list_feeds_response&, const list_feeds_response&) = default;
};

/**
 * @brief The policy a class calibrates under: its own row, or the defaults when it has none.
 */
struct calibration_policy_summary {
    /** @brief True when the class has a policy row; false when it calibrates on the defaults. */
    bool from_policy_row = false;
    /** @brief How many calendar days of fixings measure the spread. */
    int window_days = 0;
    /** @brief How many calendar days of fixings a variance calibration's input covers. */
    int variance_window_days = 0;
    /** @brief True when an item that quotes the pair the other way round is read as one over it. */
    bool allow_inverse = false;
    /** @brief What a feed does when no candidate has a fixing: keep_fixed or refuse. */
    std::string fallback_code;
    /** @brief The accepted providers, first preferred. Empty means every provider, by code. */
    std::vector<std::string> provider_order;

    friend bool operator==(const calibration_policy_summary&,
                           const calibration_policy_summary&) = default;
};

/**
 * @brief One candidate of a feed's resolution, in rank order, and why it was or was not taken.
 */
struct candidate_outcome {
    /** @brief The candidate's item, as the oresmd URI that names it. */
    std::string item;
    /** @brief The provider that publishes the item. */
    std::string provider_code;
    /** @brief True when the item quotes the pair the other way round. */
    bool inverted = false;
    /** @brief The candidate's place in the resolution, first tried first. */
    int rank = 0;
    /** @brief chosen, skipped or not_reached. */
    std::string state;
    /** @brief Why the candidate was chosen, skipped or not reached. */
    std::string reason;

    friend bool operator==(const candidate_outcome&, const candidate_outcome&) = default;
};

/**
 * @brief What a calibration run made of a feed's daily variance: the input it captured and the
 * calibration it fitted from it, stored when the run is applied.
 */
struct variance_calibration_summary {
    /** @brief The stored input; empty when the run was not applied. */
    std::string input_id;
    /** @brief The stored calibration; empty when the run was not applied. */
    std::string calibration_id;
    /** @brief "gjr_garch_qmle" for a fit, "authored" for an authored shape. */
    std::string method;
    /** @brief "proposed" when the fit was made, "rejected" when it could not be. */
    std::string state;
    /** @brief Why the fit was rejected; empty for a proposed one. */
    std::string state_reason;
    /** @brief The fixing item the input was read from. */
    std::string item;
    /** @brief The provider of that item. */
    std::string provider_code;
    /** @brief True when the item quotes the pair the other way round. */
    bool inverted = false;
    /** @brief The window the input covers, in calendar days, from the policy. */
    int window_days = 0;
    /** @brief The last date of the window, as YYYY-MM-DD. */
    std::string window_end;
    /** @brief The fixing dates the window held. */
    int point_count = 0;
    /** @brief The points whose return the fit used. */
    int used_count = 0;
    /** @brief The fitted reaction to a day's move; absent when rejected. */
    std::optional<double> alpha;
    /** @brief The fitted extra reaction to a fall; absent when rejected. */
    std::optional<double> gamma;
    /** @brief The fitted weight of yesterday's variance; absent when rejected. */
    std::optional<double> beta;
    /** @brief alpha + gamma p_minus + beta; absent when rejected. */
    std::optional<double> persistence;
    /** @brief The first day's variance over the long-run level; absent when rejected. */
    std::optional<double> starting_variance_ratio;
    /** @brief How many starts the fit ran from. */
    int starts = 0;
    /** @brief True when every start reached the same optimum. */
    bool starts_agreed = false;

    friend bool operator==(const variance_calibration_summary&,
                           const variance_calibration_summary&) = default;
};

/**
 * @brief How one feed of the class resolved: the item it starts from, or its fallback or
 * refusal, and the exceptions it states against the policy.
 */
struct calibrated_feed {
    /** @brief The feed's config id. */
    std::string config_id;
    /** @brief The source name the feed publishes under. */
    std::string source_name;
    /** @brief The base currency of the feed's pair. */
    std::string base_currency_code;
    /** @brief The quote currency of the feed's pair. */
    std::string quote_currency_code;
    /** @brief True when the feed names its own items instead of deriving them from the policy. */
    bool explicit_items = false;
    /** @brief The feed's own window, when it overrides the policy's. */
    std::optional<int> window_days_override;
    /** @brief True when no candidate has a fixing and the policy refuses to start the feed. */
    bool refused = false;
    /** @brief Where the feed starts: the item and its fixing, or why it keeps its fixed level. */
    feed_start start;
    /** @brief Every candidate the feed read, in rank order, with its outcome. */
    std::vector<candidate_outcome> candidates;
    /** @brief True when the feed runs now, so a stored calibration applies at its next start. */
    bool running = false;
    /**
     * @brief What an applied calibration did to the feed: updated, unchanged, kept, refused or
     * failed. Empty when the calibration was not applied.
     */
    std::string outcome;
    /** @brief The words of the outcome: the change written, or why nothing was written. */
    std::string outcome_message;
    /** @brief The variance calibration of a GJR-GARCH feed that starts from a fixing; absent
     * otherwise. */
    std::optional<variance_calibration_summary> variance;
    /** @brief The calibration the feed's config names and runs, as stored; absent when it names
     * none. */
    std::optional<variance_calibration_summary> current_variance;

    friend bool operator==(const calibrated_feed&, const calibrated_feed&) = default;
};

/**
 * @brief Resolve the feeds of one asset class under its policy, without starting any.
 *
 * Every feed of the class is resolved, or the one feed the request names. An applied
 * calibration writes the resolved level and deviations as a new version of each feed config
 * they change, under the reason system.calibration.
 */
struct calibrate_request {
    using response_type = struct calibrate_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.calibrate_feeds";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief The asset class to calibrate, as its refdata code. */
    std::string asset_class_code;
    /** @brief The one feed config to calibrate; empty calibrates every feed of the class. */
    std::string config_id;
    /** @brief True to write the calibration; false only reports what it would be. */
    bool apply = false;

    friend bool operator==(const calibrate_request&, const calibrate_request&) = default;
};

/**
 * @brief The policy the class calibrated under and the outcome of each of its feeds.
 */
struct calibrate_response {
    bool success = false;
    std::string message;
    /** @brief The policy the class calibrated under. */
    calibration_policy_summary policy;
    /** @brief Every feed of the class, in source name order. */
    std::vector<calibrated_feed> feeds;

    friend bool operator==(const calibrate_response&, const calibrate_response&) = default;
};

/**
 * @brief Approve or reject one proposed variance calibration, with the reviewer's reason.
 *
 * Approving supersedes the pair's earlier approval and points every feed of the pair that named
 * it at the new one, each a new version of its config; rejecting leaves the feeds alone. Only a
 * proposed calibration can be reviewed, and each decision is a new version of the calibration.
 */
struct review_variance_calibration_request {
    using response_type = struct review_variance_calibration_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.review_variance_calibration";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief The proposed calibration to decide. */
    std::string calibration_id;
    /** @brief "approve" or "reject". */
    std::string decision;
    /** @brief Why: kept as the calibration's state reason. Required. */
    std::string reason;

    friend bool operator==(const review_variance_calibration_request&,
                           const review_variance_calibration_request&) = default;
};

/**
 * @brief What the review did: the calibration's new state, the approval it superseded, and the
 * feeds it pointed at the new calibration.
 */
struct review_variance_calibration_response {
    /** @brief True when the decision was written. */
    bool success = false;
    /** @brief Why the decision was refused, or what it did. */
    std::string message;
    /** @brief Why the review was refused: "invalid", "not_found" or "decided"; empty when it was
     * not. */
    std::string refusal;
    /** @brief The calibration's state after the review. */
    std::string state;
    /** @brief The approval the new one superseded; empty when there was none. */
    std::string superseded_id;
    /** @brief The source names of the feeds now pointed at the approved calibration. */
    std::vector<std::string> repointed_feeds;

    friend bool operator==(const review_variance_calibration_response&,
                           const review_variance_calibration_response&) = default;
};

/**
 * @brief The members of start_feed_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const start_feed_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"config_id", &start_feed_request::config_id},
    };
}

/**
 * @brief The members of start_feed_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const start_feed_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &start_feed_response::success},
        ::ores::utility::codec::field{"message", &start_feed_response::message},
    };
}

/**
 * @brief The members of stop_feed_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const stop_feed_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"config_id", &stop_feed_request::config_id},
        ::ores::utility::codec::field{"source_name", &stop_feed_request::source_name},
    };
}

/**
 * @brief The members of stop_feed_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const stop_feed_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &stop_feed_response::success},
        ::ores::utility::codec::field{"message", &stop_feed_response::message},
    };
}

/**
 * @brief The members of provider_dispersion, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const provider_dispersion&) {
    return std::tuple{
        ::ores::utility::codec::field{"provider_code", &provider_dispersion::provider_code},
        ::ores::utility::codec::field{"item", &provider_dispersion::item},
        ::ores::utility::codec::field{"basis_points", &provider_dispersion::basis_points},
    };
}

/**
 * @brief The members of variance_start, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const variance_start&) {
    return std::tuple{
        ::ores::utility::codec::field{"model", &variance_start::model},
        ::ores::utility::codec::field{"calibration_id", &variance_start::calibration_id},
        ::ores::utility::codec::field{"calibration_version", &variance_start::calibration_version},
        ::ores::utility::codec::field{"fitted", &variance_start::fitted},
        ::ores::utility::codec::field{"alpha", &variance_start::alpha},
        ::ores::utility::codec::field{"gamma", &variance_start::gamma},
        ::ores::utility::codec::field{"beta", &variance_start::beta},
        ::ores::utility::codec::field{"persistence", &variance_start::persistence},
        ::ores::utility::codec::field{"initial_ratio", &variance_start::initial_ratio},
        ::ores::utility::codec::field{"returns", &variance_start::returns},
        ::ores::utility::codec::field{"note", &variance_start::note},
    };
}

/**
 * @brief The members of feed_start, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const feed_start&) {
    return std::tuple{
        ::ores::utility::codec::field{"from_fixing", &feed_start::from_fixing},
        ::ores::utility::codec::field{"item", &feed_start::item},
        ::ores::utility::codec::field{"provider_code", &feed_start::provider_code},
        ::ores::utility::codec::field{"vendor_id", &feed_start::vendor_id},
        ::ores::utility::codec::field{"value", &feed_start::value},
        ::ores::utility::codec::field{"inverted", &feed_start::inverted},
        ::ores::utility::codec::field{"fixing_date", &feed_start::fixing_date},
        ::ores::utility::codec::field{"received_at", &feed_start::received_at},
        ::ores::utility::codec::field{"window_days", &feed_start::window_days},
        ::ores::utility::codec::field{"used", &feed_start::used},
        ::ores::utility::codec::field{"hourly_spread", &feed_start::hourly_spread},
        ::ores::utility::codec::field{"note", &feed_start::note},
        ::ores::utility::codec::field{"age_days", &feed_start::age_days},
        ::ores::utility::codec::field{"daily_spread", &feed_start::daily_spread},
        ::ores::utility::codec::field{"annual_drift", &feed_start::annual_drift},
        ::ores::utility::codec::field{"min_return", &feed_start::min_return},
        ::ores::utility::codec::field{"max_return", &feed_start::max_return},
        ::ores::utility::codec::field{"jumps", &feed_start::jumps},
        ::ores::utility::codec::field{"dispersion", &feed_start::dispersion},
        ::ores::utility::codec::field{"variance", &feed_start::variance},
    };
}

/**
 * @brief The members of running_feed, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const running_feed&) {
    return std::tuple{
        ::ores::utility::codec::field{"source_name", &running_feed::source_name},
        ::ores::utility::codec::field{"start", &running_feed::start},
    };
}

/**
 * @brief The members of list_feeds_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_feeds_request&) {
    return std::tuple{};
}

/**
 * @brief The members of list_feeds_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_feeds_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &list_feeds_response::success},
        ::ores::utility::codec::field{"running_feeds", &list_feeds_response::running_feeds},
    };
}

/**
 * @brief The members of calibration_policy_summary, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calibration_policy_summary&) {
    return std::tuple{
        ::ores::utility::codec::field{"from_policy_row",
                                      &calibration_policy_summary::from_policy_row},
        ::ores::utility::codec::field{"window_days", &calibration_policy_summary::window_days},
        ::ores::utility::codec::field{"variance_window_days",
                                      &calibration_policy_summary::variance_window_days},
        ::ores::utility::codec::field{"allow_inverse", &calibration_policy_summary::allow_inverse},
        ::ores::utility::codec::field{"fallback_code", &calibration_policy_summary::fallback_code},
        ::ores::utility::codec::field{"provider_order",
                                      &calibration_policy_summary::provider_order},
    };
}

/**
 * @brief The members of candidate_outcome, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const candidate_outcome&) {
    return std::tuple{
        ::ores::utility::codec::field{"item", &candidate_outcome::item},
        ::ores::utility::codec::field{"provider_code", &candidate_outcome::provider_code},
        ::ores::utility::codec::field{"inverted", &candidate_outcome::inverted},
        ::ores::utility::codec::field{"rank", &candidate_outcome::rank},
        ::ores::utility::codec::field{"state", &candidate_outcome::state},
        ::ores::utility::codec::field{"reason", &candidate_outcome::reason},
    };
}

/**
 * @brief The members of variance_calibration_summary, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const variance_calibration_summary&) {
    return std::tuple{
        ::ores::utility::codec::field{"input_id", &variance_calibration_summary::input_id},
        ::ores::utility::codec::field{"calibration_id",
                                      &variance_calibration_summary::calibration_id},
        ::ores::utility::codec::field{"method", &variance_calibration_summary::method},
        ::ores::utility::codec::field{"state", &variance_calibration_summary::state},
        ::ores::utility::codec::field{"state_reason", &variance_calibration_summary::state_reason},
        ::ores::utility::codec::field{"item", &variance_calibration_summary::item},
        ::ores::utility::codec::field{"provider_code",
                                      &variance_calibration_summary::provider_code},
        ::ores::utility::codec::field{"inverted", &variance_calibration_summary::inverted},
        ::ores::utility::codec::field{"window_days", &variance_calibration_summary::window_days},
        ::ores::utility::codec::field{"window_end", &variance_calibration_summary::window_end},
        ::ores::utility::codec::field{"point_count", &variance_calibration_summary::point_count},
        ::ores::utility::codec::field{"used_count", &variance_calibration_summary::used_count},
        ::ores::utility::codec::field{"alpha", &variance_calibration_summary::alpha},
        ::ores::utility::codec::field{"gamma", &variance_calibration_summary::gamma},
        ::ores::utility::codec::field{"beta", &variance_calibration_summary::beta},
        ::ores::utility::codec::field{"persistence", &variance_calibration_summary::persistence},
        ::ores::utility::codec::field{"starting_variance_ratio",
                                      &variance_calibration_summary::starting_variance_ratio},
        ::ores::utility::codec::field{"starts", &variance_calibration_summary::starts},
        ::ores::utility::codec::field{"starts_agreed",
                                      &variance_calibration_summary::starts_agreed},
    };
}

/**
 * @brief The members of calibrated_feed, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calibrated_feed&) {
    return std::tuple{
        ::ores::utility::codec::field{"config_id", &calibrated_feed::config_id},
        ::ores::utility::codec::field{"source_name", &calibrated_feed::source_name},
        ::ores::utility::codec::field{"base_currency_code", &calibrated_feed::base_currency_code},
        ::ores::utility::codec::field{"quote_currency_code", &calibrated_feed::quote_currency_code},
        ::ores::utility::codec::field{"explicit_items", &calibrated_feed::explicit_items},
        ::ores::utility::codec::field{"window_days_override",
                                      &calibrated_feed::window_days_override},
        ::ores::utility::codec::field{"refused", &calibrated_feed::refused},
        ::ores::utility::codec::field{"start", &calibrated_feed::start},
        ::ores::utility::codec::field{"candidates", &calibrated_feed::candidates},
        ::ores::utility::codec::field{"running", &calibrated_feed::running},
        ::ores::utility::codec::field{"outcome", &calibrated_feed::outcome},
        ::ores::utility::codec::field{"outcome_message", &calibrated_feed::outcome_message},
        ::ores::utility::codec::field{"variance", &calibrated_feed::variance},
        ::ores::utility::codec::field{"current_variance", &calibrated_feed::current_variance},
    };
}

/**
 * @brief The members of calibrate_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calibrate_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"asset_class_code", &calibrate_request::asset_class_code},
        ::ores::utility::codec::field{"config_id", &calibrate_request::config_id},
        ::ores::utility::codec::field{"apply", &calibrate_request::apply},
    };
}

/**
 * @brief The members of calibrate_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calibrate_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &calibrate_response::success},
        ::ores::utility::codec::field{"message", &calibrate_response::message},
        ::ores::utility::codec::field{"policy", &calibrate_response::policy},
        ::ores::utility::codec::field{"feeds", &calibrate_response::feeds},
    };
}

/**
 * @brief The members of review_variance_calibration_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const review_variance_calibration_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"calibration_id",
                                      &review_variance_calibration_request::calibration_id},
        ::ores::utility::codec::field{"decision", &review_variance_calibration_request::decision},
        ::ores::utility::codec::field{"reason", &review_variance_calibration_request::reason},
    };
}

/**
 * @brief The members of review_variance_calibration_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const review_variance_calibration_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &review_variance_calibration_response::success},
        ::ores::utility::codec::field{"message", &review_variance_calibration_response::message},
        ::ores::utility::codec::field{"refusal", &review_variance_calibration_response::refusal},
        ::ores::utility::codec::field{"state", &review_variance_calibration_response::state},
        ::ores::utility::codec::field{"superseded_id",
                                      &review_variance_calibration_response::superseded_id},
        ::ores::utility::codec::field{"repointed_feeds",
                                      &review_variance_calibration_response::repointed_feeds},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::start_feed_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::start_feed_request& v);
    static ores::synthetic::messaging::start_feed_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::start_feed_request& v);
    static ores::synthetic::messaging::start_feed_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::start_feed_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::start_feed_response& v);
    static ores::synthetic::messaging::start_feed_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::start_feed_response& v);
    static ores::synthetic::messaging::start_feed_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::stop_feed_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::stop_feed_request& v);
    static ores::synthetic::messaging::stop_feed_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::stop_feed_request& v);
    static ores::synthetic::messaging::stop_feed_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::stop_feed_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::stop_feed_response& v);
    static ores::synthetic::messaging::stop_feed_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::stop_feed_response& v);
    static ores::synthetic::messaging::stop_feed_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::provider_dispersion> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::provider_dispersion& v);
    static ores::synthetic::messaging::provider_dispersion read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::provider_dispersion& v);
    static ores::synthetic::messaging::provider_dispersion
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::variance_start> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::variance_start& v);
    static ores::synthetic::messaging::variance_start read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::variance_start& v);
    static ores::synthetic::messaging::variance_start read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::feed_start> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::feed_start& v);
    static ores::synthetic::messaging::feed_start read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::feed_start& v);
    static ores::synthetic::messaging::feed_start read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::running_feed> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::running_feed& v);
    static ores::synthetic::messaging::running_feed read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::running_feed& v);
    static ores::synthetic::messaging::running_feed read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::list_feeds_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::list_feeds_request& v);
    static ores::synthetic::messaging::list_feeds_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::list_feeds_request& v);
    static ores::synthetic::messaging::list_feeds_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::list_feeds_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::list_feeds_response& v);
    static ores::synthetic::messaging::list_feeds_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::list_feeds_response& v);
    static ores::synthetic::messaging::list_feeds_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::calibration_policy_summary> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::calibration_policy_summary& v);
    static ores::synthetic::messaging::calibration_policy_summary read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::calibration_policy_summary& v);
    static ores::synthetic::messaging::calibration_policy_summary
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::candidate_outcome> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::candidate_outcome& v);
    static ores::synthetic::messaging::candidate_outcome read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::candidate_outcome& v);
    static ores::synthetic::messaging::candidate_outcome
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::variance_calibration_summary> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::variance_calibration_summary& v);
    static ores::synthetic::messaging::variance_calibration_summary
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::variance_calibration_summary& v);
    static ores::synthetic::messaging::variance_calibration_summary
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::calibrated_feed> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::calibrated_feed& v);
    static ores::synthetic::messaging::calibrated_feed read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::calibrated_feed& v);
    static ores::synthetic::messaging::calibrated_feed
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::calibrate_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::calibrate_request& v);
    static ores::synthetic::messaging::calibrate_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::calibrate_request& v);
    static ores::synthetic::messaging::calibrate_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::calibrate_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::calibrate_response& v);
    static ores::synthetic::messaging::calibrate_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::calibrate_response& v);
    static ores::synthetic::messaging::calibrate_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::review_variance_calibration_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::review_variance_calibration_request& v);
    static ores::synthetic::messaging::review_variance_calibration_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::review_variance_calibration_request& v);
    static ores::synthetic::messaging::review_variance_calibration_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::review_variance_calibration_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::review_variance_calibration_response& v);
    static ores::synthetic::messaging::review_variance_calibration_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::review_variance_calibration_response& v);
    static ores::synthetic::messaging::review_variance_calibration_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
