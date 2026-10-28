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

#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
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
};

/**
 * @brief Whether the feed started, and why not when it did not.
 */
struct start_feed_response {
    bool success = false;
    std::string message;
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
};

/**
 * @brief Whether the feed stopped, and why not when it did not.
 */
struct stop_feed_response {
    bool success = false;
    std::string message;
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
};

/**
 * @brief The feeds the service runs now, each with where it started, and whether
 * the read succeeded.
 */
struct list_feeds_response {
    bool success = false;
    /** @brief The feeds that run, in source name order. */
    std::vector<running_feed> running_feeds;
};

}

#endif
