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

#include <cstdint>
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
 * @brief Request the set of currently running feed source_names, every kind.
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
 * @brief The running feeds' source_names, and whether the read succeeded.
 */
struct list_feeds_response {
    bool success = false;
    std::vector<std::string> running_source_names;
};

}

#endif
