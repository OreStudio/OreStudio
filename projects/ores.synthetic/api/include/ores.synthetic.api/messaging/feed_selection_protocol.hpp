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
#ifndef ORES_SYNTHETIC_API_MESSAGING_FEED_SELECTION_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_FEED_SELECTION_PROTOCOL_HPP

#include <string>
#include <vector>

namespace ores::synthetic::messaging {

/**
 * @brief Start the picked feeds, or the feeds a filter matches.
 */
struct start_selected_feeds_request {
    using response_type = struct start_selected_feeds_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.start_selected_feeds";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief The folder whose feeds are wanted, with those of the folders under it; empty for the
     * feeds of every folder. Used when no feed ids are named. */
    std::string folder_id;
    /** @brief Only the feeds of this provider; empty for every provider. Used when no feed ids are
     * named. */
    std::string provider_code;
    /** @brief Only the feeds that are enabled, start on their own, and are not running now. Used
     * when no feed ids are named. */
    bool should_run = false;
    /** @brief Words that must all appear in the feed's label, source name or URI. Used when no feed
     * ids are named. */
    std::string search;
    /** @brief Which feeds: all, started or stopped; empty means all. Used when no feed ids are
     * named. */
    std::string state;
    /** @brief Only the feeds of this asset class code; empty for every class. Used when no feed ids
     * are named. */
    std::string asset_class;
    /** @brief The feeds a person picked one by one. When it is not empty, exactly these feeds are
     * meant and the filter is ignored. */
    std::vector<std::string> feed_ids;
    /** @brief The feeds of the filter a person left out. Used when no feed ids are named. */
    std::vector<std::string> excluded_ids;
};

/**
 * @brief What a start of selected feeds did.
 */
struct start_selected_feeds_response {
    /** @brief Whether the request ran. */
    bool success = false;
    /** @brief What happened in words, or why it did not run. */
    std::string message;
    /** @brief How many feeds started. */
    int started = 0;
    /** @brief How many feeds already ran. */
    int already_running = 0;
    /** @brief How many feeds could not start: disabled, or another running feed holds the series.
     */
    int skipped = 0;
    /** @brief How many feeds stopped. */
    int stopped = 0;
};

/**
 * @brief Stop the picked feeds, or the feeds a filter matches.
 */
struct stop_selected_feeds_request {
    using response_type = struct stop_selected_feeds_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.stop_selected_feeds";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief The folder whose feeds are wanted, with those of the folders under it; empty for the
     * feeds of every folder. Used when no feed ids are named. */
    std::string folder_id;
    /** @brief Only the feeds of this provider; empty for every provider. Used when no feed ids are
     * named. */
    std::string provider_code;
    /** @brief Only the feeds that are enabled, start on their own, and are not running now. Used
     * when no feed ids are named. */
    bool should_run = false;
    /** @brief Words that must all appear in the feed's label, source name or URI. Used when no feed
     * ids are named. */
    std::string search;
    /** @brief Which feeds: all, started or stopped; empty means all. Used when no feed ids are
     * named. */
    std::string state;
    /** @brief Only the feeds of this asset class code; empty for every class. Used when no feed ids
     * are named. */
    std::string asset_class;
    /** @brief The feeds a person picked one by one. When it is not empty, exactly these feeds are
     * meant and the filter is ignored. */
    std::vector<std::string> feed_ids;
    /** @brief The feeds of the filter a person left out. Used when no feed ids are named. */
    std::vector<std::string> excluded_ids;
};

/**
 * @brief What a stop of selected feeds did.
 */
struct stop_selected_feeds_response {
    /** @brief Whether the request ran. */
    bool success = false;
    /** @brief What happened in words, or why it did not run. */
    std::string message;
    /** @brief How many feeds started. */
    int started = 0;
    /** @brief How many feeds already ran. */
    int already_running = 0;
    /** @brief How many feeds could not start: disabled, or another running feed holds the series.
     */
    int skipped = 0;
    /** @brief How many feeds stopped. */
    int stopped = 0;
};

}

#endif
