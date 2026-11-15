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

#include "ores.synthetic.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
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

    friend bool operator==(const start_selected_feeds_request&,
                           const start_selected_feeds_request&) = default;
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

    friend bool operator==(const start_selected_feeds_response&,
                           const start_selected_feeds_response&) = default;
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

    friend bool operator==(const stop_selected_feeds_request&,
                           const stop_selected_feeds_request&) = default;
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

    friend bool operator==(const stop_selected_feeds_response&,
                           const stop_selected_feeds_response&) = default;
};

/**
 * @brief The members of start_selected_feeds_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<start_selected_feeds_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"folder_id", &start_selected_feeds_request::folder_id},
        ::ores::utility::codec::field{"provider_code",
                                      &start_selected_feeds_request::provider_code},
        ::ores::utility::codec::field{"should_run", &start_selected_feeds_request::should_run},
        ::ores::utility::codec::field{"search", &start_selected_feeds_request::search},
        ::ores::utility::codec::field{"state", &start_selected_feeds_request::state},
        ::ores::utility::codec::field{"asset_class", &start_selected_feeds_request::asset_class},
        ::ores::utility::codec::field{"feed_ids", &start_selected_feeds_request::feed_ids},
        ::ores::utility::codec::field{"excluded_ids", &start_selected_feeds_request::excluded_ids},
    };
}

/**
 * @brief The members of start_selected_feeds_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<start_selected_feeds_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &start_selected_feeds_response::success},
        ::ores::utility::codec::field{"message", &start_selected_feeds_response::message},
        ::ores::utility::codec::field{"started", &start_selected_feeds_response::started},
        ::ores::utility::codec::field{"already_running",
                                      &start_selected_feeds_response::already_running},
        ::ores::utility::codec::field{"skipped", &start_selected_feeds_response::skipped},
        ::ores::utility::codec::field{"stopped", &start_selected_feeds_response::stopped},
    };
}

/**
 * @brief The members of stop_selected_feeds_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stop_selected_feeds_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"folder_id", &stop_selected_feeds_request::folder_id},
        ::ores::utility::codec::field{"provider_code", &stop_selected_feeds_request::provider_code},
        ::ores::utility::codec::field{"should_run", &stop_selected_feeds_request::should_run},
        ::ores::utility::codec::field{"search", &stop_selected_feeds_request::search},
        ::ores::utility::codec::field{"state", &stop_selected_feeds_request::state},
        ::ores::utility::codec::field{"asset_class", &stop_selected_feeds_request::asset_class},
        ::ores::utility::codec::field{"feed_ids", &stop_selected_feeds_request::feed_ids},
        ::ores::utility::codec::field{"excluded_ids", &stop_selected_feeds_request::excluded_ids},
    };
}

/**
 * @brief The members of stop_selected_feeds_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stop_selected_feeds_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &stop_selected_feeds_response::success},
        ::ores::utility::codec::field{"message", &stop_selected_feeds_response::message},
        ::ores::utility::codec::field{"started", &stop_selected_feeds_response::started},
        ::ores::utility::codec::field{"already_running",
                                      &stop_selected_feeds_response::already_running},
        ::ores::utility::codec::field{"skipped", &stop_selected_feeds_response::skipped},
        ::ores::utility::codec::field{"stopped", &stop_selected_feeds_response::stopped},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::start_selected_feeds_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::start_selected_feeds_request& v);
    static ores::synthetic::messaging::start_selected_feeds_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::start_selected_feeds_request& v);
    static ores::synthetic::messaging::start_selected_feeds_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::start_selected_feeds_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::start_selected_feeds_response& v);
    static ores::synthetic::messaging::start_selected_feeds_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::start_selected_feeds_response& v);
    static ores::synthetic::messaging::start_selected_feeds_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::stop_selected_feeds_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::stop_selected_feeds_request& v);
    static ores::synthetic::messaging::stop_selected_feeds_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::stop_selected_feeds_request& v);
    static ores::synthetic::messaging::stop_selected_feeds_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::stop_selected_feeds_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::stop_selected_feeds_response& v);
    static ores::synthetic::messaging::stop_selected_feeds_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::stop_selected_feeds_response& v);
    static ores::synthetic::messaging::stop_selected_feeds_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
