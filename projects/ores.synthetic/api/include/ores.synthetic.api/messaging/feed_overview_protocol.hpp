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
#ifndef ORES_SYNTHETIC_API_MESSAGING_FEED_OVERVIEW_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_FEED_OVERVIEW_PROTOCOL_HPP

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
 * @brief One folder of the feed hierarchy, with what runs under it.
 *
 * A collection folder also carries the provider the collection stands for.
 */
struct feed_folder {
    /** @brief The folder's id. */
    std::string id;
    /** @brief The parent folder's id; empty for a root. */
    std::string parent_id;
    /** @brief The folder's display name. */
    std::string name;
    /** @brief What the folder is: root, collection, asset_class or instrument_type. */
    std::string kind;
    /** @brief For a collection, the provider it stands for; empty for any other folder. */
    std::string provider_code;
    /** @brief The FX spot feeds under the folder, at any depth. */
    int fx_feeds = 0;
    /** @brief The IR curve feeds under the folder, at any depth. */
    int ir_feeds = 0;
    /** @brief How many of those feeds are running now. */
    int running_feeds = 0;
    /** @brief How many of those feeds are enabled, start on their own, and are not running now. */
    int idle_auto_feeds = 0;
    /** @brief Whether the folder has folders under it. */
    bool has_children = false;

    friend bool operator==(const feed_folder&, const feed_folder&) = default;
};

/**
 * @brief Request the children of one folder, or the roots, with the feed counts.
 */
struct list_feed_folders_request {
    using response_type = struct list_feed_folders_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.list_feed_folders";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief The folder whose children are wanted; empty for the roots. */
    std::string parent_id;

    friend bool operator==(const list_feed_folders_request&,
                           const list_feed_folders_request&) = default;
};

/**
 * @brief The children of the folder asked for, and why there are none when
 * the read was refused.
 */
struct list_feed_folders_response {
    bool success = false;
    std::string message;
    /** @brief The children, in name order. */
    std::vector<feed_folder> folders;

    friend bool operator==(const list_feed_folders_response&,
                           const list_feed_folders_response&) = default;
};

/**
 * @brief One feed as the feed tree lists it: enough to name it, to open its
 * page and to tell whether it runs, and nothing of its process.
 */
struct folder_feed {
    /** @brief The feed's id. */
    std::string id;
    /** @brief The feed kind: fx_spot or ir_curve. */
    std::string kind;
    /** @brief What a person calls the feed: the pair, or the currency and the index. */
    std::string label;
    /** @brief The source name the feed publishes under. */
    std::string source_name;
    /** @brief The oresmd URI the feed publishes. */
    std::string oresmd_uri;
    /** @brief The folder the feed sits in; empty when it sits in none. */
    std::string folder_id;
    /** @brief Whether the feed is enabled. */
    bool enabled = false;
    /** @brief The provider the feed's collection stands for. */
    std::string provider_code;
    /** @brief The asset class of the feed: fx or interest_rates. */
    std::string asset_class_code;
    /** @brief What the feed prices: a currency pair such as EUR/USD, or a currency such as USD. */
    std::string underlying;
    /** @brief The process that generates the feed's ticks, as the feed stores it. */
    std::string engine;
    /** @brief How many ticks the feed makes in an hour. */
    int ticks_per_hour = 0;
    /** @brief Whether the feed starts on its own when the service starts. */
    bool auto_start = false;

    friend bool operator==(const folder_feed&, const folder_feed&) = default;
};

/**
 * @brief Request a page of the feeds that sit in one folder, or of the feeds of
 * every folder that match some words.
 */
struct list_folder_feeds_request {
    using response_type = struct list_folder_feeds_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.list_folder_feeds";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief The folder whose feeds are wanted, with those of the folders under it; empty for the
     * feeds of every folder. */
    std::string folder_id;
    /** @brief Only the feeds of this provider; empty for every provider. */
    std::string provider_code;
    /** @brief Only the feeds that are enabled, start on their own, and are not running now. */
    bool should_run = false;
    /** @brief Words that must all appear in the feed's label, source name or URI; empty for no
     * search. */
    std::string search;
    /** @brief Which feeds: all, started or stopped; empty means all. */
    std::string state;
    /** @brief Only the feeds of this asset class code; empty for every class. */
    std::string asset_class;
    /** @brief How many feeds to skip. */
    int offset = 0;
    /** @brief How many feeds to return at most. */
    int limit = 60;

    friend bool operator==(const list_folder_feeds_request&,
                           const list_folder_feeds_request&) = default;
};

/**
 * @brief A page of feeds, and how many match in all.
 */
struct list_folder_feeds_response {
    bool success = false;
    std::string message;
    /** @brief How many feeds match, of which the page holds some. */
    int total = 0;
    /** @brief The page of feeds, in label order. */
    std::vector<folder_feed> feeds;

    friend bool operator==(const list_folder_feeds_response&,
                           const list_folder_feeds_response&) = default;
};

/**
 * @brief The members of feed_folder, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<feed_folder>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"id", &feed_folder::id},
        ::ores::utility::codec::field{"parent_id", &feed_folder::parent_id},
        ::ores::utility::codec::field{"name", &feed_folder::name},
        ::ores::utility::codec::field{"kind", &feed_folder::kind},
        ::ores::utility::codec::field{"provider_code", &feed_folder::provider_code},
        ::ores::utility::codec::field{"fx_feeds", &feed_folder::fx_feeds},
        ::ores::utility::codec::field{"ir_feeds", &feed_folder::ir_feeds},
        ::ores::utility::codec::field{"running_feeds", &feed_folder::running_feeds},
        ::ores::utility::codec::field{"idle_auto_feeds", &feed_folder::idle_auto_feeds},
        ::ores::utility::codec::field{"has_children", &feed_folder::has_children},
    };
}

/**
 * @brief The members of list_feed_folders_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_feed_folders_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"parent_id", &list_feed_folders_request::parent_id},
    };
}

/**
 * @brief The members of list_feed_folders_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_feed_folders_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &list_feed_folders_response::success},
        ::ores::utility::codec::field{"message", &list_feed_folders_response::message},
        ::ores::utility::codec::field{"folders", &list_feed_folders_response::folders},
    };
}

/**
 * @brief The members of folder_feed, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<folder_feed>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"id", &folder_feed::id},
        ::ores::utility::codec::field{"kind", &folder_feed::kind},
        ::ores::utility::codec::field{"label", &folder_feed::label},
        ::ores::utility::codec::field{"source_name", &folder_feed::source_name},
        ::ores::utility::codec::field{"oresmd_uri", &folder_feed::oresmd_uri},
        ::ores::utility::codec::field{"folder_id", &folder_feed::folder_id},
        ::ores::utility::codec::field{"enabled", &folder_feed::enabled},
        ::ores::utility::codec::field{"provider_code", &folder_feed::provider_code},
        ::ores::utility::codec::field{"asset_class_code", &folder_feed::asset_class_code},
        ::ores::utility::codec::field{"underlying", &folder_feed::underlying},
        ::ores::utility::codec::field{"engine", &folder_feed::engine},
        ::ores::utility::codec::field{"ticks_per_hour", &folder_feed::ticks_per_hour},
        ::ores::utility::codec::field{"auto_start", &folder_feed::auto_start},
    };
}

/**
 * @brief The members of list_folder_feeds_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_folder_feeds_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"folder_id", &list_folder_feeds_request::folder_id},
        ::ores::utility::codec::field{"provider_code", &list_folder_feeds_request::provider_code},
        ::ores::utility::codec::field{"should_run", &list_folder_feeds_request::should_run},
        ::ores::utility::codec::field{"search", &list_folder_feeds_request::search},
        ::ores::utility::codec::field{"state", &list_folder_feeds_request::state},
        ::ores::utility::codec::field{"asset_class", &list_folder_feeds_request::asset_class},
        ::ores::utility::codec::field{"offset", &list_folder_feeds_request::offset},
        ::ores::utility::codec::field{"limit", &list_folder_feeds_request::limit},
    };
}

/**
 * @brief The members of list_folder_feeds_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_folder_feeds_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &list_folder_feeds_response::success},
        ::ores::utility::codec::field{"message", &list_folder_feeds_response::message},
        ::ores::utility::codec::field{"total", &list_folder_feeds_response::total},
        ::ores::utility::codec::field{"feeds", &list_folder_feeds_response::feeds},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::feed_folder> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::feed_folder& v);
    static ores::synthetic::messaging::feed_folder read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::feed_folder& v);
    static ores::synthetic::messaging::feed_folder read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_feed_folders_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::list_feed_folders_request& v);
    static ores::synthetic::messaging::list_feed_folders_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_feed_folders_request& v);
    static ores::synthetic::messaging::list_feed_folders_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_feed_folders_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::list_feed_folders_response& v);
    static ores::synthetic::messaging::list_feed_folders_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_feed_folders_response& v);
    static ores::synthetic::messaging::list_feed_folders_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::folder_feed> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::folder_feed& v);
    static ores::synthetic::messaging::folder_feed read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::folder_feed& v);
    static ores::synthetic::messaging::folder_feed read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_folder_feeds_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::list_folder_feeds_request& v);
    static ores::synthetic::messaging::list_folder_feeds_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_folder_feeds_request& v);
    static ores::synthetic::messaging::list_folder_feeds_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_folder_feeds_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::list_folder_feeds_response& v);
    static ores::synthetic::messaging::list_folder_feeds_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_folder_feeds_response& v);
    static ores::synthetic::messaging::list_folder_feeds_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
