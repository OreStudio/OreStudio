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
#ifndef ORES_MARKETDATA_API_MESSAGING_FEED_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_FEED_PROTOCOL_HPP

#include "ores.marketdata.api/domain/feed.hpp"
#include "ores.marketdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::marketdata::messaging {

struct feed_key {
    std::string name;

    friend bool operator==(const feed_key&, const feed_key&) = default;
};

struct feed_write {
    boost::uuids::uuid id;
    std::string name;
    std::string publisher_code;
    std::string session_code;
    std::string service;
    std::string provider_key;
    bool is_default;
    std::string description;

    friend bool operator==(const feed_write&, const feed_write&) = default;
};

struct feed_change {
    feed_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const feed_change&, const feed_change&) = default;
};

struct feed_removal {
    feed_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const feed_removal&, const feed_removal&) = default;
};

struct feed_lookup {
    feed_key key;
    std::optional<ores::marketdata::domain::feed> feed;

    friend bool operator==(const feed_lookup&, const feed_lookup&) = default;
};

struct feeds_filter {
    std::optional<std::string> publisher_code;
    std::optional<std::string> session_code;
    std::optional<std::string> service;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<std::string>> publisher_code_one_of;
    std::optional<std::vector<std::string>> session_code_one_of;
    std::optional<std::vector<std::string>> service_one_of;
    std::optional<std::string> search;

    friend bool operator==(const feeds_filter&, const feeds_filter&) = default;
};

struct feed_event {
    boost::uuids::uuid event_id;
    feed_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const feed_event&, const feed_event&) = default;
};

struct feed_version_key {
    feed_key feed;
    std::uint32_t version;

    friend bool operator==(const feed_version_key&, const feed_version_key&) = default;
};

struct feed_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const feed_versions_filter&, const feed_versions_filter&) = default;
};

struct list_feeds_request {
    using response_type = struct list_feeds_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feeds.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<feeds_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_feeds_request&, const list_feeds_request&) = default;
};

struct list_feeds_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::feed> feeds;
    std::uint64_t total;

    friend bool operator==(const list_feeds_response&, const list_feeds_response&) = default;
};

struct get_feed_request {
    using response_type = struct get_feed_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feeds.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    feed_key key;

    friend bool operator==(const get_feed_request&, const get_feed_request&) = default;
};

struct get_feed_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::feed> feed;

    friend bool operator==(const get_feed_response&, const get_feed_response&) = default;
};

struct get_many_feeds_request {
    using response_type = struct get_many_feeds_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feeds.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<feed_key> keys;

    friend bool operator==(const get_many_feeds_request&, const get_many_feeds_request&) = default;
};

struct get_many_feeds_response {
    ores::utility::domain::result result;
    std::vector<feed_lookup> entries;

    friend bool operator==(const get_many_feeds_response&,
                           const get_many_feeds_response&) = default;
};

struct put_feed_request {
    using response_type = struct put_feed_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feeds.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    feed_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_feed_request&, const put_feed_request&) = default;
};

struct put_feed_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::feed> feed;

    friend bool operator==(const put_feed_response&, const put_feed_response&) = default;
};

struct put_many_feeds_request {
    using response_type = struct put_many_feeds_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feeds.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<feed_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_feeds_request&, const put_many_feeds_request&) = default;
};

struct put_many_feeds_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::feed> feeds;

    friend bool operator==(const put_many_feeds_response&,
                           const put_many_feeds_response&) = default;
};

struct delete_feed_request {
    using response_type = struct delete_feed_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feeds.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    feed_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_feed_request&, const delete_feed_request&) = default;
};

struct delete_feed_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_feed_response&, const delete_feed_response&) = default;
};

struct delete_many_feeds_request {
    using response_type = struct delete_many_feeds_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feeds.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<feed_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_feeds_request&,
                           const delete_many_feeds_request&) = default;
};

struct delete_many_feeds_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_feeds_response&,
                           const delete_many_feeds_response&) = default;
};

struct list_feed_versions_request {
    using response_type = struct list_feed_versions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feeds_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    feed_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<feed_versions_filter> filter;

    friend bool operator==(const list_feed_versions_request&,
                           const list_feed_versions_request&) = default;
};

struct list_feed_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::feed> versions;
    std::uint64_t total;

    friend bool operator==(const list_feed_versions_response&,
                           const list_feed_versions_response&) = default;
};

struct get_feed_version_request {
    using response_type = struct get_feed_version_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feeds_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    feed_version_key key;

    friend bool operator==(const get_feed_version_request&,
                           const get_feed_version_request&) = default;
};

struct get_feed_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::feed> version;

    friend bool operator==(const get_feed_version_response&,
                           const get_feed_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace feed_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.events.feeds.created";
inline constexpr std::string_view updated = "marketdata.v1.events.feeds.updated";
inline constexpr std::string_view deleted = "marketdata.v1.events.feeds.deleted";
}
/**
 * @brief The members of feed_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const feed_key&) {
    return std::tuple{
        utility::codec::field{"name", &feed_key::name},
    };
}

/**
 * @brief The members of feed_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const feed_write&) {
    return std::tuple{
        utility::codec::field{"id", &feed_write::id},
        utility::codec::field{"name", &feed_write::name},
        utility::codec::field{"publisher_code", &feed_write::publisher_code},
        utility::codec::field{"session_code", &feed_write::session_code},
        utility::codec::field{"service", &feed_write::service},
        utility::codec::field{"provider_key", &feed_write::provider_key},
        utility::codec::field{"is_default", &feed_write::is_default},
        utility::codec::field{"description", &feed_write::description},
    };
}

/**
 * @brief The members of feed_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const feed_change&) {
    return std::tuple{
        utility::codec::field{"write", &feed_change::write},
        utility::codec::field{"precondition", &feed_change::precondition},
    };
}

/**
 * @brief The members of feed_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const feed_removal&) {
    return std::tuple{
        utility::codec::field{"key", &feed_removal::key},
        utility::codec::field{"precondition", &feed_removal::precondition},
    };
}

/**
 * @brief The members of feed_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const feed_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &feed_lookup::key},
        utility::codec::field{"feed", &feed_lookup::feed},
    };
}

/**
 * @brief The members of feeds_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const feeds_filter&) {
    return std::tuple{
        utility::codec::field{"publisher_code", &feeds_filter::publisher_code},
        utility::codec::field{"session_code", &feeds_filter::session_code},
        utility::codec::field{"service", &feeds_filter::service},
        utility::codec::field{"id_one_of", &feeds_filter::id_one_of},
        utility::codec::field{"publisher_code_one_of", &feeds_filter::publisher_code_one_of},
        utility::codec::field{"session_code_one_of", &feeds_filter::session_code_one_of},
        utility::codec::field{"service_one_of", &feeds_filter::service_one_of},
        utility::codec::field{"search", &feeds_filter::search},
    };
}

/**
 * @brief The members of feed_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const feed_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &feed_event::event_id},
        utility::codec::field{"key", &feed_event::key},
        utility::codec::field{"action", &feed_event::action},
        utility::codec::field{"version", &feed_event::version},
        utility::codec::field{"occurred_at", &feed_event::occurred_at},
        utility::codec::field{"correlation_id", &feed_event::correlation_id},
    };
}

/**
 * @brief The members of feed_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const feed_version_key&) {
    return std::tuple{
        utility::codec::field{"feed", &feed_version_key::feed},
        utility::codec::field{"version", &feed_version_key::version},
    };
}

/**
 * @brief The members of feed_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const feed_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &feed_versions_filter::version},
        utility::codec::field{"from_version", &feed_versions_filter::from_version},
        utility::codec::field{"to_version", &feed_versions_filter::to_version},
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
    return std::tuple{
        utility::codec::field{"offset", &list_feeds_request::offset},
        utility::codec::field{"limit", &list_feeds_request::limit},
        utility::codec::field{"order", &list_feeds_request::order},
        utility::codec::field{"filter", &list_feeds_request::filter},
        utility::codec::field{"as_of", &list_feeds_request::as_of},
    };
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
        utility::codec::field{"result", &list_feeds_response::result},
        utility::codec::field{"feeds", &list_feeds_response::feeds},
        utility::codec::field{"total", &list_feeds_response::total},
    };
}

/**
 * @brief The members of get_feed_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_feed_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_feed_request::key},
    };
}

/**
 * @brief The members of get_feed_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_feed_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_feed_response::result},
        utility::codec::field{"feed", &get_feed_response::feed},
    };
}

/**
 * @brief The members of get_many_feeds_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_feeds_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_feeds_request::keys},
    };
}

/**
 * @brief The members of get_many_feeds_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_feeds_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_feeds_response::result},
        utility::codec::field{"entries", &get_many_feeds_response::entries},
    };
}

/**
 * @brief The members of put_feed_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_feed_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_feed_request::change},
        utility::codec::field{"intent", &put_feed_request::intent},
    };
}

/**
 * @brief The members of put_feed_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_feed_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_feed_response::result},
        utility::codec::field{"feed", &put_feed_response::feed},
    };
}

/**
 * @brief The members of put_many_feeds_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_feeds_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_feeds_request::changes},
        utility::codec::field{"intent", &put_many_feeds_request::intent},
    };
}

/**
 * @brief The members of put_many_feeds_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_feeds_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_feeds_response::result},
        utility::codec::field{"feeds", &put_many_feeds_response::feeds},
    };
}

/**
 * @brief The members of delete_feed_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_feed_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_feed_request::removal},
        utility::codec::field{"intent", &delete_feed_request::intent},
    };
}

/**
 * @brief The members of delete_feed_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_feed_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_feed_response::result},
    };
}

/**
 * @brief The members of delete_many_feeds_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_feeds_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_feeds_request::removals},
        utility::codec::field{"intent", &delete_many_feeds_request::intent},
    };
}

/**
 * @brief The members of delete_many_feeds_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_feeds_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_feeds_response::result},
    };
}

/**
 * @brief The members of list_feed_versions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_feed_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_feed_versions_request::key},
        utility::codec::field{"offset", &list_feed_versions_request::offset},
        utility::codec::field{"limit", &list_feed_versions_request::limit},
        utility::codec::field{"order", &list_feed_versions_request::order},
        utility::codec::field{"filter", &list_feed_versions_request::filter},
    };
}

/**
 * @brief The members of list_feed_versions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_feed_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_feed_versions_response::result},
        utility::codec::field{"versions", &list_feed_versions_response::versions},
        utility::codec::field{"total", &list_feed_versions_response::total},
    };
}

/**
 * @brief The members of get_feed_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_feed_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_feed_version_request::key},
    };
}

/**
 * @brief The members of get_feed_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_feed_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_feed_version_response::result},
        utility::codec::field{"version", &get_feed_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::feed_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::feed_key& v);
    static ores::marketdata::messaging::feed_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::feed_key& v);
    static ores::marketdata::messaging::feed_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::feed_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::feed_write& v);
    static ores::marketdata::messaging::feed_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::feed_write& v);
    static ores::marketdata::messaging::feed_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::feed_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::feed_change& v);
    static ores::marketdata::messaging::feed_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::feed_change& v);
    static ores::marketdata::messaging::feed_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::feed_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::feed_removal& v);
    static ores::marketdata::messaging::feed_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::feed_removal& v);
    static ores::marketdata::messaging::feed_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::feed_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::feed_lookup& v);
    static ores::marketdata::messaging::feed_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::feed_lookup& v);
    static ores::marketdata::messaging::feed_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::feeds_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::feeds_filter& v);
    static ores::marketdata::messaging::feeds_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::feeds_filter& v);
    static ores::marketdata::messaging::feeds_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::feed_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::feed_event& v);
    static ores::marketdata::messaging::feed_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::feed_event& v);
    static ores::marketdata::messaging::feed_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::feed_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::feed_version_key& v);
    static ores::marketdata::messaging::feed_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::feed_version_key& v);
    static ores::marketdata::messaging::feed_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::feed_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::feed_versions_filter& v);
    static ores::marketdata::messaging::feed_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::feed_versions_filter& v);
    static ores::marketdata::messaging::feed_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::list_feeds_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::list_feeds_request& v);
    static ores::marketdata::messaging::list_feeds_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::list_feeds_request& v);
    static ores::marketdata::messaging::list_feeds_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::list_feeds_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::list_feeds_response& v);
    static ores::marketdata::messaging::list_feeds_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::list_feeds_response& v);
    static ores::marketdata::messaging::list_feeds_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::get_feed_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_feed_request& v);
    static ores::marketdata::messaging::get_feed_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::get_feed_request& v);
    static ores::marketdata::messaging::get_feed_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::get_feed_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_feed_response& v);
    static ores::marketdata::messaging::get_feed_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::get_feed_response& v);
    static ores::marketdata::messaging::get_feed_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::get_many_feeds_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_many_feeds_request& v);
    static ores::marketdata::messaging::get_many_feeds_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::get_many_feeds_request& v);
    static ores::marketdata::messaging::get_many_feeds_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_feeds_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_many_feeds_response& v);
    static ores::marketdata::messaging::get_many_feeds_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::get_many_feeds_response& v);
    static ores::marketdata::messaging::get_many_feeds_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::put_feed_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_feed_request& v);
    static ores::marketdata::messaging::put_feed_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::put_feed_request& v);
    static ores::marketdata::messaging::put_feed_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::put_feed_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_feed_response& v);
    static ores::marketdata::messaging::put_feed_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::put_feed_response& v);
    static ores::marketdata::messaging::put_feed_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::put_many_feeds_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_many_feeds_request& v);
    static ores::marketdata::messaging::put_many_feeds_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::put_many_feeds_request& v);
    static ores::marketdata::messaging::put_many_feeds_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_feeds_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_many_feeds_response& v);
    static ores::marketdata::messaging::put_many_feeds_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::put_many_feeds_response& v);
    static ores::marketdata::messaging::put_many_feeds_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::delete_feed_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delete_feed_request& v);
    static ores::marketdata::messaging::delete_feed_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::delete_feed_request& v);
    static ores::marketdata::messaging::delete_feed_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::delete_feed_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delete_feed_response& v);
    static ores::marketdata::messaging::delete_feed_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::delete_feed_response& v);
    static ores::marketdata::messaging::delete_feed_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_feeds_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delete_many_feeds_request& v);
    static ores::marketdata::messaging::delete_many_feeds_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_feeds_request& v);
    static ores::marketdata::messaging::delete_many_feeds_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_feeds_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delete_many_feeds_response& v);
    static ores::marketdata::messaging::delete_many_feeds_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_feeds_response& v);
    static ores::marketdata::messaging::delete_many_feeds_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_feed_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::list_feed_versions_request& v);
    static ores::marketdata::messaging::list_feed_versions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_feed_versions_request& v);
    static ores::marketdata::messaging::list_feed_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_feed_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_feed_versions_response& v);
    static ores::marketdata::messaging::list_feed_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_feed_versions_response& v);
    static ores::marketdata::messaging::list_feed_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_feed_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_feed_version_request& v);
    static ores::marketdata::messaging::get_feed_version_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_feed_version_request& v);
    static ores::marketdata::messaging::get_feed_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_feed_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_feed_version_response& v);
    static ores::marketdata::messaging::get_feed_version_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_feed_version_response& v);
    static ores::marketdata::messaging::get_feed_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
