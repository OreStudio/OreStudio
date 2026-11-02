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
#ifndef ORES_MARKETDATA_API_MESSAGING_ROUTE_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_ROUTE_PROTOCOL_HPP

#include "ores.marketdata.api/domain/route.hpp"
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

struct route_key {
    std::string publisher_code;

    friend bool operator==(const route_key&, const route_key&) = default;
};

struct route_write {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    std::string publisher_code;
    std::string asset_class;
    boost::uuids::uuid feed_id;

    friend bool operator==(const route_write&, const route_write&) = default;
};

struct route_change {
    route_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const route_change&, const route_change&) = default;
};

struct route_removal {
    route_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const route_removal&, const route_removal&) = default;
};

struct route_lookup {
    route_key key;
    std::optional<ores::marketdata::domain::route> route;

    friend bool operator==(const route_lookup&, const route_lookup&) = default;
};

struct routes_filter {
    std::optional<boost::uuids::uuid> party_id;
    std::optional<std::string> publisher_code;
    std::optional<std::string> asset_class;
    std::optional<boost::uuids::uuid> feed_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> party_id_one_of;
    std::optional<std::vector<std::string>> publisher_code_one_of;
    std::optional<std::vector<std::string>> asset_class_one_of;
    std::optional<std::vector<boost::uuids::uuid>> feed_id_one_of;

    friend bool operator==(const routes_filter&, const routes_filter&) = default;
};

struct route_event {
    boost::uuids::uuid event_id;
    route_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const route_event&, const route_event&) = default;
};

struct route_version_key {
    route_key route;
    std::uint32_t version;

    friend bool operator==(const route_version_key&, const route_version_key&) = default;
};

struct route_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const route_versions_filter&, const route_versions_filter&) = default;
};

struct list_routes_request {
    using response_type = struct list_routes_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.routes.list";
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
    std::optional<routes_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_routes_request&, const list_routes_request&) = default;
};

struct list_routes_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::route> routes;
    std::uint64_t total;

    friend bool operator==(const list_routes_response&, const list_routes_response&) = default;
};

struct get_route_request {
    using response_type = struct get_route_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.routes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    route_key key;

    friend bool operator==(const get_route_request&, const get_route_request&) = default;
};

struct get_route_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::route> route;

    friend bool operator==(const get_route_response&, const get_route_response&) = default;
};

struct get_many_routes_request {
    using response_type = struct get_many_routes_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.routes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<route_key> keys;

    friend bool operator==(const get_many_routes_request&,
                           const get_many_routes_request&) = default;
};

struct get_many_routes_response {
    ores::utility::domain::result result;
    std::vector<route_lookup> entries;

    friend bool operator==(const get_many_routes_response&,
                           const get_many_routes_response&) = default;
};

struct put_route_request {
    using response_type = struct put_route_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.routes.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    route_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_route_request&, const put_route_request&) = default;
};

struct put_route_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::route> route;

    friend bool operator==(const put_route_response&, const put_route_response&) = default;
};

struct put_many_routes_request {
    using response_type = struct put_many_routes_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.routes.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<route_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_routes_request&,
                           const put_many_routes_request&) = default;
};

struct put_many_routes_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::route> routes;

    friend bool operator==(const put_many_routes_response&,
                           const put_many_routes_response&) = default;
};

struct delete_route_request {
    using response_type = struct delete_route_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.routes.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    route_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_route_request&, const delete_route_request&) = default;
};

struct delete_route_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_route_response&, const delete_route_response&) = default;
};

struct delete_many_routes_request {
    using response_type = struct delete_many_routes_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.routes.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<route_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_routes_request&,
                           const delete_many_routes_request&) = default;
};

struct delete_many_routes_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_routes_response&,
                           const delete_many_routes_response&) = default;
};

struct list_route_versions_request {
    using response_type = struct list_route_versions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.routes_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    route_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<route_versions_filter> filter;

    friend bool operator==(const list_route_versions_request&,
                           const list_route_versions_request&) = default;
};

struct list_route_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::route> versions;
    std::uint64_t total;

    friend bool operator==(const list_route_versions_response&,
                           const list_route_versions_response&) = default;
};

struct get_route_version_request {
    using response_type = struct get_route_version_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.routes_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    route_version_key key;

    friend bool operator==(const get_route_version_request&,
                           const get_route_version_request&) = default;
};

struct get_route_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::route> version;

    friend bool operator==(const get_route_version_response&,
                           const get_route_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace route_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.routes_events.created";
inline constexpr std::string_view updated = "marketdata.v1.routes_events.updated";
inline constexpr std::string_view deleted = "marketdata.v1.routes_events.deleted";
}
/**
 * @brief The members of route_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const route_key&) {
    return std::tuple{
        utility::codec::field{"publisher_code", &route_key::publisher_code},
    };
}

/**
 * @brief The members of route_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const route_write&) {
    return std::tuple{
        utility::codec::field{"id", &route_write::id},
        utility::codec::field{"party_id", &route_write::party_id},
        utility::codec::field{"publisher_code", &route_write::publisher_code},
        utility::codec::field{"asset_class", &route_write::asset_class},
        utility::codec::field{"feed_id", &route_write::feed_id},
    };
}

/**
 * @brief The members of route_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const route_change&) {
    return std::tuple{
        utility::codec::field{"write", &route_change::write},
        utility::codec::field{"precondition", &route_change::precondition},
    };
}

/**
 * @brief The members of route_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const route_removal&) {
    return std::tuple{
        utility::codec::field{"key", &route_removal::key},
        utility::codec::field{"precondition", &route_removal::precondition},
    };
}

/**
 * @brief The members of route_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const route_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &route_lookup::key},
        utility::codec::field{"route", &route_lookup::route},
    };
}

/**
 * @brief The members of routes_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const routes_filter&) {
    return std::tuple{
        utility::codec::field{"party_id", &routes_filter::party_id},
        utility::codec::field{"publisher_code", &routes_filter::publisher_code},
        utility::codec::field{"asset_class", &routes_filter::asset_class},
        utility::codec::field{"feed_id", &routes_filter::feed_id},
        utility::codec::field{"id_one_of", &routes_filter::id_one_of},
        utility::codec::field{"party_id_one_of", &routes_filter::party_id_one_of},
        utility::codec::field{"publisher_code_one_of", &routes_filter::publisher_code_one_of},
        utility::codec::field{"asset_class_one_of", &routes_filter::asset_class_one_of},
        utility::codec::field{"feed_id_one_of", &routes_filter::feed_id_one_of},
    };
}

/**
 * @brief The members of route_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const route_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &route_event::event_id},
        utility::codec::field{"key", &route_event::key},
        utility::codec::field{"action", &route_event::action},
        utility::codec::field{"version", &route_event::version},
        utility::codec::field{"occurred_at", &route_event::occurred_at},
        utility::codec::field{"correlation_id", &route_event::correlation_id},
    };
}

/**
 * @brief The members of route_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const route_version_key&) {
    return std::tuple{
        utility::codec::field{"route", &route_version_key::route},
        utility::codec::field{"version", &route_version_key::version},
    };
}

/**
 * @brief The members of route_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const route_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &route_versions_filter::version},
        utility::codec::field{"from_version", &route_versions_filter::from_version},
        utility::codec::field{"to_version", &route_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_routes_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_routes_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_routes_request::offset},
        utility::codec::field{"limit", &list_routes_request::limit},
        utility::codec::field{"order", &list_routes_request::order},
        utility::codec::field{"filter", &list_routes_request::filter},
        utility::codec::field{"as_of", &list_routes_request::as_of},
    };
}

/**
 * @brief The members of list_routes_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_routes_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_routes_response::result},
        utility::codec::field{"routes", &list_routes_response::routes},
        utility::codec::field{"total", &list_routes_response::total},
    };
}

/**
 * @brief The members of get_route_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_route_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_route_request::key},
    };
}

/**
 * @brief The members of get_route_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_route_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_route_response::result},
        utility::codec::field{"route", &get_route_response::route},
    };
}

/**
 * @brief The members of get_many_routes_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_routes_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_routes_request::keys},
    };
}

/**
 * @brief The members of get_many_routes_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_routes_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_routes_response::result},
        utility::codec::field{"entries", &get_many_routes_response::entries},
    };
}

/**
 * @brief The members of put_route_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_route_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_route_request::change},
        utility::codec::field{"intent", &put_route_request::intent},
    };
}

/**
 * @brief The members of put_route_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_route_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_route_response::result},
        utility::codec::field{"route", &put_route_response::route},
    };
}

/**
 * @brief The members of put_many_routes_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_routes_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_routes_request::changes},
        utility::codec::field{"intent", &put_many_routes_request::intent},
    };
}

/**
 * @brief The members of put_many_routes_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_routes_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_routes_response::result},
        utility::codec::field{"routes", &put_many_routes_response::routes},
    };
}

/**
 * @brief The members of delete_route_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_route_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_route_request::removal},
        utility::codec::field{"intent", &delete_route_request::intent},
    };
}

/**
 * @brief The members of delete_route_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_route_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_route_response::result},
    };
}

/**
 * @brief The members of delete_many_routes_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_routes_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_routes_request::removals},
        utility::codec::field{"intent", &delete_many_routes_request::intent},
    };
}

/**
 * @brief The members of delete_many_routes_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_routes_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_routes_response::result},
    };
}

/**
 * @brief The members of list_route_versions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_route_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_route_versions_request::key},
        utility::codec::field{"offset", &list_route_versions_request::offset},
        utility::codec::field{"limit", &list_route_versions_request::limit},
        utility::codec::field{"order", &list_route_versions_request::order},
        utility::codec::field{"filter", &list_route_versions_request::filter},
    };
}

/**
 * @brief The members of list_route_versions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_route_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_route_versions_response::result},
        utility::codec::field{"versions", &list_route_versions_response::versions},
        utility::codec::field{"total", &list_route_versions_response::total},
    };
}

/**
 * @brief The members of get_route_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_route_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_route_version_request::key},
    };
}

/**
 * @brief The members of get_route_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_route_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_route_version_response::result},
        utility::codec::field{"version", &get_route_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::route_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::route_key& v);
    static ores::marketdata::messaging::route_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::route_key& v);
    static ores::marketdata::messaging::route_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::route_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::route_write& v);
    static ores::marketdata::messaging::route_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::route_write& v);
    static ores::marketdata::messaging::route_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::route_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::route_change& v);
    static ores::marketdata::messaging::route_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::route_change& v);
    static ores::marketdata::messaging::route_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::route_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::route_removal& v);
    static ores::marketdata::messaging::route_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::route_removal& v);
    static ores::marketdata::messaging::route_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::route_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::route_lookup& v);
    static ores::marketdata::messaging::route_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::route_lookup& v);
    static ores::marketdata::messaging::route_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::routes_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::routes_filter& v);
    static ores::marketdata::messaging::routes_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::routes_filter& v);
    static ores::marketdata::messaging::routes_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::route_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::route_event& v);
    static ores::marketdata::messaging::route_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::route_event& v);
    static ores::marketdata::messaging::route_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::route_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::route_version_key& v);
    static ores::marketdata::messaging::route_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::route_version_key& v);
    static ores::marketdata::messaging::route_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::route_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::route_versions_filter& v);
    static ores::marketdata::messaging::route_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::route_versions_filter& v);
    static ores::marketdata::messaging::route_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::list_routes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::list_routes_request& v);
    static ores::marketdata::messaging::list_routes_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::list_routes_request& v);
    static ores::marketdata::messaging::list_routes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::list_routes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::list_routes_response& v);
    static ores::marketdata::messaging::list_routes_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::list_routes_response& v);
    static ores::marketdata::messaging::list_routes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::get_route_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_route_request& v);
    static ores::marketdata::messaging::get_route_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::get_route_request& v);
    static ores::marketdata::messaging::get_route_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::get_route_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_route_response& v);
    static ores::marketdata::messaging::get_route_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::get_route_response& v);
    static ores::marketdata::messaging::get_route_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_routes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_many_routes_request& v);
    static ores::marketdata::messaging::get_many_routes_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::get_many_routes_request& v);
    static ores::marketdata::messaging::get_many_routes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_routes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_many_routes_response& v);
    static ores::marketdata::messaging::get_many_routes_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_routes_response& v);
    static ores::marketdata::messaging::get_many_routes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::put_route_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_route_request& v);
    static ores::marketdata::messaging::put_route_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::put_route_request& v);
    static ores::marketdata::messaging::put_route_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::put_route_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_route_response& v);
    static ores::marketdata::messaging::put_route_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::put_route_response& v);
    static ores::marketdata::messaging::put_route_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_routes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_many_routes_request& v);
    static ores::marketdata::messaging::put_many_routes_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::put_many_routes_request& v);
    static ores::marketdata::messaging::put_many_routes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_routes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_many_routes_response& v);
    static ores::marketdata::messaging::put_many_routes_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_routes_response& v);
    static ores::marketdata::messaging::put_many_routes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::delete_route_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delete_route_request& v);
    static ores::marketdata::messaging::delete_route_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::delete_route_request& v);
    static ores::marketdata::messaging::delete_route_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::delete_route_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delete_route_response& v);
    static ores::marketdata::messaging::delete_route_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::delete_route_response& v);
    static ores::marketdata::messaging::delete_route_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_routes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delete_many_routes_request& v);
    static ores::marketdata::messaging::delete_many_routes_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_routes_request& v);
    static ores::marketdata::messaging::delete_many_routes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_routes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_routes_response& v);
    static ores::marketdata::messaging::delete_many_routes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_routes_response& v);
    static ores::marketdata::messaging::delete_many_routes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_route_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_route_versions_request& v);
    static ores::marketdata::messaging::list_route_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_route_versions_request& v);
    static ores::marketdata::messaging::list_route_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_route_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_route_versions_response& v);
    static ores::marketdata::messaging::list_route_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_route_versions_response& v);
    static ores::marketdata::messaging::list_route_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_route_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_route_version_request& v);
    static ores::marketdata::messaging::get_route_version_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_route_version_request& v);
    static ores::marketdata::messaging::get_route_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_route_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_route_version_response& v);
    static ores::marketdata::messaging::get_route_version_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_route_version_response& v);
    static ores::marketdata::messaging::get_route_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
