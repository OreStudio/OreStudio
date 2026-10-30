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
#ifndef ORES_DQ_API_MESSAGING_CATALOG_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_CATALOG_PROTOCOL_HPP

#include "ores.dq.api/domain/catalog.hpp"
#include "ores.dq.api/export.hpp"
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

namespace ores::dq::messaging {

struct catalog_key {
    std::string name;
};

struct catalog_write {
    std::string name;
    std::string description;
    std::optional<std::string> owner;
};

struct catalog_change {
    catalog_write write;
    ores::utility::domain::precondition precondition;
};

struct catalog_removal {
    catalog_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct catalog_lookup {
    catalog_key key;
    std::optional<ores::dq::domain::catalog> catalog;
};

struct catalogs_filter {
    std::optional<std::vector<std::string>> name_one_of;
};

struct catalog_event {
    boost::uuids::uuid event_id;
    catalog_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct catalog_version_key {
    catalog_key catalog;
    std::uint32_t version;
};

struct catalog_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_catalogs_request {
    using response_type = struct list_catalogs_response;
    static constexpr std::string_view nats_subject = "dq.v1.catalogs.list";
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
    std::optional<catalogs_filter> filter;
    std::optional<std::string> as_of;
};

struct list_catalogs_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::catalog> catalogs;
    std::uint64_t total;
};

struct get_catalog_request {
    using response_type = struct get_catalog_response;
    static constexpr std::string_view nats_subject = "dq.v1.catalogs.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    catalog_key key;
};

struct get_catalog_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::catalog> catalog;
};

struct get_many_catalogs_request {
    using response_type = struct get_many_catalogs_response;
    static constexpr std::string_view nats_subject = "dq.v1.catalogs.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<catalog_key> keys;
};

struct get_many_catalogs_response {
    ores::utility::domain::result result;
    std::vector<catalog_lookup> entries;
};

struct put_catalog_request {
    using response_type = struct put_catalog_response;
    static constexpr std::string_view nats_subject = "dq.v1.catalogs.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    catalog_change change;
    ores::utility::domain::change_intent intent;
};

struct put_catalog_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::catalog> catalog;
};

struct put_many_catalogs_request {
    using response_type = struct put_many_catalogs_response;
    static constexpr std::string_view nats_subject = "dq.v1.catalogs.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<catalog_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_catalogs_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::catalog> catalogs;
};

struct delete_catalog_request {
    using response_type = struct delete_catalog_response;
    static constexpr std::string_view nats_subject = "dq.v1.catalogs.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    catalog_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_catalog_response {
    ores::utility::domain::result result;
};

struct delete_many_catalogs_request {
    using response_type = struct delete_many_catalogs_response;
    static constexpr std::string_view nats_subject = "dq.v1.catalogs.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<catalog_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_catalogs_response {
    ores::utility::domain::result result;
};

struct list_catalog_versions_request {
    using response_type = struct list_catalog_versions_response;
    static constexpr std::string_view nats_subject = "dq.v1.catalogs_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    catalog_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<catalog_versions_filter> filter;
};

struct list_catalog_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::catalog> versions;
    std::uint64_t total;
};

struct get_catalog_version_request {
    using response_type = struct get_catalog_version_response;
    static constexpr std::string_view nats_subject = "dq.v1.catalogs_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    catalog_version_key key;
};

struct get_catalog_version_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::catalog> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace catalog_event_subjects {
inline constexpr std::string_view created = "dq.v1.catalogs_events.created";
inline constexpr std::string_view updated = "dq.v1.catalogs_events.updated";
inline constexpr std::string_view deleted = "dq.v1.catalogs_events.deleted";
}

/**
 * @brief The members of catalog_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const catalog_key&) {
    return std::tuple{
        utility::codec::field{"name", &catalog_key::name},
    };
}

/**
 * @brief The members of catalog_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const catalog_write&) {
    return std::tuple{
        utility::codec::field{"name", &catalog_write::name},
        utility::codec::field{"description", &catalog_write::description},
        utility::codec::field{"owner", &catalog_write::owner},
    };
}

/**
 * @brief The members of catalog_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const catalog_change&) {
    return std::tuple{
        utility::codec::field{"write", &catalog_change::write},
        utility::codec::field{"precondition", &catalog_change::precondition},
    };
}

/**
 * @brief The members of catalog_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const catalog_removal&) {
    return std::tuple{
        utility::codec::field{"key", &catalog_removal::key},
        utility::codec::field{"precondition", &catalog_removal::precondition},
    };
}

/**
 * @brief The members of catalog_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const catalog_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &catalog_lookup::key},
        utility::codec::field{"catalog", &catalog_lookup::catalog},
    };
}

/**
 * @brief The members of catalogs_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const catalogs_filter&) {
    return std::tuple{
        utility::codec::field{"name_one_of", &catalogs_filter::name_one_of},
    };
}

/**
 * @brief The members of catalog_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const catalog_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &catalog_event::event_id},
        utility::codec::field{"key", &catalog_event::key},
        utility::codec::field{"action", &catalog_event::action},
        utility::codec::field{"version", &catalog_event::version},
        utility::codec::field{"occurred_at", &catalog_event::occurred_at},
        utility::codec::field{"correlation_id", &catalog_event::correlation_id},
    };
}

/**
 * @brief The members of catalog_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const catalog_version_key&) {
    return std::tuple{
        utility::codec::field{"catalog", &catalog_version_key::catalog},
        utility::codec::field{"version", &catalog_version_key::version},
    };
}

/**
 * @brief The members of catalog_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const catalog_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &catalog_versions_filter::version},
        utility::codec::field{"from_version", &catalog_versions_filter::from_version},
        utility::codec::field{"to_version", &catalog_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_catalogs_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_catalogs_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_catalogs_request::offset},
        utility::codec::field{"limit", &list_catalogs_request::limit},
        utility::codec::field{"order", &list_catalogs_request::order},
        utility::codec::field{"filter", &list_catalogs_request::filter},
        utility::codec::field{"as_of", &list_catalogs_request::as_of},
    };
}

/**
 * @brief The members of list_catalogs_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_catalogs_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_catalogs_response::result},
        utility::codec::field{"catalogs", &list_catalogs_response::catalogs},
        utility::codec::field{"total", &list_catalogs_response::total},
    };
}

/**
 * @brief The members of get_catalog_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_catalog_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_catalog_request::key},
    };
}

/**
 * @brief The members of get_catalog_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_catalog_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_catalog_response::result},
        utility::codec::field{"catalog", &get_catalog_response::catalog},
    };
}

/**
 * @brief The members of get_many_catalogs_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_catalogs_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_catalogs_request::keys},
    };
}

/**
 * @brief The members of get_many_catalogs_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_catalogs_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_catalogs_response::result},
        utility::codec::field{"entries", &get_many_catalogs_response::entries},
    };
}

/**
 * @brief The members of put_catalog_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_catalog_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_catalog_request::change},
        utility::codec::field{"intent", &put_catalog_request::intent},
    };
}

/**
 * @brief The members of put_catalog_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_catalog_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_catalog_response::result},
        utility::codec::field{"catalog", &put_catalog_response::catalog},
    };
}

/**
 * @brief The members of put_many_catalogs_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_catalogs_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_catalogs_request::changes},
        utility::codec::field{"intent", &put_many_catalogs_request::intent},
    };
}

/**
 * @brief The members of put_many_catalogs_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_catalogs_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_catalogs_response::result},
        utility::codec::field{"catalogs", &put_many_catalogs_response::catalogs},
    };
}

/**
 * @brief The members of delete_catalog_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_catalog_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_catalog_request::removal},
        utility::codec::field{"intent", &delete_catalog_request::intent},
    };
}

/**
 * @brief The members of delete_catalog_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_catalog_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_catalog_response::result},
    };
}

/**
 * @brief The members of delete_many_catalogs_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_catalogs_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_catalogs_request::removals},
        utility::codec::field{"intent", &delete_many_catalogs_request::intent},
    };
}

/**
 * @brief The members of delete_many_catalogs_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_catalogs_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_catalogs_response::result},
    };
}

/**
 * @brief The members of list_catalog_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_catalog_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_catalog_versions_request::key},
        utility::codec::field{"offset", &list_catalog_versions_request::offset},
        utility::codec::field{"limit", &list_catalog_versions_request::limit},
        utility::codec::field{"order", &list_catalog_versions_request::order},
        utility::codec::field{"filter", &list_catalog_versions_request::filter},
    };
}

/**
 * @brief The members of list_catalog_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_catalog_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_catalog_versions_response::result},
        utility::codec::field{"versions", &list_catalog_versions_response::versions},
        utility::codec::field{"total", &list_catalog_versions_response::total},
    };
}

/**
 * @brief The members of get_catalog_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_catalog_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_catalog_version_request::key},
    };
}

/**
 * @brief The members of get_catalog_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_catalog_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_catalog_version_response::result},
        utility::codec::field{"version", &get_catalog_version_response::version},
    };
}

}

namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::catalog_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::catalog_key& v);
    static ores::dq::messaging::catalog_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::catalog_key& v);
    static ores::dq::messaging::catalog_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::catalog_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::catalog_write& v);
    static ores::dq::messaging::catalog_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::catalog_write& v);
    static ores::dq::messaging::catalog_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::catalog_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::catalog_change& v);
    static ores::dq::messaging::catalog_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::catalog_change& v);
    static ores::dq::messaging::catalog_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::catalog_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::catalog_removal& v);
    static ores::dq::messaging::catalog_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::catalog_removal& v);
    static ores::dq::messaging::catalog_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::catalog_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::catalog_lookup& v);
    static ores::dq::messaging::catalog_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::catalog_lookup& v);
    static ores::dq::messaging::catalog_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::catalogs_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::catalogs_filter& v);
    static ores::dq::messaging::catalogs_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::catalogs_filter& v);
    static ores::dq::messaging::catalogs_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::catalog_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::catalog_event& v);
    static ores::dq::messaging::catalog_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::catalog_event& v);
    static ores::dq::messaging::catalog_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::catalog_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::catalog_version_key& v);
    static ores::dq::messaging::catalog_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::catalog_version_key& v);
    static ores::dq::messaging::catalog_version_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::catalog_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::catalog_versions_filter& v);
    static ores::dq::messaging::catalog_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::catalog_versions_filter& v);
    static ores::dq::messaging::catalog_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_catalogs_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_catalogs_request& v);
    static ores::dq::messaging::list_catalogs_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_catalogs_request& v);
    static ores::dq::messaging::list_catalogs_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_catalogs_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_catalogs_response& v);
    static ores::dq::messaging::list_catalogs_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_catalogs_response& v);
    static ores::dq::messaging::list_catalogs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_catalog_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_catalog_request& v);
    static ores::dq::messaging::get_catalog_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_catalog_request& v);
    static ores::dq::messaging::get_catalog_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_catalog_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_catalog_response& v);
    static ores::dq::messaging::get_catalog_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_catalog_response& v);
    static ores::dq::messaging::get_catalog_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_catalogs_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_catalogs_request& v);
    static ores::dq::messaging::get_many_catalogs_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_many_catalogs_request& v);
    static ores::dq::messaging::get_many_catalogs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_catalogs_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_catalogs_response& v);
    static ores::dq::messaging::get_many_catalogs_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_many_catalogs_response& v);
    static ores::dq::messaging::get_many_catalogs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_catalog_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_catalog_request& v);
    static ores::dq::messaging::put_catalog_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_catalog_request& v);
    static ores::dq::messaging::put_catalog_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_catalog_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_catalog_response& v);
    static ores::dq::messaging::put_catalog_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_catalog_response& v);
    static ores::dq::messaging::put_catalog_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_catalogs_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_catalogs_request& v);
    static ores::dq::messaging::put_many_catalogs_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_many_catalogs_request& v);
    static ores::dq::messaging::put_many_catalogs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_catalogs_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_catalogs_response& v);
    static ores::dq::messaging::put_many_catalogs_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_many_catalogs_response& v);
    static ores::dq::messaging::put_many_catalogs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_catalog_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_catalog_request& v);
    static ores::dq::messaging::delete_catalog_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_catalog_request& v);
    static ores::dq::messaging::delete_catalog_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_catalog_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_catalog_response& v);
    static ores::dq::messaging::delete_catalog_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_catalog_response& v);
    static ores::dq::messaging::delete_catalog_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_catalogs_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_many_catalogs_request& v);
    static ores::dq::messaging::delete_many_catalogs_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_many_catalogs_request& v);
    static ores::dq::messaging::delete_many_catalogs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_catalogs_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_many_catalogs_response& v);
    static ores::dq::messaging::delete_many_catalogs_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_many_catalogs_response& v);
    static ores::dq::messaging::delete_many_catalogs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_catalog_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_catalog_versions_request& v);
    static ores::dq::messaging::list_catalog_versions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_catalog_versions_request& v);
    static ores::dq::messaging::list_catalog_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_catalog_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_catalog_versions_response& v);
    static ores::dq::messaging::list_catalog_versions_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_catalog_versions_response& v);
    static ores::dq::messaging::list_catalog_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_catalog_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_catalog_version_request& v);
    static ores::dq::messaging::get_catalog_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_catalog_version_request& v);
    static ores::dq::messaging::get_catalog_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_catalog_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_catalog_version_response& v);
    static ores::dq::messaging::get_catalog_version_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_catalog_version_response& v);
    static ores::dq::messaging::get_catalog_version_response
    read_msgpack(std::span<const std::byte> data);
};
}

#endif
