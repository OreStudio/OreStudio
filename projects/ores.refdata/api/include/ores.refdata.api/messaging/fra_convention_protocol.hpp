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
#ifndef ORES_REFDATA_API_MESSAGING_FRA_CONVENTION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_FRA_CONVENTION_PROTOCOL_HPP

#include "ores.refdata.api/domain/fra_convention.hpp"
#include "ores.refdata.api/export.hpp"
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

namespace ores::refdata::messaging {

struct fra_convention_key {
    std::string id;
};

struct fra_convention_write {
    std::string id;
    std::string index;
    std::optional<std::string> oresmd_uri;
};

struct fra_convention_change {
    fra_convention_write write;
    ores::utility::domain::precondition precondition;
};

struct fra_convention_removal {
    fra_convention_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct fra_convention_lookup {
    fra_convention_key key;
    std::optional<ores::refdata::domain::fra_convention> fra_convention;
};

struct fra_conventions_filter {
    std::optional<std::vector<std::string>> id_one_of;
};

struct fra_convention_event {
    boost::uuids::uuid event_id;
    fra_convention_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct fra_convention_version_key {
    fra_convention_key fra_convention;
    std::uint32_t version;
};

struct fra_convention_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_fra_conventions_request {
    using response_type = struct list_fra_conventions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fra_conventions.list";
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
    std::optional<fra_conventions_filter> filter;
    std::optional<std::string> as_of;
};

struct list_fra_conventions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::fra_convention> fra_conventions;
    std::uint64_t total;
};

struct get_fra_convention_request {
    using response_type = struct get_fra_convention_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fra_conventions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fra_convention_key key;
};

struct get_fra_convention_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::fra_convention> fra_convention;
};

struct get_many_fra_conventions_request {
    using response_type = struct get_many_fra_conventions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fra_conventions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fra_convention_key> keys;
};

struct get_many_fra_conventions_response {
    ores::utility::domain::result result;
    std::vector<fra_convention_lookup> entries;
};

struct put_fra_convention_request {
    using response_type = struct put_fra_convention_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fra_conventions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fra_convention_change change;
    ores::utility::domain::change_intent intent;
};

struct put_fra_convention_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::fra_convention> fra_convention;
};

struct put_many_fra_conventions_request {
    using response_type = struct put_many_fra_conventions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fra_conventions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fra_convention_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_fra_conventions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::fra_convention> fra_conventions;
};

struct delete_fra_convention_request {
    using response_type = struct delete_fra_convention_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fra_conventions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fra_convention_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_fra_convention_response {
    ores::utility::domain::result result;
};

struct delete_many_fra_conventions_request {
    using response_type = struct delete_many_fra_conventions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fra_conventions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fra_convention_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_fra_conventions_response {
    ores::utility::domain::result result;
};

struct list_fra_convention_versions_request {
    using response_type = struct list_fra_convention_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fra_conventions_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fra_convention_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<fra_convention_versions_filter> filter;
};

struct list_fra_convention_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::fra_convention> versions;
    std::uint64_t total;
};

struct get_fra_convention_version_request {
    using response_type = struct get_fra_convention_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fra_conventions_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fra_convention_version_key key;
};

struct get_fra_convention_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::fra_convention> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace fra_convention_event_subjects {
inline constexpr std::string_view created = "refdata.v1.fra_conventions_events.created";
inline constexpr std::string_view updated = "refdata.v1.fra_conventions_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.fra_conventions_events.deleted";
}
/**
 * @brief The members of fra_convention_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fra_convention_key&) {
    return std::tuple{
        utility::codec::field{"id", &fra_convention_key::id},
    };
}

/**
 * @brief The members of fra_convention_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fra_convention_write&) {
    return std::tuple{
        utility::codec::field{"id", &fra_convention_write::id},
        utility::codec::field{"index", &fra_convention_write::index},
        utility::codec::field{"oresmd_uri", &fra_convention_write::oresmd_uri},
    };
}

/**
 * @brief The members of fra_convention_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fra_convention_change&) {
    return std::tuple{
        utility::codec::field{"write", &fra_convention_change::write},
        utility::codec::field{"precondition", &fra_convention_change::precondition},
    };
}

/**
 * @brief The members of fra_convention_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fra_convention_removal&) {
    return std::tuple{
        utility::codec::field{"key", &fra_convention_removal::key},
        utility::codec::field{"precondition", &fra_convention_removal::precondition},
    };
}

/**
 * @brief The members of fra_convention_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fra_convention_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &fra_convention_lookup::key},
        utility::codec::field{"fra_convention", &fra_convention_lookup::fra_convention},
    };
}

/**
 * @brief The members of fra_conventions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fra_conventions_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &fra_conventions_filter::id_one_of},
    };
}

/**
 * @brief The members of fra_convention_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fra_convention_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &fra_convention_event::event_id},
        utility::codec::field{"key", &fra_convention_event::key},
        utility::codec::field{"action", &fra_convention_event::action},
        utility::codec::field{"version", &fra_convention_event::version},
        utility::codec::field{"occurred_at", &fra_convention_event::occurred_at},
        utility::codec::field{"correlation_id", &fra_convention_event::correlation_id},
    };
}

/**
 * @brief The members of fra_convention_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fra_convention_version_key&) {
    return std::tuple{
        utility::codec::field{"fra_convention", &fra_convention_version_key::fra_convention},
        utility::codec::field{"version", &fra_convention_version_key::version},
    };
}

/**
 * @brief The members of fra_convention_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fra_convention_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &fra_convention_versions_filter::version},
        utility::codec::field{"from_version", &fra_convention_versions_filter::from_version},
        utility::codec::field{"to_version", &fra_convention_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_fra_conventions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_fra_conventions_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_fra_conventions_request::offset},
        utility::codec::field{"limit", &list_fra_conventions_request::limit},
        utility::codec::field{"order", &list_fra_conventions_request::order},
        utility::codec::field{"filter", &list_fra_conventions_request::filter},
        utility::codec::field{"as_of", &list_fra_conventions_request::as_of},
    };
}

/**
 * @brief The members of list_fra_conventions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_fra_conventions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_fra_conventions_response::result},
        utility::codec::field{"fra_conventions", &list_fra_conventions_response::fra_conventions},
        utility::codec::field{"total", &list_fra_conventions_response::total},
    };
}

/**
 * @brief The members of get_fra_convention_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_fra_convention_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_fra_convention_request::key},
    };
}

/**
 * @brief The members of get_fra_convention_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_fra_convention_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_fra_convention_response::result},
        utility::codec::field{"fra_convention", &get_fra_convention_response::fra_convention},
    };
}

/**
 * @brief The members of get_many_fra_conventions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_fra_conventions_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_fra_conventions_request::keys},
    };
}

/**
 * @brief The members of get_many_fra_conventions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_fra_conventions_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_fra_conventions_response::result},
        utility::codec::field{"entries", &get_many_fra_conventions_response::entries},
    };
}

/**
 * @brief The members of put_fra_convention_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_fra_convention_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_fra_convention_request::change},
        utility::codec::field{"intent", &put_fra_convention_request::intent},
    };
}

/**
 * @brief The members of put_fra_convention_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_fra_convention_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_fra_convention_response::result},
        utility::codec::field{"fra_convention", &put_fra_convention_response::fra_convention},
    };
}

/**
 * @brief The members of put_many_fra_conventions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_fra_conventions_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_fra_conventions_request::changes},
        utility::codec::field{"intent", &put_many_fra_conventions_request::intent},
    };
}

/**
 * @brief The members of put_many_fra_conventions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_fra_conventions_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_fra_conventions_response::result},
        utility::codec::field{"fra_conventions",
                              &put_many_fra_conventions_response::fra_conventions},
    };
}

/**
 * @brief The members of delete_fra_convention_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_fra_convention_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_fra_convention_request::removal},
        utility::codec::field{"intent", &delete_fra_convention_request::intent},
    };
}

/**
 * @brief The members of delete_fra_convention_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_fra_convention_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_fra_convention_response::result},
    };
}

/**
 * @brief The members of delete_many_fra_conventions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_fra_conventions_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_fra_conventions_request::removals},
        utility::codec::field{"intent", &delete_many_fra_conventions_request::intent},
    };
}

/**
 * @brief The members of delete_many_fra_conventions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_fra_conventions_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_fra_conventions_response::result},
    };
}

/**
 * @brief The members of list_fra_convention_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_fra_convention_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_fra_convention_versions_request::key},
        utility::codec::field{"offset", &list_fra_convention_versions_request::offset},
        utility::codec::field{"limit", &list_fra_convention_versions_request::limit},
        utility::codec::field{"order", &list_fra_convention_versions_request::order},
        utility::codec::field{"filter", &list_fra_convention_versions_request::filter},
    };
}

/**
 * @brief The members of list_fra_convention_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_fra_convention_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_fra_convention_versions_response::result},
        utility::codec::field{"versions", &list_fra_convention_versions_response::versions},
        utility::codec::field{"total", &list_fra_convention_versions_response::total},
    };
}

/**
 * @brief The members of get_fra_convention_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_fra_convention_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_fra_convention_version_request::key},
    };
}

/**
 * @brief The members of get_fra_convention_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_fra_convention_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_fra_convention_version_response::result},
        utility::codec::field{"version", &get_fra_convention_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fra_convention_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fra_convention_key& v);
    static ores::refdata::messaging::fra_convention_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::fra_convention_key& v);
    static ores::refdata::messaging::fra_convention_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fra_convention_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fra_convention_write& v);
    static ores::refdata::messaging::fra_convention_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::fra_convention_write& v);
    static ores::refdata::messaging::fra_convention_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fra_convention_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fra_convention_change& v);
    static ores::refdata::messaging::fra_convention_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::fra_convention_change& v);
    static ores::refdata::messaging::fra_convention_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fra_convention_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fra_convention_removal& v);
    static ores::refdata::messaging::fra_convention_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::fra_convention_removal& v);
    static ores::refdata::messaging::fra_convention_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fra_convention_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fra_convention_lookup& v);
    static ores::refdata::messaging::fra_convention_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::fra_convention_lookup& v);
    static ores::refdata::messaging::fra_convention_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fra_conventions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fra_conventions_filter& v);
    static ores::refdata::messaging::fra_conventions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::fra_conventions_filter& v);
    static ores::refdata::messaging::fra_conventions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fra_convention_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fra_convention_event& v);
    static ores::refdata::messaging::fra_convention_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::fra_convention_event& v);
    static ores::refdata::messaging::fra_convention_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fra_convention_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fra_convention_version_key& v);
    static ores::refdata::messaging::fra_convention_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::fra_convention_version_key& v);
    static ores::refdata::messaging::fra_convention_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::fra_convention_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::fra_convention_versions_filter& v);
    static ores::refdata::messaging::fra_convention_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::fra_convention_versions_filter& v);
    static ores::refdata::messaging::fra_convention_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_fra_conventions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_fra_conventions_request& v);
    static ores::refdata::messaging::list_fra_conventions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_fra_conventions_request& v);
    static ores::refdata::messaging::list_fra_conventions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_fra_conventions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_fra_conventions_response& v);
    static ores::refdata::messaging::list_fra_conventions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_fra_conventions_response& v);
    static ores::refdata::messaging::list_fra_conventions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_fra_convention_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_fra_convention_request& v);
    static ores::refdata::messaging::get_fra_convention_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_fra_convention_request& v);
    static ores::refdata::messaging::get_fra_convention_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_fra_convention_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_fra_convention_response& v);
    static ores::refdata::messaging::get_fra_convention_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_fra_convention_response& v);
    static ores::refdata::messaging::get_fra_convention_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_fra_conventions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_fra_conventions_request& v);
    static ores::refdata::messaging::get_many_fra_conventions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_fra_conventions_request& v);
    static ores::refdata::messaging::get_many_fra_conventions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_fra_conventions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_fra_conventions_response& v);
    static ores::refdata::messaging::get_many_fra_conventions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_fra_conventions_response& v);
    static ores::refdata::messaging::get_many_fra_conventions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_fra_convention_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_fra_convention_request& v);
    static ores::refdata::messaging::put_fra_convention_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_fra_convention_request& v);
    static ores::refdata::messaging::put_fra_convention_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_fra_convention_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_fra_convention_response& v);
    static ores::refdata::messaging::put_fra_convention_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_fra_convention_response& v);
    static ores::refdata::messaging::put_fra_convention_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_fra_conventions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_fra_conventions_request& v);
    static ores::refdata::messaging::put_many_fra_conventions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_fra_conventions_request& v);
    static ores::refdata::messaging::put_many_fra_conventions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_fra_conventions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_fra_conventions_response& v);
    static ores::refdata::messaging::put_many_fra_conventions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_fra_conventions_response& v);
    static ores::refdata::messaging::put_many_fra_conventions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_fra_convention_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_fra_convention_request& v);
    static ores::refdata::messaging::delete_fra_convention_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_fra_convention_request& v);
    static ores::refdata::messaging::delete_fra_convention_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_fra_convention_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_fra_convention_response& v);
    static ores::refdata::messaging::delete_fra_convention_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_fra_convention_response& v);
    static ores::refdata::messaging::delete_fra_convention_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_fra_conventions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_fra_conventions_request& v);
    static ores::refdata::messaging::delete_many_fra_conventions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_fra_conventions_request& v);
    static ores::refdata::messaging::delete_many_fra_conventions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_fra_conventions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_fra_conventions_response& v);
    static ores::refdata::messaging::delete_many_fra_conventions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_fra_conventions_response& v);
    static ores::refdata::messaging::delete_many_fra_conventions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_fra_convention_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_fra_convention_versions_request& v);
    static ores::refdata::messaging::list_fra_convention_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_fra_convention_versions_request& v);
    static ores::refdata::messaging::list_fra_convention_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_fra_convention_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_fra_convention_versions_response& v);
    static ores::refdata::messaging::list_fra_convention_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_fra_convention_versions_response& v);
    static ores::refdata::messaging::list_fra_convention_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_fra_convention_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_fra_convention_version_request& v);
    static ores::refdata::messaging::get_fra_convention_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_fra_convention_version_request& v);
    static ores::refdata::messaging::get_fra_convention_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_fra_convention_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_fra_convention_version_response& v);
    static ores::refdata::messaging::get_fra_convention_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_fra_convention_version_response& v);
    static ores::refdata::messaging::get_fra_convention_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
