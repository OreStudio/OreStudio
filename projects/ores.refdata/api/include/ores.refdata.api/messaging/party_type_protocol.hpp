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
#ifndef ORES_REFDATA_API_MESSAGING_PARTY_TYPE_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_PARTY_TYPE_PROTOCOL_HPP

#include "ores.refdata.api/domain/party_type.hpp"
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

struct party_type_key {
    std::string code;
};

struct party_type_write {
    std::string code;
    std::string name;
    std::string description;
    int display_order;
};

struct party_type_change {
    party_type_write write;
    ores::utility::domain::precondition precondition;
};

struct party_type_removal {
    party_type_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct party_type_lookup {
    party_type_key key;
    std::optional<ores::refdata::domain::party_type> party_type;
};

struct party_types_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct party_type_event {
    boost::uuids::uuid event_id;
    party_type_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct party_type_version_key {
    party_type_key party_type;
    std::uint32_t version;
};

struct party_type_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_party_types_request {
    using response_type = struct list_party_types_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_types.list";
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
    std::optional<party_types_filter> filter;
    std::optional<std::string> as_of;
};

struct list_party_types_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::party_type> types;
    std::uint64_t total;
};

struct get_party_type_request {
    using response_type = struct get_party_type_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_types.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_type_key key;
};

struct get_party_type_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::party_type> party_type;
};

struct get_many_party_types_request {
    using response_type = struct get_many_party_types_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_types.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<party_type_key> keys;
};

struct get_many_party_types_response {
    ores::utility::domain::result result;
    std::vector<party_type_lookup> entries;
};

struct put_party_type_request {
    using response_type = struct put_party_type_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_types.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_type_change change;
    ores::utility::domain::change_intent intent;
};

struct put_party_type_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::party_type> party_type;
};

struct put_many_party_types_request {
    using response_type = struct put_many_party_types_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_types.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<party_type_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_party_types_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::party_type> types;
};

struct delete_party_type_request {
    using response_type = struct delete_party_type_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_types.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_type_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_party_type_response {
    ores::utility::domain::result result;
};

struct delete_many_party_types_request {
    using response_type = struct delete_many_party_types_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_types.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<party_type_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_party_types_response {
    ores::utility::domain::result result;
};

struct list_party_type_versions_request {
    using response_type = struct list_party_type_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_types_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_type_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<party_type_versions_filter> filter;
};

struct list_party_type_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::party_type> versions;
    std::uint64_t total;
};

struct get_party_type_version_request {
    using response_type = struct get_party_type_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_types_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_type_version_key key;
};

struct get_party_type_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::party_type> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace party_type_event_subjects {
inline constexpr std::string_view created = "refdata.v1.party_types_events.created";
inline constexpr std::string_view updated = "refdata.v1.party_types_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.party_types_events.deleted";
}
/**
 * @brief The members of party_type_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_type_key&) {
    return std::tuple{
        utility::codec::field{"code", &party_type_key::code},
    };
}

/**
 * @brief The members of party_type_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_type_write&) {
    return std::tuple{
        utility::codec::field{"code", &party_type_write::code},
        utility::codec::field{"name", &party_type_write::name},
        utility::codec::field{"description", &party_type_write::description},
        utility::codec::field{"display_order", &party_type_write::display_order},
    };
}

/**
 * @brief The members of party_type_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_type_change&) {
    return std::tuple{
        utility::codec::field{"write", &party_type_change::write},
        utility::codec::field{"precondition", &party_type_change::precondition},
    };
}

/**
 * @brief The members of party_type_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_type_removal&) {
    return std::tuple{
        utility::codec::field{"key", &party_type_removal::key},
        utility::codec::field{"precondition", &party_type_removal::precondition},
    };
}

/**
 * @brief The members of party_type_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_type_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &party_type_lookup::key},
        utility::codec::field{"party_type", &party_type_lookup::party_type},
    };
}

/**
 * @brief The members of party_types_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_types_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &party_types_filter::code_one_of},
    };
}

/**
 * @brief The members of party_type_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_type_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &party_type_event::event_id},
        utility::codec::field{"key", &party_type_event::key},
        utility::codec::field{"action", &party_type_event::action},
        utility::codec::field{"version", &party_type_event::version},
        utility::codec::field{"occurred_at", &party_type_event::occurred_at},
        utility::codec::field{"correlation_id", &party_type_event::correlation_id},
    };
}

/**
 * @brief The members of party_type_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_type_version_key&) {
    return std::tuple{
        utility::codec::field{"party_type", &party_type_version_key::party_type},
        utility::codec::field{"version", &party_type_version_key::version},
    };
}

/**
 * @brief The members of party_type_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_type_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &party_type_versions_filter::version},
        utility::codec::field{"from_version", &party_type_versions_filter::from_version},
        utility::codec::field{"to_version", &party_type_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_party_types_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_party_types_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_party_types_request::offset},
        utility::codec::field{"limit", &list_party_types_request::limit},
        utility::codec::field{"order", &list_party_types_request::order},
        utility::codec::field{"filter", &list_party_types_request::filter},
        utility::codec::field{"as_of", &list_party_types_request::as_of},
    };
}

/**
 * @brief The members of list_party_types_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_party_types_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_party_types_response::result},
        utility::codec::field{"types", &list_party_types_response::types},
        utility::codec::field{"total", &list_party_types_response::total},
    };
}

/**
 * @brief The members of get_party_type_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_party_type_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_party_type_request::key},
    };
}

/**
 * @brief The members of get_party_type_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_party_type_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_party_type_response::result},
        utility::codec::field{"party_type", &get_party_type_response::party_type},
    };
}

/**
 * @brief The members of get_many_party_types_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_party_types_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_party_types_request::keys},
    };
}

/**
 * @brief The members of get_many_party_types_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_party_types_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_party_types_response::result},
        utility::codec::field{"entries", &get_many_party_types_response::entries},
    };
}

/**
 * @brief The members of put_party_type_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_party_type_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_party_type_request::change},
        utility::codec::field{"intent", &put_party_type_request::intent},
    };
}

/**
 * @brief The members of put_party_type_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_party_type_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_party_type_response::result},
        utility::codec::field{"party_type", &put_party_type_response::party_type},
    };
}

/**
 * @brief The members of put_many_party_types_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_party_types_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_party_types_request::changes},
        utility::codec::field{"intent", &put_many_party_types_request::intent},
    };
}

/**
 * @brief The members of put_many_party_types_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_party_types_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_party_types_response::result},
        utility::codec::field{"types", &put_many_party_types_response::types},
    };
}

/**
 * @brief The members of delete_party_type_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_party_type_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_party_type_request::removal},
        utility::codec::field{"intent", &delete_party_type_request::intent},
    };
}

/**
 * @brief The members of delete_party_type_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_party_type_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_party_type_response::result},
    };
}

/**
 * @brief The members of delete_many_party_types_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_party_types_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_party_types_request::removals},
        utility::codec::field{"intent", &delete_many_party_types_request::intent},
    };
}

/**
 * @brief The members of delete_many_party_types_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_party_types_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_party_types_response::result},
    };
}

/**
 * @brief The members of list_party_type_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_party_type_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_party_type_versions_request::key},
        utility::codec::field{"offset", &list_party_type_versions_request::offset},
        utility::codec::field{"limit", &list_party_type_versions_request::limit},
        utility::codec::field{"order", &list_party_type_versions_request::order},
        utility::codec::field{"filter", &list_party_type_versions_request::filter},
    };
}

/**
 * @brief The members of list_party_type_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_party_type_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_party_type_versions_response::result},
        utility::codec::field{"versions", &list_party_type_versions_response::versions},
        utility::codec::field{"total", &list_party_type_versions_response::total},
    };
}

/**
 * @brief The members of get_party_type_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_party_type_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_party_type_version_request::key},
    };
}

/**
 * @brief The members of get_party_type_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_party_type_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_party_type_version_response::result},
        utility::codec::field{"version", &get_party_type_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_type_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_type_key& v);
    static ores::refdata::messaging::party_type_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_type_key& v);
    static ores::refdata::messaging::party_type_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_type_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_type_write& v);
    static ores::refdata::messaging::party_type_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_type_write& v);
    static ores::refdata::messaging::party_type_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_type_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_type_change& v);
    static ores::refdata::messaging::party_type_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_type_change& v);
    static ores::refdata::messaging::party_type_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_type_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_type_removal& v);
    static ores::refdata::messaging::party_type_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_type_removal& v);
    static ores::refdata::messaging::party_type_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_type_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_type_lookup& v);
    static ores::refdata::messaging::party_type_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_type_lookup& v);
    static ores::refdata::messaging::party_type_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_types_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_types_filter& v);
    static ores::refdata::messaging::party_types_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_types_filter& v);
    static ores::refdata::messaging::party_types_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_type_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_type_event& v);
    static ores::refdata::messaging::party_type_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_type_event& v);
    static ores::refdata::messaging::party_type_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_type_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_type_version_key& v);
    static ores::refdata::messaging::party_type_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_type_version_key& v);
    static ores::refdata::messaging::party_type_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_type_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_type_versions_filter& v);
    static ores::refdata::messaging::party_type_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_type_versions_filter& v);
    static ores::refdata::messaging::party_type_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_party_types_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_party_types_request& v);
    static ores::refdata::messaging::list_party_types_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_party_types_request& v);
    static ores::refdata::messaging::list_party_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_party_types_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_party_types_response& v);
    static ores::refdata::messaging::list_party_types_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_party_types_response& v);
    static ores::refdata::messaging::list_party_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_party_type_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_party_type_request& v);
    static ores::refdata::messaging::get_party_type_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_party_type_request& v);
    static ores::refdata::messaging::get_party_type_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_party_type_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_party_type_response& v);
    static ores::refdata::messaging::get_party_type_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_party_type_response& v);
    static ores::refdata::messaging::get_party_type_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_many_party_types_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_many_party_types_request& v);
    static ores::refdata::messaging::get_many_party_types_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_party_types_request& v);
    static ores::refdata::messaging::get_many_party_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_party_types_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_many_party_types_response& v);
    static ores::refdata::messaging::get_many_party_types_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_party_types_response& v);
    static ores::refdata::messaging::get_many_party_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_party_type_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_party_type_request& v);
    static ores::refdata::messaging::put_party_type_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_party_type_request& v);
    static ores::refdata::messaging::put_party_type_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_party_type_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_party_type_response& v);
    static ores::refdata::messaging::put_party_type_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_party_type_response& v);
    static ores::refdata::messaging::put_party_type_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_many_party_types_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_many_party_types_request& v);
    static ores::refdata::messaging::put_many_party_types_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_party_types_request& v);
    static ores::refdata::messaging::put_many_party_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_party_types_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_many_party_types_response& v);
    static ores::refdata::messaging::put_many_party_types_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_party_types_response& v);
    static ores::refdata::messaging::put_many_party_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_party_type_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_party_type_request& v);
    static ores::refdata::messaging::delete_party_type_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_party_type_request& v);
    static ores::refdata::messaging::delete_party_type_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_party_type_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_party_type_response& v);
    static ores::refdata::messaging::delete_party_type_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_party_type_response& v);
    static ores::refdata::messaging::delete_party_type_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_party_types_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_party_types_request& v);
    static ores::refdata::messaging::delete_many_party_types_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_party_types_request& v);
    static ores::refdata::messaging::delete_many_party_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_party_types_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_party_types_response& v);
    static ores::refdata::messaging::delete_many_party_types_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_party_types_response& v);
    static ores::refdata::messaging::delete_many_party_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_party_type_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_party_type_versions_request& v);
    static ores::refdata::messaging::list_party_type_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_party_type_versions_request& v);
    static ores::refdata::messaging::list_party_type_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_party_type_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_party_type_versions_response& v);
    static ores::refdata::messaging::list_party_type_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_party_type_versions_response& v);
    static ores::refdata::messaging::list_party_type_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_party_type_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_party_type_version_request& v);
    static ores::refdata::messaging::get_party_type_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_party_type_version_request& v);
    static ores::refdata::messaging::get_party_type_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_party_type_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_party_type_version_response& v);
    static ores::refdata::messaging::get_party_type_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_party_type_version_response& v);
    static ores::refdata::messaging::get_party_type_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
