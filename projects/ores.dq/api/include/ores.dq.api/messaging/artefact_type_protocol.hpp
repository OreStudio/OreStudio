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
#ifndef ORES_DQ_API_MESSAGING_ARTEFACT_TYPE_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_ARTEFACT_TYPE_PROTOCOL_HPP

#include "ores.dq.api/domain/artefact_type.hpp"
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

struct artefact_type_key {
    std::string code;
};

struct artefact_type_write {
    std::string code;
    std::string name;
    std::optional<std::string> description;
    std::optional<std::string> artefact_table;
    std::optional<std::string> target_table;
    std::optional<std::string> target_subject;
    int display_order;
};

struct artefact_type_change {
    artefact_type_write write;
    ores::utility::domain::precondition precondition;
};

struct artefact_type_removal {
    artefact_type_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct artefact_type_lookup {
    artefact_type_key key;
    std::optional<ores::dq::domain::artefact_type> artefact_type;
};

struct artefact_types_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct artefact_type_event {
    boost::uuids::uuid event_id;
    artefact_type_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct artefact_type_version_key {
    artefact_type_key artefact_type;
    std::uint32_t version;
};

struct artefact_type_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_artefact_types_request {
    using response_type = struct list_artefact_types_response;
    static constexpr std::string_view nats_subject = "dq.v1.artefact_types.list";
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
    std::optional<artefact_types_filter> filter;
    std::optional<std::string> as_of;
};

struct list_artefact_types_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::artefact_type> types;
    std::uint64_t total;
};

struct get_artefact_type_request {
    using response_type = struct get_artefact_type_response;
    static constexpr std::string_view nats_subject = "dq.v1.artefact_types.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    artefact_type_key key;
};

struct get_artefact_type_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::artefact_type> artefact_type;
};

struct get_many_artefact_types_request {
    using response_type = struct get_many_artefact_types_response;
    static constexpr std::string_view nats_subject = "dq.v1.artefact_types.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<artefact_type_key> keys;
};

struct get_many_artefact_types_response {
    ores::utility::domain::result result;
    std::vector<artefact_type_lookup> entries;
};

struct put_artefact_type_request {
    using response_type = struct put_artefact_type_response;
    static constexpr std::string_view nats_subject = "dq.v1.artefact_types.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    artefact_type_change change;
    ores::utility::domain::change_intent intent;
};

struct put_artefact_type_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::artefact_type> artefact_type;
};

struct put_many_artefact_types_request {
    using response_type = struct put_many_artefact_types_response;
    static constexpr std::string_view nats_subject = "dq.v1.artefact_types.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<artefact_type_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_artefact_types_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::artefact_type> types;
};

struct delete_artefact_type_request {
    using response_type = struct delete_artefact_type_response;
    static constexpr std::string_view nats_subject = "dq.v1.artefact_types.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    artefact_type_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_artefact_type_response {
    ores::utility::domain::result result;
};

struct delete_many_artefact_types_request {
    using response_type = struct delete_many_artefact_types_response;
    static constexpr std::string_view nats_subject = "dq.v1.artefact_types.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<artefact_type_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_artefact_types_response {
    ores::utility::domain::result result;
};

struct list_artefact_type_versions_request {
    using response_type = struct list_artefact_type_versions_response;
    static constexpr std::string_view nats_subject = "dq.v1.artefact_types_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    artefact_type_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<artefact_type_versions_filter> filter;
};

struct list_artefact_type_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::artefact_type> versions;
    std::uint64_t total;
};

struct get_artefact_type_version_request {
    using response_type = struct get_artefact_type_version_response;
    static constexpr std::string_view nats_subject = "dq.v1.artefact_types_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    artefact_type_version_key key;
};

struct get_artefact_type_version_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::artefact_type> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace artefact_type_event_subjects {
inline constexpr std::string_view created = "dq.v1.artefact_types_events.created";
inline constexpr std::string_view updated = "dq.v1.artefact_types_events.updated";
inline constexpr std::string_view deleted = "dq.v1.artefact_types_events.deleted";
}

/**
 * @brief The members of artefact_type_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const artefact_type_key&) {
    return std::tuple{
        utility::codec::field{"code", &artefact_type_key::code},
    };
}

/**
 * @brief The members of artefact_type_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const artefact_type_write&) {
    return std::tuple{
        utility::codec::field{"code", &artefact_type_write::code},
        utility::codec::field{"name", &artefact_type_write::name},
        utility::codec::field{"description", &artefact_type_write::description},
        utility::codec::field{"artefact_table", &artefact_type_write::artefact_table},
        utility::codec::field{"target_table", &artefact_type_write::target_table},
        utility::codec::field{"target_subject", &artefact_type_write::target_subject},
        utility::codec::field{"display_order", &artefact_type_write::display_order},
    };
}

/**
 * @brief The members of artefact_type_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const artefact_type_change&) {
    return std::tuple{
        utility::codec::field{"write", &artefact_type_change::write},
        utility::codec::field{"precondition", &artefact_type_change::precondition},
    };
}

/**
 * @brief The members of artefact_type_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const artefact_type_removal&) {
    return std::tuple{
        utility::codec::field{"key", &artefact_type_removal::key},
        utility::codec::field{"precondition", &artefact_type_removal::precondition},
    };
}

/**
 * @brief The members of artefact_type_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const artefact_type_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &artefact_type_lookup::key},
        utility::codec::field{"artefact_type", &artefact_type_lookup::artefact_type},
    };
}

/**
 * @brief The members of artefact_types_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const artefact_types_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &artefact_types_filter::code_one_of},
    };
}

/**
 * @brief The members of artefact_type_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const artefact_type_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &artefact_type_event::event_id},
        utility::codec::field{"key", &artefact_type_event::key},
        utility::codec::field{"action", &artefact_type_event::action},
        utility::codec::field{"version", &artefact_type_event::version},
        utility::codec::field{"occurred_at", &artefact_type_event::occurred_at},
        utility::codec::field{"correlation_id", &artefact_type_event::correlation_id},
    };
}

/**
 * @brief The members of artefact_type_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const artefact_type_version_key&) {
    return std::tuple{
        utility::codec::field{"artefact_type", &artefact_type_version_key::artefact_type},
        utility::codec::field{"version", &artefact_type_version_key::version},
    };
}

/**
 * @brief The members of artefact_type_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const artefact_type_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &artefact_type_versions_filter::version},
        utility::codec::field{"from_version", &artefact_type_versions_filter::from_version},
        utility::codec::field{"to_version", &artefact_type_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_artefact_types_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_artefact_types_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_artefact_types_request::offset},
        utility::codec::field{"limit", &list_artefact_types_request::limit},
        utility::codec::field{"order", &list_artefact_types_request::order},
        utility::codec::field{"filter", &list_artefact_types_request::filter},
        utility::codec::field{"as_of", &list_artefact_types_request::as_of},
    };
}

/**
 * @brief The members of list_artefact_types_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_artefact_types_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_artefact_types_response::result},
        utility::codec::field{"types", &list_artefact_types_response::types},
        utility::codec::field{"total", &list_artefact_types_response::total},
    };
}

/**
 * @brief The members of get_artefact_type_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_artefact_type_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_artefact_type_request::key},
    };
}

/**
 * @brief The members of get_artefact_type_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_artefact_type_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_artefact_type_response::result},
        utility::codec::field{"artefact_type", &get_artefact_type_response::artefact_type},
    };
}

/**
 * @brief The members of get_many_artefact_types_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_artefact_types_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_artefact_types_request::keys},
    };
}

/**
 * @brief The members of get_many_artefact_types_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_artefact_types_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_artefact_types_response::result},
        utility::codec::field{"entries", &get_many_artefact_types_response::entries},
    };
}

/**
 * @brief The members of put_artefact_type_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_artefact_type_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_artefact_type_request::change},
        utility::codec::field{"intent", &put_artefact_type_request::intent},
    };
}

/**
 * @brief The members of put_artefact_type_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_artefact_type_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_artefact_type_response::result},
        utility::codec::field{"artefact_type", &put_artefact_type_response::artefact_type},
    };
}

/**
 * @brief The members of put_many_artefact_types_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_artefact_types_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_artefact_types_request::changes},
        utility::codec::field{"intent", &put_many_artefact_types_request::intent},
    };
}

/**
 * @brief The members of put_many_artefact_types_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_artefact_types_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_artefact_types_response::result},
        utility::codec::field{"types", &put_many_artefact_types_response::types},
    };
}

/**
 * @brief The members of delete_artefact_type_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_artefact_type_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_artefact_type_request::removal},
        utility::codec::field{"intent", &delete_artefact_type_request::intent},
    };
}

/**
 * @brief The members of delete_artefact_type_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_artefact_type_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_artefact_type_response::result},
    };
}

/**
 * @brief The members of delete_many_artefact_types_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_artefact_types_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_artefact_types_request::removals},
        utility::codec::field{"intent", &delete_many_artefact_types_request::intent},
    };
}

/**
 * @brief The members of delete_many_artefact_types_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_artefact_types_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_artefact_types_response::result},
    };
}

/**
 * @brief The members of list_artefact_type_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_artefact_type_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_artefact_type_versions_request::key},
        utility::codec::field{"offset", &list_artefact_type_versions_request::offset},
        utility::codec::field{"limit", &list_artefact_type_versions_request::limit},
        utility::codec::field{"order", &list_artefact_type_versions_request::order},
        utility::codec::field{"filter", &list_artefact_type_versions_request::filter},
    };
}

/**
 * @brief The members of list_artefact_type_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_artefact_type_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_artefact_type_versions_response::result},
        utility::codec::field{"versions", &list_artefact_type_versions_response::versions},
        utility::codec::field{"total", &list_artefact_type_versions_response::total},
    };
}

/**
 * @brief The members of get_artefact_type_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_artefact_type_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_artefact_type_version_request::key},
    };
}

/**
 * @brief The members of get_artefact_type_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_artefact_type_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_artefact_type_version_response::result},
        utility::codec::field{"version", &get_artefact_type_version_response::version},
    };
}

}

namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::artefact_type_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::artefact_type_key& v);
    static ores::dq::messaging::artefact_type_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::artefact_type_key& v);
    static ores::dq::messaging::artefact_type_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::artefact_type_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::artefact_type_write& v);
    static ores::dq::messaging::artefact_type_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::artefact_type_write& v);
    static ores::dq::messaging::artefact_type_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::artefact_type_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::artefact_type_change& v);
    static ores::dq::messaging::artefact_type_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::artefact_type_change& v);
    static ores::dq::messaging::artefact_type_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::artefact_type_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::artefact_type_removal& v);
    static ores::dq::messaging::artefact_type_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::artefact_type_removal& v);
    static ores::dq::messaging::artefact_type_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::artefact_type_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::artefact_type_lookup& v);
    static ores::dq::messaging::artefact_type_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::artefact_type_lookup& v);
    static ores::dq::messaging::artefact_type_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::artefact_types_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::artefact_types_filter& v);
    static ores::dq::messaging::artefact_types_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::artefact_types_filter& v);
    static ores::dq::messaging::artefact_types_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::artefact_type_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::artefact_type_event& v);
    static ores::dq::messaging::artefact_type_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::artefact_type_event& v);
    static ores::dq::messaging::artefact_type_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::artefact_type_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::artefact_type_version_key& v);
    static ores::dq::messaging::artefact_type_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::artefact_type_version_key& v);
    static ores::dq::messaging::artefact_type_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::artefact_type_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::artefact_type_versions_filter& v);
    static ores::dq::messaging::artefact_type_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::artefact_type_versions_filter& v);
    static ores::dq::messaging::artefact_type_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_artefact_types_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_artefact_types_request& v);
    static ores::dq::messaging::list_artefact_types_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_artefact_types_request& v);
    static ores::dq::messaging::list_artefact_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_artefact_types_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_artefact_types_response& v);
    static ores::dq::messaging::list_artefact_types_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_artefact_types_response& v);
    static ores::dq::messaging::list_artefact_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_artefact_type_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_artefact_type_request& v);
    static ores::dq::messaging::get_artefact_type_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_artefact_type_request& v);
    static ores::dq::messaging::get_artefact_type_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_artefact_type_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_artefact_type_response& v);
    static ores::dq::messaging::get_artefact_type_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_artefact_type_response& v);
    static ores::dq::messaging::get_artefact_type_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_artefact_types_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_artefact_types_request& v);
    static ores::dq::messaging::get_many_artefact_types_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_many_artefact_types_request& v);
    static ores::dq::messaging::get_many_artefact_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_artefact_types_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_artefact_types_response& v);
    static ores::dq::messaging::get_many_artefact_types_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_many_artefact_types_response& v);
    static ores::dq::messaging::get_many_artefact_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_artefact_type_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_artefact_type_request& v);
    static ores::dq::messaging::put_artefact_type_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_artefact_type_request& v);
    static ores::dq::messaging::put_artefact_type_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_artefact_type_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_artefact_type_response& v);
    static ores::dq::messaging::put_artefact_type_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_artefact_type_response& v);
    static ores::dq::messaging::put_artefact_type_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_artefact_types_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_artefact_types_request& v);
    static ores::dq::messaging::put_many_artefact_types_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_many_artefact_types_request& v);
    static ores::dq::messaging::put_many_artefact_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_artefact_types_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_artefact_types_response& v);
    static ores::dq::messaging::put_many_artefact_types_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::put_many_artefact_types_response& v);
    static ores::dq::messaging::put_many_artefact_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_artefact_type_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_artefact_type_request& v);
    static ores::dq::messaging::delete_artefact_type_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_artefact_type_request& v);
    static ores::dq::messaging::delete_artefact_type_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_artefact_type_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_artefact_type_response& v);
    static ores::dq::messaging::delete_artefact_type_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_artefact_type_response& v);
    static ores::dq::messaging::delete_artefact_type_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_artefact_types_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_many_artefact_types_request& v);
    static ores::dq::messaging::delete_many_artefact_types_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_artefact_types_request& v);
    static ores::dq::messaging::delete_many_artefact_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_artefact_types_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::delete_many_artefact_types_response& v);
    static ores::dq::messaging::delete_many_artefact_types_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_artefact_types_response& v);
    static ores::dq::messaging::delete_many_artefact_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_artefact_type_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_artefact_type_versions_request& v);
    static ores::dq::messaging::list_artefact_type_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_artefact_type_versions_request& v);
    static ores::dq::messaging::list_artefact_type_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_artefact_type_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_artefact_type_versions_response& v);
    static ores::dq::messaging::list_artefact_type_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_artefact_type_versions_response& v);
    static ores::dq::messaging::list_artefact_type_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_artefact_type_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_artefact_type_version_request& v);
    static ores::dq::messaging::get_artefact_type_version_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_artefact_type_version_request& v);
    static ores::dq::messaging::get_artefact_type_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_artefact_type_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_artefact_type_version_response& v);
    static ores::dq::messaging::get_artefact_type_version_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_artefact_type_version_response& v);
    static ores::dq::messaging::get_artefact_type_version_response
    read_msgpack(std::span<const std::byte> data);
};
}

#endif
