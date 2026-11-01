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
#ifndef ORES_TRADING_API_MESSAGING_STRUCTURE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_STRUCTURE_PROTOCOL_HPP

#include "ores.trading.api/domain/structure.hpp"
#include "ores.trading.api/export.hpp"
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

namespace ores::trading::messaging {

struct structure_key {
    boost::uuids::uuid id;
};

struct structure_write {
    boost::uuids::uuid id;
    boost::uuids::uuid counterparty_id;
    std::string kind;
    std::string template_code;
    boost::uuids::uuid parent_structure_id;
};

struct structure_change {
    structure_write write;
    ores::utility::domain::precondition precondition;
};

struct structure_removal {
    structure_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct structure_lookup {
    structure_key key;
    std::optional<ores::trading::domain::structure> structure;
};

struct structures_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct structure_event {
    boost::uuids::uuid event_id;
    structure_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct structure_version_key {
    structure_key structure;
    std::uint32_t version;
};

struct structure_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_structures_request {
    using response_type = struct list_structures_response;
    static constexpr std::string_view nats_subject = "trading.v1.structures.list";
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
    std::optional<structures_filter> filter;
    std::optional<std::string> as_of;
};

struct list_structures_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::structure> structures;
    std::uint64_t total;
};

struct get_structure_request {
    using response_type = struct get_structure_response;
    static constexpr std::string_view nats_subject = "trading.v1.structures.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_key key;
};

struct get_structure_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::structure> structure;
};

struct get_many_structures_request {
    using response_type = struct get_many_structures_response;
    static constexpr std::string_view nats_subject = "trading.v1.structures.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<structure_key> keys;
};

struct get_many_structures_response {
    ores::utility::domain::result result;
    std::vector<structure_lookup> entries;
};

struct put_structure_request {
    using response_type = struct put_structure_response;
    static constexpr std::string_view nats_subject = "trading.v1.structures.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_change change;
    ores::utility::domain::change_intent intent;
};

struct put_structure_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::structure> structure;
};

struct put_many_structures_request {
    using response_type = struct put_many_structures_response;
    static constexpr std::string_view nats_subject = "trading.v1.structures.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<structure_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_structures_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::structure> structures;
};

struct delete_structure_request {
    using response_type = struct delete_structure_response;
    static constexpr std::string_view nats_subject = "trading.v1.structures.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_structure_response {
    ores::utility::domain::result result;
};

struct delete_many_structures_request {
    using response_type = struct delete_many_structures_response;
    static constexpr std::string_view nats_subject = "trading.v1.structures.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<structure_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_structures_response {
    ores::utility::domain::result result;
};

struct list_structure_versions_request {
    using response_type = struct list_structure_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.structures_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<structure_versions_filter> filter;
};

struct list_structure_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::structure> versions;
    std::uint64_t total;
};

struct get_structure_version_request {
    using response_type = struct get_structure_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.structures_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_version_key key;
};

struct get_structure_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::structure> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace structure_event_subjects {
inline constexpr std::string_view created = "trading.v1.structures_events.created";
inline constexpr std::string_view updated = "trading.v1.structures_events.updated";
inline constexpr std::string_view deleted = "trading.v1.structures_events.deleted";
}
/**
 * @brief The members of structure_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const structure_key&) {
    return std::tuple{
        utility::codec::field{"id", &structure_key::id},
    };
}

/**
 * @brief The members of structure_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const structure_write&) {
    return std::tuple{
        utility::codec::field{"id", &structure_write::id},
        utility::codec::field{"counterparty_id", &structure_write::counterparty_id},
        utility::codec::field{"kind", &structure_write::kind},
        utility::codec::field{"template_code", &structure_write::template_code},
        utility::codec::field{"parent_structure_id", &structure_write::parent_structure_id},
    };
}

/**
 * @brief The members of structure_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const structure_change&) {
    return std::tuple{
        utility::codec::field{"write", &structure_change::write},
        utility::codec::field{"precondition", &structure_change::precondition},
    };
}

/**
 * @brief The members of structure_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const structure_removal&) {
    return std::tuple{
        utility::codec::field{"key", &structure_removal::key},
        utility::codec::field{"precondition", &structure_removal::precondition},
    };
}

/**
 * @brief The members of structure_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const structure_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &structure_lookup::key},
        utility::codec::field{"structure", &structure_lookup::structure},
    };
}

/**
 * @brief The members of structures_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const structures_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &structures_filter::id_one_of},
    };
}

/**
 * @brief The members of structure_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const structure_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &structure_event::event_id},
        utility::codec::field{"key", &structure_event::key},
        utility::codec::field{"action", &structure_event::action},
        utility::codec::field{"version", &structure_event::version},
        utility::codec::field{"occurred_at", &structure_event::occurred_at},
        utility::codec::field{"correlation_id", &structure_event::correlation_id},
    };
}

/**
 * @brief The members of structure_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const structure_version_key&) {
    return std::tuple{
        utility::codec::field{"structure", &structure_version_key::structure},
        utility::codec::field{"version", &structure_version_key::version},
    };
}

/**
 * @brief The members of structure_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const structure_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &structure_versions_filter::version},
        utility::codec::field{"from_version", &structure_versions_filter::from_version},
        utility::codec::field{"to_version", &structure_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_structures_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_structures_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_structures_request::offset},
        utility::codec::field{"limit", &list_structures_request::limit},
        utility::codec::field{"order", &list_structures_request::order},
        utility::codec::field{"filter", &list_structures_request::filter},
        utility::codec::field{"as_of", &list_structures_request::as_of},
    };
}

/**
 * @brief The members of list_structures_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_structures_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_structures_response::result},
        utility::codec::field{"structures", &list_structures_response::structures},
        utility::codec::field{"total", &list_structures_response::total},
    };
}

/**
 * @brief The members of get_structure_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_structure_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_structure_request::key},
    };
}

/**
 * @brief The members of get_structure_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_structure_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_structure_response::result},
        utility::codec::field{"structure", &get_structure_response::structure},
    };
}

/**
 * @brief The members of get_many_structures_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_structures_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_structures_request::keys},
    };
}

/**
 * @brief The members of get_many_structures_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_structures_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_structures_response::result},
        utility::codec::field{"entries", &get_many_structures_response::entries},
    };
}

/**
 * @brief The members of put_structure_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_structure_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_structure_request::change},
        utility::codec::field{"intent", &put_structure_request::intent},
    };
}

/**
 * @brief The members of put_structure_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_structure_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_structure_response::result},
        utility::codec::field{"structure", &put_structure_response::structure},
    };
}

/**
 * @brief The members of put_many_structures_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_structures_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_structures_request::changes},
        utility::codec::field{"intent", &put_many_structures_request::intent},
    };
}

/**
 * @brief The members of put_many_structures_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_structures_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_structures_response::result},
        utility::codec::field{"structures", &put_many_structures_response::structures},
    };
}

/**
 * @brief The members of delete_structure_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_structure_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_structure_request::removal},
        utility::codec::field{"intent", &delete_structure_request::intent},
    };
}

/**
 * @brief The members of delete_structure_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_structure_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_structure_response::result},
    };
}

/**
 * @brief The members of delete_many_structures_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_structures_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_structures_request::removals},
        utility::codec::field{"intent", &delete_many_structures_request::intent},
    };
}

/**
 * @brief The members of delete_many_structures_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_structures_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_structures_response::result},
    };
}

/**
 * @brief The members of list_structure_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_structure_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_structure_versions_request::key},
        utility::codec::field{"offset", &list_structure_versions_request::offset},
        utility::codec::field{"limit", &list_structure_versions_request::limit},
        utility::codec::field{"order", &list_structure_versions_request::order},
        utility::codec::field{"filter", &list_structure_versions_request::filter},
    };
}

/**
 * @brief The members of list_structure_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_structure_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_structure_versions_response::result},
        utility::codec::field{"versions", &list_structure_versions_response::versions},
        utility::codec::field{"total", &list_structure_versions_response::total},
    };
}

/**
 * @brief The members of get_structure_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_structure_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_structure_version_request::key},
    };
}

/**
 * @brief The members of get_structure_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_structure_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_structure_version_response::result},
        utility::codec::field{"version", &get_structure_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::structure_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::structure_key& v);
    static ores::trading::messaging::structure_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::structure_key& v);
    static ores::trading::messaging::structure_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::structure_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::structure_write& v);
    static ores::trading::messaging::structure_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::structure_write& v);
    static ores::trading::messaging::structure_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::structure_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::structure_change& v);
    static ores::trading::messaging::structure_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::structure_change& v);
    static ores::trading::messaging::structure_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::structure_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::structure_removal& v);
    static ores::trading::messaging::structure_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::structure_removal& v);
    static ores::trading::messaging::structure_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::structure_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::structure_lookup& v);
    static ores::trading::messaging::structure_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::structure_lookup& v);
    static ores::trading::messaging::structure_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::structures_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::structures_filter& v);
    static ores::trading::messaging::structures_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::structures_filter& v);
    static ores::trading::messaging::structures_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::structure_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::structure_event& v);
    static ores::trading::messaging::structure_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::structure_event& v);
    static ores::trading::messaging::structure_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::structure_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::structure_version_key& v);
    static ores::trading::messaging::structure_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::structure_version_key& v);
    static ores::trading::messaging::structure_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::structure_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::structure_versions_filter& v);
    static ores::trading::messaging::structure_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::structure_versions_filter& v);
    static ores::trading::messaging::structure_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_structures_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_structures_request& v);
    static ores::trading::messaging::list_structures_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_structures_request& v);
    static ores::trading::messaging::list_structures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_structures_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_structures_response& v);
    static ores::trading::messaging::list_structures_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_structures_response& v);
    static ores::trading::messaging::list_structures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_structure_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_structure_request& v);
    static ores::trading::messaging::get_structure_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_structure_request& v);
    static ores::trading::messaging::get_structure_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_structure_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_structure_response& v);
    static ores::trading::messaging::get_structure_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_structure_response& v);
    static ores::trading::messaging::get_structure_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_many_structures_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_many_structures_request& v);
    static ores::trading::messaging::get_many_structures_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_structures_request& v);
    static ores::trading::messaging::get_many_structures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_many_structures_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_many_structures_response& v);
    static ores::trading::messaging::get_many_structures_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_structures_response& v);
    static ores::trading::messaging::get_many_structures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_structure_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_structure_request& v);
    static ores::trading::messaging::put_structure_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_structure_request& v);
    static ores::trading::messaging::put_structure_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_structure_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_structure_response& v);
    static ores::trading::messaging::put_structure_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_structure_response& v);
    static ores::trading::messaging::put_structure_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_many_structures_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_many_structures_request& v);
    static ores::trading::messaging::put_many_structures_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_structures_request& v);
    static ores::trading::messaging::put_many_structures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_many_structures_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_many_structures_response& v);
    static ores::trading::messaging::put_many_structures_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_structures_response& v);
    static ores::trading::messaging::put_many_structures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_structure_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_structure_request& v);
    static ores::trading::messaging::delete_structure_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::delete_structure_request& v);
    static ores::trading::messaging::delete_structure_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_structure_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_structure_response& v);
    static ores::trading::messaging::delete_structure_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::delete_structure_response& v);
    static ores::trading::messaging::delete_structure_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_structures_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_structures_request& v);
    static ores::trading::messaging::delete_many_structures_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_structures_request& v);
    static ores::trading::messaging::delete_many_structures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_structures_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_structures_response& v);
    static ores::trading::messaging::delete_many_structures_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_structures_response& v);
    static ores::trading::messaging::delete_many_structures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_structure_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_structure_versions_request& v);
    static ores::trading::messaging::list_structure_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_structure_versions_request& v);
    static ores::trading::messaging::list_structure_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_structure_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_structure_versions_response& v);
    static ores::trading::messaging::list_structure_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_structure_versions_response& v);
    static ores::trading::messaging::list_structure_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_structure_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_structure_version_request& v);
    static ores::trading::messaging::get_structure_version_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_structure_version_request& v);
    static ores::trading::messaging::get_structure_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_structure_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_structure_version_response& v);
    static ores::trading::messaging::get_structure_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_structure_version_response& v);
    static ores::trading::messaging::get_structure_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
