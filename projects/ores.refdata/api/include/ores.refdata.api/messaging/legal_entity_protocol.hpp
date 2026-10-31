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
#ifndef ORES_REFDATA_API_MESSAGING_LEGAL_ENTITY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_LEGAL_ENTITY_PROTOCOL_HPP

#include "ores.refdata.api/domain/legal_entity.hpp"
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

struct legal_entity_key {
    std::string lei;
};

struct legal_entity_write {
    std::string lei;
    std::string legal_name;
    std::string jurisdiction;
    std::string entity_status;
    std::string entity_category;
    int display_order;
};

struct legal_entity_change {
    legal_entity_write write;
    ores::utility::domain::precondition precondition;
};

struct legal_entity_removal {
    legal_entity_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct legal_entity_lookup {
    legal_entity_key key;
    std::optional<ores::refdata::domain::legal_entity> legal_entity;
};

struct legal_entities_filter {
    std::optional<std::vector<std::string>> lei_one_of;
};

struct legal_entity_event {
    boost::uuids::uuid event_id;
    legal_entity_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct legal_entity_version_key {
    legal_entity_key legal_entity;
    std::uint32_t version;
};

struct legal_entity_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_legal_entities_request {
    using response_type = struct list_legal_entities_response;
    static constexpr std::string_view nats_subject = "refdata.v1.legal_entities.list";
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
    std::optional<legal_entities_filter> filter;
    std::optional<std::string> as_of;
};

struct list_legal_entities_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::legal_entity> legal_entities;
    std::uint64_t total;
};

struct get_legal_entity_request {
    using response_type = struct get_legal_entity_response;
    static constexpr std::string_view nats_subject = "refdata.v1.legal_entities.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    legal_entity_key key;
};

struct get_legal_entity_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::legal_entity> legal_entity;
};

struct get_many_legal_entities_request {
    using response_type = struct get_many_legal_entities_response;
    static constexpr std::string_view nats_subject = "refdata.v1.legal_entities.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<legal_entity_key> keys;
};

struct get_many_legal_entities_response {
    ores::utility::domain::result result;
    std::vector<legal_entity_lookup> entries;
};

struct put_legal_entity_request {
    using response_type = struct put_legal_entity_response;
    static constexpr std::string_view nats_subject = "refdata.v1.legal_entities.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    legal_entity_change change;
    ores::utility::domain::change_intent intent;
};

struct put_legal_entity_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::legal_entity> legal_entity;
};

struct put_many_legal_entities_request {
    using response_type = struct put_many_legal_entities_response;
    static constexpr std::string_view nats_subject = "refdata.v1.legal_entities.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<legal_entity_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_legal_entities_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::legal_entity> legal_entities;
};

struct delete_legal_entity_request {
    using response_type = struct delete_legal_entity_response;
    static constexpr std::string_view nats_subject = "refdata.v1.legal_entities.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    legal_entity_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_legal_entity_response {
    ores::utility::domain::result result;
};

struct delete_many_legal_entities_request {
    using response_type = struct delete_many_legal_entities_response;
    static constexpr std::string_view nats_subject = "refdata.v1.legal_entities.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<legal_entity_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_legal_entities_response {
    ores::utility::domain::result result;
};

struct list_legal_entity_versions_request {
    using response_type = struct list_legal_entity_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.legal_entities_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    legal_entity_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<legal_entity_versions_filter> filter;
};

struct list_legal_entity_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::legal_entity> versions;
    std::uint64_t total;
};

struct get_legal_entity_version_request {
    using response_type = struct get_legal_entity_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.legal_entities_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    legal_entity_version_key key;
};

struct get_legal_entity_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::legal_entity> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace legal_entity_event_subjects {
inline constexpr std::string_view created = "refdata.v1.legal_entities_events.created";
inline constexpr std::string_view updated = "refdata.v1.legal_entities_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.legal_entities_events.deleted";
}
/**
 * @brief The members of legal_entity_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const legal_entity_key&) {
    return std::tuple{
        utility::codec::field{"lei", &legal_entity_key::lei},
    };
}

/**
 * @brief The members of legal_entity_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const legal_entity_write&) {
    return std::tuple{
        utility::codec::field{"lei", &legal_entity_write::lei},
        utility::codec::field{"legal_name", &legal_entity_write::legal_name},
        utility::codec::field{"jurisdiction", &legal_entity_write::jurisdiction},
        utility::codec::field{"entity_status", &legal_entity_write::entity_status},
        utility::codec::field{"entity_category", &legal_entity_write::entity_category},
        utility::codec::field{"display_order", &legal_entity_write::display_order},
    };
}

/**
 * @brief The members of legal_entity_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const legal_entity_change&) {
    return std::tuple{
        utility::codec::field{"write", &legal_entity_change::write},
        utility::codec::field{"precondition", &legal_entity_change::precondition},
    };
}

/**
 * @brief The members of legal_entity_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const legal_entity_removal&) {
    return std::tuple{
        utility::codec::field{"key", &legal_entity_removal::key},
        utility::codec::field{"precondition", &legal_entity_removal::precondition},
    };
}

/**
 * @brief The members of legal_entity_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const legal_entity_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &legal_entity_lookup::key},
        utility::codec::field{"legal_entity", &legal_entity_lookup::legal_entity},
    };
}

/**
 * @brief The members of legal_entities_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const legal_entities_filter&) {
    return std::tuple{
        utility::codec::field{"lei_one_of", &legal_entities_filter::lei_one_of},
    };
}

/**
 * @brief The members of legal_entity_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const legal_entity_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &legal_entity_event::event_id},
        utility::codec::field{"key", &legal_entity_event::key},
        utility::codec::field{"action", &legal_entity_event::action},
        utility::codec::field{"version", &legal_entity_event::version},
        utility::codec::field{"occurred_at", &legal_entity_event::occurred_at},
        utility::codec::field{"correlation_id", &legal_entity_event::correlation_id},
    };
}

/**
 * @brief The members of legal_entity_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const legal_entity_version_key&) {
    return std::tuple{
        utility::codec::field{"legal_entity", &legal_entity_version_key::legal_entity},
        utility::codec::field{"version", &legal_entity_version_key::version},
    };
}

/**
 * @brief The members of legal_entity_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const legal_entity_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &legal_entity_versions_filter::version},
        utility::codec::field{"from_version", &legal_entity_versions_filter::from_version},
        utility::codec::field{"to_version", &legal_entity_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_legal_entities_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_legal_entities_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_legal_entities_request::offset},
        utility::codec::field{"limit", &list_legal_entities_request::limit},
        utility::codec::field{"order", &list_legal_entities_request::order},
        utility::codec::field{"filter", &list_legal_entities_request::filter},
        utility::codec::field{"as_of", &list_legal_entities_request::as_of},
    };
}

/**
 * @brief The members of list_legal_entities_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_legal_entities_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_legal_entities_response::result},
        utility::codec::field{"legal_entities", &list_legal_entities_response::legal_entities},
        utility::codec::field{"total", &list_legal_entities_response::total},
    };
}

/**
 * @brief The members of get_legal_entity_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_legal_entity_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_legal_entity_request::key},
    };
}

/**
 * @brief The members of get_legal_entity_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_legal_entity_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_legal_entity_response::result},
        utility::codec::field{"legal_entity", &get_legal_entity_response::legal_entity},
    };
}

/**
 * @brief The members of get_many_legal_entities_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_legal_entities_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_legal_entities_request::keys},
    };
}

/**
 * @brief The members of get_many_legal_entities_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_legal_entities_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_legal_entities_response::result},
        utility::codec::field{"entries", &get_many_legal_entities_response::entries},
    };
}

/**
 * @brief The members of put_legal_entity_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_legal_entity_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_legal_entity_request::change},
        utility::codec::field{"intent", &put_legal_entity_request::intent},
    };
}

/**
 * @brief The members of put_legal_entity_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_legal_entity_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_legal_entity_response::result},
        utility::codec::field{"legal_entity", &put_legal_entity_response::legal_entity},
    };
}

/**
 * @brief The members of put_many_legal_entities_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_legal_entities_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_legal_entities_request::changes},
        utility::codec::field{"intent", &put_many_legal_entities_request::intent},
    };
}

/**
 * @brief The members of put_many_legal_entities_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_legal_entities_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_legal_entities_response::result},
        utility::codec::field{"legal_entities", &put_many_legal_entities_response::legal_entities},
    };
}

/**
 * @brief The members of delete_legal_entity_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_legal_entity_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_legal_entity_request::removal},
        utility::codec::field{"intent", &delete_legal_entity_request::intent},
    };
}

/**
 * @brief The members of delete_legal_entity_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_legal_entity_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_legal_entity_response::result},
    };
}

/**
 * @brief The members of delete_many_legal_entities_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_legal_entities_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_legal_entities_request::removals},
        utility::codec::field{"intent", &delete_many_legal_entities_request::intent},
    };
}

/**
 * @brief The members of delete_many_legal_entities_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_legal_entities_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_legal_entities_response::result},
    };
}

/**
 * @brief The members of list_legal_entity_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_legal_entity_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_legal_entity_versions_request::key},
        utility::codec::field{"offset", &list_legal_entity_versions_request::offset},
        utility::codec::field{"limit", &list_legal_entity_versions_request::limit},
        utility::codec::field{"order", &list_legal_entity_versions_request::order},
        utility::codec::field{"filter", &list_legal_entity_versions_request::filter},
    };
}

/**
 * @brief The members of list_legal_entity_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_legal_entity_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_legal_entity_versions_response::result},
        utility::codec::field{"versions", &list_legal_entity_versions_response::versions},
        utility::codec::field{"total", &list_legal_entity_versions_response::total},
    };
}

/**
 * @brief The members of get_legal_entity_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_legal_entity_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_legal_entity_version_request::key},
    };
}

/**
 * @brief The members of get_legal_entity_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_legal_entity_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_legal_entity_version_response::result},
        utility::codec::field{"version", &get_legal_entity_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::legal_entity_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::legal_entity_key& v);
    static ores::refdata::messaging::legal_entity_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::legal_entity_key& v);
    static ores::refdata::messaging::legal_entity_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::legal_entity_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::legal_entity_write& v);
    static ores::refdata::messaging::legal_entity_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::legal_entity_write& v);
    static ores::refdata::messaging::legal_entity_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::legal_entity_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::legal_entity_change& v);
    static ores::refdata::messaging::legal_entity_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::legal_entity_change& v);
    static ores::refdata::messaging::legal_entity_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::legal_entity_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::legal_entity_removal& v);
    static ores::refdata::messaging::legal_entity_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::legal_entity_removal& v);
    static ores::refdata::messaging::legal_entity_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::legal_entity_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::legal_entity_lookup& v);
    static ores::refdata::messaging::legal_entity_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::legal_entity_lookup& v);
    static ores::refdata::messaging::legal_entity_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::legal_entities_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::legal_entities_filter& v);
    static ores::refdata::messaging::legal_entities_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::legal_entities_filter& v);
    static ores::refdata::messaging::legal_entities_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::legal_entity_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::legal_entity_event& v);
    static ores::refdata::messaging::legal_entity_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::legal_entity_event& v);
    static ores::refdata::messaging::legal_entity_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::legal_entity_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::legal_entity_version_key& v);
    static ores::refdata::messaging::legal_entity_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::legal_entity_version_key& v);
    static ores::refdata::messaging::legal_entity_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::legal_entity_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::legal_entity_versions_filter& v);
    static ores::refdata::messaging::legal_entity_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::legal_entity_versions_filter& v);
    static ores::refdata::messaging::legal_entity_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_legal_entities_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_legal_entities_request& v);
    static ores::refdata::messaging::list_legal_entities_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_legal_entities_request& v);
    static ores::refdata::messaging::list_legal_entities_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_legal_entities_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_legal_entities_response& v);
    static ores::refdata::messaging::list_legal_entities_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_legal_entities_response& v);
    static ores::refdata::messaging::list_legal_entities_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_legal_entity_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_legal_entity_request& v);
    static ores::refdata::messaging::get_legal_entity_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_legal_entity_request& v);
    static ores::refdata::messaging::get_legal_entity_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_legal_entity_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_legal_entity_response& v);
    static ores::refdata::messaging::get_legal_entity_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_legal_entity_response& v);
    static ores::refdata::messaging::get_legal_entity_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_legal_entities_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_legal_entities_request& v);
    static ores::refdata::messaging::get_many_legal_entities_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_legal_entities_request& v);
    static ores::refdata::messaging::get_many_legal_entities_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_legal_entities_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_legal_entities_response& v);
    static ores::refdata::messaging::get_many_legal_entities_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_legal_entities_response& v);
    static ores::refdata::messaging::get_many_legal_entities_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_legal_entity_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_legal_entity_request& v);
    static ores::refdata::messaging::put_legal_entity_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_legal_entity_request& v);
    static ores::refdata::messaging::put_legal_entity_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_legal_entity_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_legal_entity_response& v);
    static ores::refdata::messaging::put_legal_entity_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_legal_entity_response& v);
    static ores::refdata::messaging::put_legal_entity_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_legal_entities_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_legal_entities_request& v);
    static ores::refdata::messaging::put_many_legal_entities_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_legal_entities_request& v);
    static ores::refdata::messaging::put_many_legal_entities_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_legal_entities_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_legal_entities_response& v);
    static ores::refdata::messaging::put_many_legal_entities_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_legal_entities_response& v);
    static ores::refdata::messaging::put_many_legal_entities_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_legal_entity_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_legal_entity_request& v);
    static ores::refdata::messaging::delete_legal_entity_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_legal_entity_request& v);
    static ores::refdata::messaging::delete_legal_entity_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_legal_entity_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_legal_entity_response& v);
    static ores::refdata::messaging::delete_legal_entity_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_legal_entity_response& v);
    static ores::refdata::messaging::delete_legal_entity_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_legal_entities_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_legal_entities_request& v);
    static ores::refdata::messaging::delete_many_legal_entities_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_legal_entities_request& v);
    static ores::refdata::messaging::delete_many_legal_entities_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_legal_entities_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_legal_entities_response& v);
    static ores::refdata::messaging::delete_many_legal_entities_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_legal_entities_response& v);
    static ores::refdata::messaging::delete_many_legal_entities_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_legal_entity_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_legal_entity_versions_request& v);
    static ores::refdata::messaging::list_legal_entity_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_legal_entity_versions_request& v);
    static ores::refdata::messaging::list_legal_entity_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_legal_entity_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_legal_entity_versions_response& v);
    static ores::refdata::messaging::list_legal_entity_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_legal_entity_versions_response& v);
    static ores::refdata::messaging::list_legal_entity_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_legal_entity_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_legal_entity_version_request& v);
    static ores::refdata::messaging::get_legal_entity_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_legal_entity_version_request& v);
    static ores::refdata::messaging::get_legal_entity_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_legal_entity_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_legal_entity_version_response& v);
    static ores::refdata::messaging::get_legal_entity_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_legal_entity_version_response& v);
    static ores::refdata::messaging::get_legal_entity_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
