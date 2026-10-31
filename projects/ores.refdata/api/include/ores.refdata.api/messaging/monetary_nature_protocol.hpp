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
#ifndef ORES_REFDATA_API_MESSAGING_MONETARY_NATURE_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_MONETARY_NATURE_PROTOCOL_HPP

#include "ores.refdata.api/domain/monetary_nature.hpp"
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

struct monetary_nature_key {
    std::string code;
};

struct monetary_nature_write {
    std::string code;
    std::string name;
    std::string description;
    int display_order;
};

struct monetary_nature_change {
    monetary_nature_write write;
    ores::utility::domain::precondition precondition;
};

struct monetary_nature_removal {
    monetary_nature_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct monetary_nature_lookup {
    monetary_nature_key key;
    std::optional<ores::refdata::domain::monetary_nature> monetary_nature;
};

struct monetary_natures_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct monetary_nature_event {
    boost::uuids::uuid event_id;
    monetary_nature_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct monetary_nature_version_key {
    monetary_nature_key monetary_nature;
    std::uint32_t version;
};

struct monetary_nature_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_monetary_natures_request {
    using response_type = struct list_monetary_natures_response;
    static constexpr std::string_view nats_subject = "refdata.v1.monetary_natures.list";
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
    std::optional<monetary_natures_filter> filter;
    std::optional<std::string> as_of;
};

struct list_monetary_natures_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::monetary_nature> types;
    std::uint64_t total;
};

struct get_monetary_nature_request {
    using response_type = struct get_monetary_nature_response;
    static constexpr std::string_view nats_subject = "refdata.v1.monetary_natures.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    monetary_nature_key key;
};

struct get_monetary_nature_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::monetary_nature> monetary_nature;
};

struct get_many_monetary_natures_request {
    using response_type = struct get_many_monetary_natures_response;
    static constexpr std::string_view nats_subject = "refdata.v1.monetary_natures.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<monetary_nature_key> keys;
};

struct get_many_monetary_natures_response {
    ores::utility::domain::result result;
    std::vector<monetary_nature_lookup> entries;
};

struct put_monetary_nature_request {
    using response_type = struct put_monetary_nature_response;
    static constexpr std::string_view nats_subject = "refdata.v1.monetary_natures.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    monetary_nature_change change;
    ores::utility::domain::change_intent intent;
};

struct put_monetary_nature_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::monetary_nature> monetary_nature;
};

struct put_many_monetary_natures_request {
    using response_type = struct put_many_monetary_natures_response;
    static constexpr std::string_view nats_subject = "refdata.v1.monetary_natures.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<monetary_nature_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_monetary_natures_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::monetary_nature> types;
};

struct delete_monetary_nature_request {
    using response_type = struct delete_monetary_nature_response;
    static constexpr std::string_view nats_subject = "refdata.v1.monetary_natures.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    monetary_nature_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_monetary_nature_response {
    ores::utility::domain::result result;
};

struct delete_many_monetary_natures_request {
    using response_type = struct delete_many_monetary_natures_response;
    static constexpr std::string_view nats_subject = "refdata.v1.monetary_natures.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<monetary_nature_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_monetary_natures_response {
    ores::utility::domain::result result;
};

struct list_monetary_nature_versions_request {
    using response_type = struct list_monetary_nature_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.monetary_natures_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    monetary_nature_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<monetary_nature_versions_filter> filter;
};

struct list_monetary_nature_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::monetary_nature> versions;
    std::uint64_t total;
};

struct get_monetary_nature_version_request {
    using response_type = struct get_monetary_nature_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.monetary_natures_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    monetary_nature_version_key key;
};

struct get_monetary_nature_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::monetary_nature> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace monetary_nature_event_subjects {
inline constexpr std::string_view created = "refdata.v1.monetary_natures_events.created";
inline constexpr std::string_view updated = "refdata.v1.monetary_natures_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.monetary_natures_events.deleted";
}
/**
 * @brief The members of monetary_nature_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const monetary_nature_key&) {
    return std::tuple{
        utility::codec::field{"code", &monetary_nature_key::code},
    };
}

/**
 * @brief The members of monetary_nature_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const monetary_nature_write&) {
    return std::tuple{
        utility::codec::field{"code", &monetary_nature_write::code},
        utility::codec::field{"name", &monetary_nature_write::name},
        utility::codec::field{"description", &monetary_nature_write::description},
        utility::codec::field{"display_order", &monetary_nature_write::display_order},
    };
}

/**
 * @brief The members of monetary_nature_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const monetary_nature_change&) {
    return std::tuple{
        utility::codec::field{"write", &monetary_nature_change::write},
        utility::codec::field{"precondition", &monetary_nature_change::precondition},
    };
}

/**
 * @brief The members of monetary_nature_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const monetary_nature_removal&) {
    return std::tuple{
        utility::codec::field{"key", &monetary_nature_removal::key},
        utility::codec::field{"precondition", &monetary_nature_removal::precondition},
    };
}

/**
 * @brief The members of monetary_nature_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const monetary_nature_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &monetary_nature_lookup::key},
        utility::codec::field{"monetary_nature", &monetary_nature_lookup::monetary_nature},
    };
}

/**
 * @brief The members of monetary_natures_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const monetary_natures_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &monetary_natures_filter::code_one_of},
    };
}

/**
 * @brief The members of monetary_nature_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const monetary_nature_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &monetary_nature_event::event_id},
        utility::codec::field{"key", &monetary_nature_event::key},
        utility::codec::field{"action", &monetary_nature_event::action},
        utility::codec::field{"version", &monetary_nature_event::version},
        utility::codec::field{"occurred_at", &monetary_nature_event::occurred_at},
        utility::codec::field{"correlation_id", &monetary_nature_event::correlation_id},
    };
}

/**
 * @brief The members of monetary_nature_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const monetary_nature_version_key&) {
    return std::tuple{
        utility::codec::field{"monetary_nature", &monetary_nature_version_key::monetary_nature},
        utility::codec::field{"version", &monetary_nature_version_key::version},
    };
}

/**
 * @brief The members of monetary_nature_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const monetary_nature_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &monetary_nature_versions_filter::version},
        utility::codec::field{"from_version", &monetary_nature_versions_filter::from_version},
        utility::codec::field{"to_version", &monetary_nature_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_monetary_natures_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_monetary_natures_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_monetary_natures_request::offset},
        utility::codec::field{"limit", &list_monetary_natures_request::limit},
        utility::codec::field{"order", &list_monetary_natures_request::order},
        utility::codec::field{"filter", &list_monetary_natures_request::filter},
        utility::codec::field{"as_of", &list_monetary_natures_request::as_of},
    };
}

/**
 * @brief The members of list_monetary_natures_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_monetary_natures_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_monetary_natures_response::result},
        utility::codec::field{"types", &list_monetary_natures_response::types},
        utility::codec::field{"total", &list_monetary_natures_response::total},
    };
}

/**
 * @brief The members of get_monetary_nature_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_monetary_nature_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_monetary_nature_request::key},
    };
}

/**
 * @brief The members of get_monetary_nature_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_monetary_nature_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_monetary_nature_response::result},
        utility::codec::field{"monetary_nature", &get_monetary_nature_response::monetary_nature},
    };
}

/**
 * @brief The members of get_many_monetary_natures_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_monetary_natures_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_monetary_natures_request::keys},
    };
}

/**
 * @brief The members of get_many_monetary_natures_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_monetary_natures_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_monetary_natures_response::result},
        utility::codec::field{"entries", &get_many_monetary_natures_response::entries},
    };
}

/**
 * @brief The members of put_monetary_nature_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_monetary_nature_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_monetary_nature_request::change},
        utility::codec::field{"intent", &put_monetary_nature_request::intent},
    };
}

/**
 * @brief The members of put_monetary_nature_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_monetary_nature_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_monetary_nature_response::result},
        utility::codec::field{"monetary_nature", &put_monetary_nature_response::monetary_nature},
    };
}

/**
 * @brief The members of put_many_monetary_natures_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_monetary_natures_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_monetary_natures_request::changes},
        utility::codec::field{"intent", &put_many_monetary_natures_request::intent},
    };
}

/**
 * @brief The members of put_many_monetary_natures_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_monetary_natures_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_monetary_natures_response::result},
        utility::codec::field{"types", &put_many_monetary_natures_response::types},
    };
}

/**
 * @brief The members of delete_monetary_nature_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_monetary_nature_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_monetary_nature_request::removal},
        utility::codec::field{"intent", &delete_monetary_nature_request::intent},
    };
}

/**
 * @brief The members of delete_monetary_nature_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_monetary_nature_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_monetary_nature_response::result},
    };
}

/**
 * @brief The members of delete_many_monetary_natures_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_monetary_natures_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_monetary_natures_request::removals},
        utility::codec::field{"intent", &delete_many_monetary_natures_request::intent},
    };
}

/**
 * @brief The members of delete_many_monetary_natures_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_monetary_natures_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_monetary_natures_response::result},
    };
}

/**
 * @brief The members of list_monetary_nature_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_monetary_nature_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_monetary_nature_versions_request::key},
        utility::codec::field{"offset", &list_monetary_nature_versions_request::offset},
        utility::codec::field{"limit", &list_monetary_nature_versions_request::limit},
        utility::codec::field{"order", &list_monetary_nature_versions_request::order},
        utility::codec::field{"filter", &list_monetary_nature_versions_request::filter},
    };
}

/**
 * @brief The members of list_monetary_nature_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_monetary_nature_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_monetary_nature_versions_response::result},
        utility::codec::field{"versions", &list_monetary_nature_versions_response::versions},
        utility::codec::field{"total", &list_monetary_nature_versions_response::total},
    };
}

/**
 * @brief The members of get_monetary_nature_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_monetary_nature_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_monetary_nature_version_request::key},
    };
}

/**
 * @brief The members of get_monetary_nature_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_monetary_nature_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_monetary_nature_version_response::result},
        utility::codec::field{"version", &get_monetary_nature_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::monetary_nature_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::monetary_nature_key& v);
    static ores::refdata::messaging::monetary_nature_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::monetary_nature_key& v);
    static ores::refdata::messaging::monetary_nature_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::monetary_nature_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::monetary_nature_write& v);
    static ores::refdata::messaging::monetary_nature_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::monetary_nature_write& v);
    static ores::refdata::messaging::monetary_nature_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::monetary_nature_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::monetary_nature_change& v);
    static ores::refdata::messaging::monetary_nature_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::monetary_nature_change& v);
    static ores::refdata::messaging::monetary_nature_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::monetary_nature_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::monetary_nature_removal& v);
    static ores::refdata::messaging::monetary_nature_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::monetary_nature_removal& v);
    static ores::refdata::messaging::monetary_nature_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::monetary_nature_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::monetary_nature_lookup& v);
    static ores::refdata::messaging::monetary_nature_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::monetary_nature_lookup& v);
    static ores::refdata::messaging::monetary_nature_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::monetary_natures_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::monetary_natures_filter& v);
    static ores::refdata::messaging::monetary_natures_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::monetary_natures_filter& v);
    static ores::refdata::messaging::monetary_natures_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::monetary_nature_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::monetary_nature_event& v);
    static ores::refdata::messaging::monetary_nature_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::monetary_nature_event& v);
    static ores::refdata::messaging::monetary_nature_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::monetary_nature_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::monetary_nature_version_key& v);
    static ores::refdata::messaging::monetary_nature_version_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::monetary_nature_version_key& v);
    static ores::refdata::messaging::monetary_nature_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::monetary_nature_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::monetary_nature_versions_filter& v);
    static ores::refdata::messaging::monetary_nature_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::monetary_nature_versions_filter& v);
    static ores::refdata::messaging::monetary_nature_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_monetary_natures_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_monetary_natures_request& v);
    static ores::refdata::messaging::list_monetary_natures_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_monetary_natures_request& v);
    static ores::refdata::messaging::list_monetary_natures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_monetary_natures_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_monetary_natures_response& v);
    static ores::refdata::messaging::list_monetary_natures_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_monetary_natures_response& v);
    static ores::refdata::messaging::list_monetary_natures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_monetary_nature_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_monetary_nature_request& v);
    static ores::refdata::messaging::get_monetary_nature_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_monetary_nature_request& v);
    static ores::refdata::messaging::get_monetary_nature_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_monetary_nature_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_monetary_nature_response& v);
    static ores::refdata::messaging::get_monetary_nature_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_monetary_nature_response& v);
    static ores::refdata::messaging::get_monetary_nature_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_monetary_natures_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_monetary_natures_request& v);
    static ores::refdata::messaging::get_many_monetary_natures_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_monetary_natures_request& v);
    static ores::refdata::messaging::get_many_monetary_natures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_monetary_natures_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_monetary_natures_response& v);
    static ores::refdata::messaging::get_many_monetary_natures_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_monetary_natures_response& v);
    static ores::refdata::messaging::get_many_monetary_natures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_monetary_nature_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_monetary_nature_request& v);
    static ores::refdata::messaging::put_monetary_nature_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_monetary_nature_request& v);
    static ores::refdata::messaging::put_monetary_nature_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_monetary_nature_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_monetary_nature_response& v);
    static ores::refdata::messaging::put_monetary_nature_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_monetary_nature_response& v);
    static ores::refdata::messaging::put_monetary_nature_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_monetary_natures_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_monetary_natures_request& v);
    static ores::refdata::messaging::put_many_monetary_natures_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_monetary_natures_request& v);
    static ores::refdata::messaging::put_many_monetary_natures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_monetary_natures_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_monetary_natures_response& v);
    static ores::refdata::messaging::put_many_monetary_natures_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_monetary_natures_response& v);
    static ores::refdata::messaging::put_many_monetary_natures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_monetary_nature_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_monetary_nature_request& v);
    static ores::refdata::messaging::delete_monetary_nature_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_monetary_nature_request& v);
    static ores::refdata::messaging::delete_monetary_nature_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_monetary_nature_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_monetary_nature_response& v);
    static ores::refdata::messaging::delete_monetary_nature_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_monetary_nature_response& v);
    static ores::refdata::messaging::delete_monetary_nature_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_monetary_natures_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_monetary_natures_request& v);
    static ores::refdata::messaging::delete_many_monetary_natures_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_monetary_natures_request& v);
    static ores::refdata::messaging::delete_many_monetary_natures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_monetary_natures_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_monetary_natures_response& v);
    static ores::refdata::messaging::delete_many_monetary_natures_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_monetary_natures_response& v);
    static ores::refdata::messaging::delete_many_monetary_natures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_monetary_nature_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_monetary_nature_versions_request& v);
    static ores::refdata::messaging::list_monetary_nature_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_monetary_nature_versions_request& v);
    static ores::refdata::messaging::list_monetary_nature_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_monetary_nature_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_monetary_nature_versions_response& v);
    static ores::refdata::messaging::list_monetary_nature_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_monetary_nature_versions_response& v);
    static ores::refdata::messaging::list_monetary_nature_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_monetary_nature_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_monetary_nature_version_request& v);
    static ores::refdata::messaging::get_monetary_nature_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_monetary_nature_version_request& v);
    static ores::refdata::messaging::get_monetary_nature_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_monetary_nature_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_monetary_nature_version_response& v);
    static ores::refdata::messaging::get_monetary_nature_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_monetary_nature_version_response& v);
    static ores::refdata::messaging::get_monetary_nature_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
