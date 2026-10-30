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
#ifndef ORES_DQ_API_MESSAGING_CODING_SCHEME_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_CODING_SCHEME_PROTOCOL_HPP

#include "ores.dq.api/domain/coding_scheme.hpp"
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

struct coding_scheme_key {
    std::string code;
};

struct coding_scheme_write {
    std::string code;
    std::string name;
    std::string authority_type;
    std::string subject_area_name;
    std::string domain_name;
    std::string uri;
    std::string description;
};

struct coding_scheme_change {
    coding_scheme_write write;
    ores::utility::domain::precondition precondition;
};

struct coding_scheme_removal {
    coding_scheme_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct coding_scheme_lookup {
    coding_scheme_key key;
    std::optional<ores::dq::domain::coding_scheme> coding_scheme;
};

struct coding_schemes_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct coding_scheme_event {
    boost::uuids::uuid event_id;
    coding_scheme_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct coding_scheme_version_key {
    coding_scheme_key coding_scheme;
    std::uint32_t version;
};

struct coding_scheme_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_coding_schemes_request {
    using response_type = struct list_coding_schemes_response;
    static constexpr std::string_view nats_subject = "dq.v1.coding_schemes.list";
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
    std::optional<coding_schemes_filter> filter;
    std::optional<std::string> as_of;
};

struct list_coding_schemes_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::coding_scheme> schemes;
    std::uint64_t total;
};

struct get_coding_scheme_request {
    using response_type = struct get_coding_scheme_response;
    static constexpr std::string_view nats_subject = "dq.v1.coding_schemes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    coding_scheme_key key;
};

struct get_coding_scheme_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::coding_scheme> coding_scheme;
};

struct get_many_coding_schemes_request {
    using response_type = struct get_many_coding_schemes_response;
    static constexpr std::string_view nats_subject = "dq.v1.coding_schemes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<coding_scheme_key> keys;
};

struct get_many_coding_schemes_response {
    ores::utility::domain::result result;
    std::vector<coding_scheme_lookup> entries;
};

struct put_coding_scheme_request {
    using response_type = struct put_coding_scheme_response;
    static constexpr std::string_view nats_subject = "dq.v1.coding_schemes.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    coding_scheme_change change;
    ores::utility::domain::change_intent intent;
};

struct put_coding_scheme_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::coding_scheme> coding_scheme;
};

struct put_many_coding_schemes_request {
    using response_type = struct put_many_coding_schemes_response;
    static constexpr std::string_view nats_subject = "dq.v1.coding_schemes.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<coding_scheme_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_coding_schemes_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::coding_scheme> schemes;
};

struct delete_coding_scheme_request {
    using response_type = struct delete_coding_scheme_response;
    static constexpr std::string_view nats_subject = "dq.v1.coding_schemes.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    coding_scheme_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_coding_scheme_response {
    ores::utility::domain::result result;
};

struct delete_many_coding_schemes_request {
    using response_type = struct delete_many_coding_schemes_response;
    static constexpr std::string_view nats_subject = "dq.v1.coding_schemes.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<coding_scheme_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_coding_schemes_response {
    ores::utility::domain::result result;
};

struct list_coding_scheme_versions_request {
    using response_type = struct list_coding_scheme_versions_response;
    static constexpr std::string_view nats_subject = "dq.v1.coding_schemes_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    coding_scheme_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<coding_scheme_versions_filter> filter;
};

struct list_coding_scheme_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::coding_scheme> versions;
    std::uint64_t total;
};

struct get_coding_scheme_version_request {
    using response_type = struct get_coding_scheme_version_response;
    static constexpr std::string_view nats_subject = "dq.v1.coding_schemes_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    coding_scheme_version_key key;
};

struct get_coding_scheme_version_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::coding_scheme> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace coding_scheme_event_subjects {
inline constexpr std::string_view created = "dq.v1.coding_schemes_events.created";
inline constexpr std::string_view updated = "dq.v1.coding_schemes_events.updated";
inline constexpr std::string_view deleted = "dq.v1.coding_schemes_events.deleted";
}
/**
 * @brief The members of coding_scheme_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const coding_scheme_key&) {
    return std::tuple{
        utility::codec::field{"code", &coding_scheme_key::code},
    };
}

/**
 * @brief The members of coding_scheme_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const coding_scheme_write&) {
    return std::tuple{
        utility::codec::field{"code", &coding_scheme_write::code},
        utility::codec::field{"name", &coding_scheme_write::name},
        utility::codec::field{"authority_type", &coding_scheme_write::authority_type},
        utility::codec::field{"subject_area_name", &coding_scheme_write::subject_area_name},
        utility::codec::field{"domain_name", &coding_scheme_write::domain_name},
        utility::codec::field{"uri", &coding_scheme_write::uri},
        utility::codec::field{"description", &coding_scheme_write::description},
    };
}

/**
 * @brief The members of coding_scheme_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const coding_scheme_change&) {
    return std::tuple{
        utility::codec::field{"write", &coding_scheme_change::write},
        utility::codec::field{"precondition", &coding_scheme_change::precondition},
    };
}

/**
 * @brief The members of coding_scheme_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const coding_scheme_removal&) {
    return std::tuple{
        utility::codec::field{"key", &coding_scheme_removal::key},
        utility::codec::field{"precondition", &coding_scheme_removal::precondition},
    };
}

/**
 * @brief The members of coding_scheme_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const coding_scheme_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &coding_scheme_lookup::key},
        utility::codec::field{"coding_scheme", &coding_scheme_lookup::coding_scheme},
    };
}

/**
 * @brief The members of coding_schemes_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const coding_schemes_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &coding_schemes_filter::code_one_of},
    };
}

/**
 * @brief The members of coding_scheme_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const coding_scheme_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &coding_scheme_event::event_id},
        utility::codec::field{"key", &coding_scheme_event::key},
        utility::codec::field{"action", &coding_scheme_event::action},
        utility::codec::field{"version", &coding_scheme_event::version},
        utility::codec::field{"occurred_at", &coding_scheme_event::occurred_at},
        utility::codec::field{"correlation_id", &coding_scheme_event::correlation_id},
    };
}

/**
 * @brief The members of coding_scheme_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const coding_scheme_version_key&) {
    return std::tuple{
        utility::codec::field{"coding_scheme", &coding_scheme_version_key::coding_scheme},
        utility::codec::field{"version", &coding_scheme_version_key::version},
    };
}

/**
 * @brief The members of coding_scheme_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const coding_scheme_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &coding_scheme_versions_filter::version},
        utility::codec::field{"from_version", &coding_scheme_versions_filter::from_version},
        utility::codec::field{"to_version", &coding_scheme_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_coding_schemes_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_coding_schemes_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_coding_schemes_request::offset},
        utility::codec::field{"limit", &list_coding_schemes_request::limit},
        utility::codec::field{"order", &list_coding_schemes_request::order},
        utility::codec::field{"filter", &list_coding_schemes_request::filter},
        utility::codec::field{"as_of", &list_coding_schemes_request::as_of},
    };
}

/**
 * @brief The members of list_coding_schemes_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_coding_schemes_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_coding_schemes_response::result},
        utility::codec::field{"schemes", &list_coding_schemes_response::schemes},
        utility::codec::field{"total", &list_coding_schemes_response::total},
    };
}

/**
 * @brief The members of get_coding_scheme_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_coding_scheme_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_coding_scheme_request::key},
    };
}

/**
 * @brief The members of get_coding_scheme_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_coding_scheme_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_coding_scheme_response::result},
        utility::codec::field{"coding_scheme", &get_coding_scheme_response::coding_scheme},
    };
}

/**
 * @brief The members of get_many_coding_schemes_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_coding_schemes_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_coding_schemes_request::keys},
    };
}

/**
 * @brief The members of get_many_coding_schemes_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_coding_schemes_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_coding_schemes_response::result},
        utility::codec::field{"entries", &get_many_coding_schemes_response::entries},
    };
}

/**
 * @brief The members of put_coding_scheme_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_coding_scheme_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_coding_scheme_request::change},
        utility::codec::field{"intent", &put_coding_scheme_request::intent},
    };
}

/**
 * @brief The members of put_coding_scheme_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_coding_scheme_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_coding_scheme_response::result},
        utility::codec::field{"coding_scheme", &put_coding_scheme_response::coding_scheme},
    };
}

/**
 * @brief The members of put_many_coding_schemes_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_coding_schemes_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_coding_schemes_request::changes},
        utility::codec::field{"intent", &put_many_coding_schemes_request::intent},
    };
}

/**
 * @brief The members of put_many_coding_schemes_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_coding_schemes_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_coding_schemes_response::result},
        utility::codec::field{"schemes", &put_many_coding_schemes_response::schemes},
    };
}

/**
 * @brief The members of delete_coding_scheme_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_coding_scheme_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_coding_scheme_request::removal},
        utility::codec::field{"intent", &delete_coding_scheme_request::intent},
    };
}

/**
 * @brief The members of delete_coding_scheme_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_coding_scheme_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_coding_scheme_response::result},
    };
}

/**
 * @brief The members of delete_many_coding_schemes_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_coding_schemes_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_coding_schemes_request::removals},
        utility::codec::field{"intent", &delete_many_coding_schemes_request::intent},
    };
}

/**
 * @brief The members of delete_many_coding_schemes_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_coding_schemes_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_coding_schemes_response::result},
    };
}

/**
 * @brief The members of list_coding_scheme_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_coding_scheme_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_coding_scheme_versions_request::key},
        utility::codec::field{"offset", &list_coding_scheme_versions_request::offset},
        utility::codec::field{"limit", &list_coding_scheme_versions_request::limit},
        utility::codec::field{"order", &list_coding_scheme_versions_request::order},
        utility::codec::field{"filter", &list_coding_scheme_versions_request::filter},
    };
}

/**
 * @brief The members of list_coding_scheme_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_coding_scheme_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_coding_scheme_versions_response::result},
        utility::codec::field{"versions", &list_coding_scheme_versions_response::versions},
        utility::codec::field{"total", &list_coding_scheme_versions_response::total},
    };
}

/**
 * @brief The members of get_coding_scheme_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_coding_scheme_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_coding_scheme_version_request::key},
    };
}

/**
 * @brief The members of get_coding_scheme_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_coding_scheme_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_coding_scheme_version_response::result},
        utility::codec::field{"version", &get_coding_scheme_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::coding_scheme_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::coding_scheme_key& v);
    static ores::dq::messaging::coding_scheme_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::coding_scheme_key& v);
    static ores::dq::messaging::coding_scheme_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::coding_scheme_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::coding_scheme_write& v);
    static ores::dq::messaging::coding_scheme_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::coding_scheme_write& v);
    static ores::dq::messaging::coding_scheme_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::coding_scheme_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::coding_scheme_change& v);
    static ores::dq::messaging::coding_scheme_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::coding_scheme_change& v);
    static ores::dq::messaging::coding_scheme_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::coding_scheme_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::coding_scheme_removal& v);
    static ores::dq::messaging::coding_scheme_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::coding_scheme_removal& v);
    static ores::dq::messaging::coding_scheme_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::coding_scheme_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::coding_scheme_lookup& v);
    static ores::dq::messaging::coding_scheme_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::coding_scheme_lookup& v);
    static ores::dq::messaging::coding_scheme_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::coding_schemes_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::coding_schemes_filter& v);
    static ores::dq::messaging::coding_schemes_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::coding_schemes_filter& v);
    static ores::dq::messaging::coding_schemes_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::coding_scheme_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::coding_scheme_event& v);
    static ores::dq::messaging::coding_scheme_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::coding_scheme_event& v);
    static ores::dq::messaging::coding_scheme_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::coding_scheme_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::coding_scheme_version_key& v);
    static ores::dq::messaging::coding_scheme_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::coding_scheme_version_key& v);
    static ores::dq::messaging::coding_scheme_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::coding_scheme_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::coding_scheme_versions_filter& v);
    static ores::dq::messaging::coding_scheme_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::coding_scheme_versions_filter& v);
    static ores::dq::messaging::coding_scheme_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_coding_schemes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_coding_schemes_request& v);
    static ores::dq::messaging::list_coding_schemes_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_coding_schemes_request& v);
    static ores::dq::messaging::list_coding_schemes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_coding_schemes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_coding_schemes_response& v);
    static ores::dq::messaging::list_coding_schemes_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_coding_schemes_response& v);
    static ores::dq::messaging::list_coding_schemes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_coding_scheme_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_coding_scheme_request& v);
    static ores::dq::messaging::get_coding_scheme_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_coding_scheme_request& v);
    static ores::dq::messaging::get_coding_scheme_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_coding_scheme_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_coding_scheme_response& v);
    static ores::dq::messaging::get_coding_scheme_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_coding_scheme_response& v);
    static ores::dq::messaging::get_coding_scheme_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_coding_schemes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_coding_schemes_request& v);
    static ores::dq::messaging::get_many_coding_schemes_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_many_coding_schemes_request& v);
    static ores::dq::messaging::get_many_coding_schemes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_coding_schemes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_coding_schemes_response& v);
    static ores::dq::messaging::get_many_coding_schemes_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_many_coding_schemes_response& v);
    static ores::dq::messaging::get_many_coding_schemes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_coding_scheme_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_coding_scheme_request& v);
    static ores::dq::messaging::put_coding_scheme_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_coding_scheme_request& v);
    static ores::dq::messaging::put_coding_scheme_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_coding_scheme_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_coding_scheme_response& v);
    static ores::dq::messaging::put_coding_scheme_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_coding_scheme_response& v);
    static ores::dq::messaging::put_coding_scheme_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_coding_schemes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_coding_schemes_request& v);
    static ores::dq::messaging::put_many_coding_schemes_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_many_coding_schemes_request& v);
    static ores::dq::messaging::put_many_coding_schemes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_coding_schemes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_coding_schemes_response& v);
    static ores::dq::messaging::put_many_coding_schemes_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::put_many_coding_schemes_response& v);
    static ores::dq::messaging::put_many_coding_schemes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_coding_scheme_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_coding_scheme_request& v);
    static ores::dq::messaging::delete_coding_scheme_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_coding_scheme_request& v);
    static ores::dq::messaging::delete_coding_scheme_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_coding_scheme_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_coding_scheme_response& v);
    static ores::dq::messaging::delete_coding_scheme_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_coding_scheme_response& v);
    static ores::dq::messaging::delete_coding_scheme_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_coding_schemes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_many_coding_schemes_request& v);
    static ores::dq::messaging::delete_many_coding_schemes_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_coding_schemes_request& v);
    static ores::dq::messaging::delete_many_coding_schemes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_coding_schemes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::delete_many_coding_schemes_response& v);
    static ores::dq::messaging::delete_many_coding_schemes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_coding_schemes_response& v);
    static ores::dq::messaging::delete_many_coding_schemes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_coding_scheme_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_coding_scheme_versions_request& v);
    static ores::dq::messaging::list_coding_scheme_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_coding_scheme_versions_request& v);
    static ores::dq::messaging::list_coding_scheme_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_coding_scheme_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_coding_scheme_versions_response& v);
    static ores::dq::messaging::list_coding_scheme_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_coding_scheme_versions_response& v);
    static ores::dq::messaging::list_coding_scheme_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_coding_scheme_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_coding_scheme_version_request& v);
    static ores::dq::messaging::get_coding_scheme_version_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_coding_scheme_version_request& v);
    static ores::dq::messaging::get_coding_scheme_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_coding_scheme_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_coding_scheme_version_response& v);
    static ores::dq::messaging::get_coding_scheme_version_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_coding_scheme_version_response& v);
    static ores::dq::messaging::get_coding_scheme_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
