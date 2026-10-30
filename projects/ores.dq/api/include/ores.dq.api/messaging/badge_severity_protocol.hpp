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
#ifndef ORES_DQ_API_MESSAGING_BADGE_SEVERITY_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_BADGE_SEVERITY_PROTOCOL_HPP

#include "ores.dq.api/domain/badge_severity.hpp"
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

struct badge_severity_key {
    std::string code;
};

struct badge_severity_write {
    std::string code;
    std::string name;
    std::string description;
    int display_order;
};

struct badge_severity_change {
    badge_severity_write write;
    ores::utility::domain::precondition precondition;
};

struct badge_severity_removal {
    badge_severity_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct badge_severity_lookup {
    badge_severity_key key;
    std::optional<ores::dq::domain::badge_severity> badge_severity;
};

struct badge_severities_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct badge_severity_event {
    boost::uuids::uuid event_id;
    badge_severity_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct badge_severity_version_key {
    badge_severity_key badge_severity;
    std::uint32_t version;
};

struct badge_severity_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_badge_severities_request {
    using response_type = struct list_badge_severities_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_severities.list";
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
    std::optional<badge_severities_filter> filter;
    std::optional<std::string> as_of;
};

struct list_badge_severities_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::badge_severity> severities;
    std::uint64_t total;
};

struct get_badge_severity_request {
    using response_type = struct get_badge_severity_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_severities.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    badge_severity_key key;
};

struct get_badge_severity_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::badge_severity> badge_severity;
};

struct get_many_badge_severities_request {
    using response_type = struct get_many_badge_severities_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_severities.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<badge_severity_key> keys;
};

struct get_many_badge_severities_response {
    ores::utility::domain::result result;
    std::vector<badge_severity_lookup> entries;
};

struct put_badge_severity_request {
    using response_type = struct put_badge_severity_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_severities.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    badge_severity_change change;
    ores::utility::domain::change_intent intent;
};

struct put_badge_severity_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::badge_severity> badge_severity;
};

struct put_many_badge_severities_request {
    using response_type = struct put_many_badge_severities_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_severities.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<badge_severity_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_badge_severities_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::badge_severity> severities;
};

struct delete_badge_severity_request {
    using response_type = struct delete_badge_severity_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_severities.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    badge_severity_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_badge_severity_response {
    ores::utility::domain::result result;
};

struct delete_many_badge_severities_request {
    using response_type = struct delete_many_badge_severities_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_severities.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<badge_severity_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_badge_severities_response {
    ores::utility::domain::result result;
};

struct list_badge_severity_versions_request {
    using response_type = struct list_badge_severity_versions_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_severities_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    badge_severity_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<badge_severity_versions_filter> filter;
};

struct list_badge_severity_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::badge_severity> versions;
    std::uint64_t total;
};

struct get_badge_severity_version_request {
    using response_type = struct get_badge_severity_version_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_severities_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    badge_severity_version_key key;
};

struct get_badge_severity_version_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::badge_severity> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace badge_severity_event_subjects {
inline constexpr std::string_view created = "dq.v1.badge_severities_events.created";
inline constexpr std::string_view updated = "dq.v1.badge_severities_events.updated";
inline constexpr std::string_view deleted = "dq.v1.badge_severities_events.deleted";
}

/**
 * @brief The members of badge_severity_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const badge_severity_key&) {
    return std::tuple{
        utility::codec::field{"code", &badge_severity_key::code},
    };
}

/**
 * @brief The members of badge_severity_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const badge_severity_write&) {
    return std::tuple{
        utility::codec::field{"code", &badge_severity_write::code},
        utility::codec::field{"name", &badge_severity_write::name},
        utility::codec::field{"description", &badge_severity_write::description},
        utility::codec::field{"display_order", &badge_severity_write::display_order},
    };
}

/**
 * @brief The members of badge_severity_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const badge_severity_change&) {
    return std::tuple{
        utility::codec::field{"write", &badge_severity_change::write},
        utility::codec::field{"precondition", &badge_severity_change::precondition},
    };
}

/**
 * @brief The members of badge_severity_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const badge_severity_removal&) {
    return std::tuple{
        utility::codec::field{"key", &badge_severity_removal::key},
        utility::codec::field{"precondition", &badge_severity_removal::precondition},
    };
}

/**
 * @brief The members of badge_severity_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const badge_severity_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &badge_severity_lookup::key},
        utility::codec::field{"badge_severity", &badge_severity_lookup::badge_severity},
    };
}

/**
 * @brief The members of badge_severities_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const badge_severities_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &badge_severities_filter::code_one_of},
    };
}

/**
 * @brief The members of badge_severity_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const badge_severity_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &badge_severity_event::event_id},
        utility::codec::field{"key", &badge_severity_event::key},
        utility::codec::field{"action", &badge_severity_event::action},
        utility::codec::field{"version", &badge_severity_event::version},
        utility::codec::field{"occurred_at", &badge_severity_event::occurred_at},
        utility::codec::field{"correlation_id", &badge_severity_event::correlation_id},
    };
}

/**
 * @brief The members of badge_severity_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const badge_severity_version_key&) {
    return std::tuple{
        utility::codec::field{"badge_severity", &badge_severity_version_key::badge_severity},
        utility::codec::field{"version", &badge_severity_version_key::version},
    };
}

/**
 * @brief The members of badge_severity_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const badge_severity_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &badge_severity_versions_filter::version},
        utility::codec::field{"from_version", &badge_severity_versions_filter::from_version},
        utility::codec::field{"to_version", &badge_severity_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_badge_severities_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_badge_severities_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_badge_severities_request::offset},
        utility::codec::field{"limit", &list_badge_severities_request::limit},
        utility::codec::field{"order", &list_badge_severities_request::order},
        utility::codec::field{"filter", &list_badge_severities_request::filter},
        utility::codec::field{"as_of", &list_badge_severities_request::as_of},
    };
}

/**
 * @brief The members of list_badge_severities_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_badge_severities_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_badge_severities_response::result},
        utility::codec::field{"severities", &list_badge_severities_response::severities},
        utility::codec::field{"total", &list_badge_severities_response::total},
    };
}

/**
 * @brief The members of get_badge_severity_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_badge_severity_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_badge_severity_request::key},
    };
}

/**
 * @brief The members of get_badge_severity_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_badge_severity_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_badge_severity_response::result},
        utility::codec::field{"badge_severity", &get_badge_severity_response::badge_severity},
    };
}

/**
 * @brief The members of get_many_badge_severities_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_badge_severities_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_badge_severities_request::keys},
    };
}

/**
 * @brief The members of get_many_badge_severities_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_badge_severities_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_badge_severities_response::result},
        utility::codec::field{"entries", &get_many_badge_severities_response::entries},
    };
}

/**
 * @brief The members of put_badge_severity_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_badge_severity_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_badge_severity_request::change},
        utility::codec::field{"intent", &put_badge_severity_request::intent},
    };
}

/**
 * @brief The members of put_badge_severity_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_badge_severity_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_badge_severity_response::result},
        utility::codec::field{"badge_severity", &put_badge_severity_response::badge_severity},
    };
}

/**
 * @brief The members of put_many_badge_severities_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_badge_severities_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_badge_severities_request::changes},
        utility::codec::field{"intent", &put_many_badge_severities_request::intent},
    };
}

/**
 * @brief The members of put_many_badge_severities_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_badge_severities_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_badge_severities_response::result},
        utility::codec::field{"severities", &put_many_badge_severities_response::severities},
    };
}

/**
 * @brief The members of delete_badge_severity_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_badge_severity_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_badge_severity_request::removal},
        utility::codec::field{"intent", &delete_badge_severity_request::intent},
    };
}

/**
 * @brief The members of delete_badge_severity_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_badge_severity_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_badge_severity_response::result},
    };
}

/**
 * @brief The members of delete_many_badge_severities_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_badge_severities_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_badge_severities_request::removals},
        utility::codec::field{"intent", &delete_many_badge_severities_request::intent},
    };
}

/**
 * @brief The members of delete_many_badge_severities_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_badge_severities_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_badge_severities_response::result},
    };
}

/**
 * @brief The members of list_badge_severity_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_badge_severity_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_badge_severity_versions_request::key},
        utility::codec::field{"offset", &list_badge_severity_versions_request::offset},
        utility::codec::field{"limit", &list_badge_severity_versions_request::limit},
        utility::codec::field{"order", &list_badge_severity_versions_request::order},
        utility::codec::field{"filter", &list_badge_severity_versions_request::filter},
    };
}

/**
 * @brief The members of list_badge_severity_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_badge_severity_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_badge_severity_versions_response::result},
        utility::codec::field{"versions", &list_badge_severity_versions_response::versions},
        utility::codec::field{"total", &list_badge_severity_versions_response::total},
    };
}

/**
 * @brief The members of get_badge_severity_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_badge_severity_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_badge_severity_version_request::key},
    };
}

/**
 * @brief The members of get_badge_severity_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_badge_severity_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_badge_severity_version_response::result},
        utility::codec::field{"version", &get_badge_severity_version_response::version},
    };
}

}

namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::badge_severity_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::badge_severity_key& v);
    static ores::dq::messaging::badge_severity_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::badge_severity_key& v);
    static ores::dq::messaging::badge_severity_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::badge_severity_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::badge_severity_write& v);
    static ores::dq::messaging::badge_severity_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::badge_severity_write& v);
    static ores::dq::messaging::badge_severity_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::badge_severity_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::badge_severity_change& v);
    static ores::dq::messaging::badge_severity_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::badge_severity_change& v);
    static ores::dq::messaging::badge_severity_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::badge_severity_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::badge_severity_removal& v);
    static ores::dq::messaging::badge_severity_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::badge_severity_removal& v);
    static ores::dq::messaging::badge_severity_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::badge_severity_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::badge_severity_lookup& v);
    static ores::dq::messaging::badge_severity_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::badge_severity_lookup& v);
    static ores::dq::messaging::badge_severity_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::badge_severities_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::badge_severities_filter& v);
    static ores::dq::messaging::badge_severities_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::badge_severities_filter& v);
    static ores::dq::messaging::badge_severities_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::badge_severity_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::badge_severity_event& v);
    static ores::dq::messaging::badge_severity_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::badge_severity_event& v);
    static ores::dq::messaging::badge_severity_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::badge_severity_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::badge_severity_version_key& v);
    static ores::dq::messaging::badge_severity_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::badge_severity_version_key& v);
    static ores::dq::messaging::badge_severity_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::badge_severity_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::badge_severity_versions_filter& v);
    static ores::dq::messaging::badge_severity_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::badge_severity_versions_filter& v);
    static ores::dq::messaging::badge_severity_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_badge_severities_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_badge_severities_request& v);
    static ores::dq::messaging::list_badge_severities_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_badge_severities_request& v);
    static ores::dq::messaging::list_badge_severities_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_badge_severities_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_badge_severities_response& v);
    static ores::dq::messaging::list_badge_severities_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_badge_severities_response& v);
    static ores::dq::messaging::list_badge_severities_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_badge_severity_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_badge_severity_request& v);
    static ores::dq::messaging::get_badge_severity_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_badge_severity_request& v);
    static ores::dq::messaging::get_badge_severity_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_badge_severity_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_badge_severity_response& v);
    static ores::dq::messaging::get_badge_severity_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_badge_severity_response& v);
    static ores::dq::messaging::get_badge_severity_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_badge_severities_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_badge_severities_request& v);
    static ores::dq::messaging::get_many_badge_severities_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_many_badge_severities_request& v);
    static ores::dq::messaging::get_many_badge_severities_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_badge_severities_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_badge_severities_response& v);
    static ores::dq::messaging::get_many_badge_severities_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_many_badge_severities_response& v);
    static ores::dq::messaging::get_many_badge_severities_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_badge_severity_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_badge_severity_request& v);
    static ores::dq::messaging::put_badge_severity_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_badge_severity_request& v);
    static ores::dq::messaging::put_badge_severity_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_badge_severity_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_badge_severity_response& v);
    static ores::dq::messaging::put_badge_severity_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_badge_severity_response& v);
    static ores::dq::messaging::put_badge_severity_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_badge_severities_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_badge_severities_request& v);
    static ores::dq::messaging::put_many_badge_severities_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::put_many_badge_severities_request& v);
    static ores::dq::messaging::put_many_badge_severities_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_badge_severities_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_badge_severities_response& v);
    static ores::dq::messaging::put_many_badge_severities_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::put_many_badge_severities_response& v);
    static ores::dq::messaging::put_many_badge_severities_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_badge_severity_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_badge_severity_request& v);
    static ores::dq::messaging::delete_badge_severity_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_badge_severity_request& v);
    static ores::dq::messaging::delete_badge_severity_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_badge_severity_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_badge_severity_response& v);
    static ores::dq::messaging::delete_badge_severity_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_badge_severity_response& v);
    static ores::dq::messaging::delete_badge_severity_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_badge_severities_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::delete_many_badge_severities_request& v);
    static ores::dq::messaging::delete_many_badge_severities_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_badge_severities_request& v);
    static ores::dq::messaging::delete_many_badge_severities_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_badge_severities_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::delete_many_badge_severities_response& v);
    static ores::dq::messaging::delete_many_badge_severities_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_badge_severities_response& v);
    static ores::dq::messaging::delete_many_badge_severities_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_badge_severity_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_badge_severity_versions_request& v);
    static ores::dq::messaging::list_badge_severity_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_badge_severity_versions_request& v);
    static ores::dq::messaging::list_badge_severity_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_badge_severity_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_badge_severity_versions_response& v);
    static ores::dq::messaging::list_badge_severity_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_badge_severity_versions_response& v);
    static ores::dq::messaging::list_badge_severity_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_badge_severity_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_badge_severity_version_request& v);
    static ores::dq::messaging::get_badge_severity_version_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_badge_severity_version_request& v);
    static ores::dq::messaging::get_badge_severity_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_badge_severity_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::get_badge_severity_version_response& v);
    static ores::dq::messaging::get_badge_severity_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_badge_severity_version_response& v);
    static ores::dq::messaging::get_badge_severity_version_response
    read_msgpack(std::span<const std::byte> data);
};
}

#endif
