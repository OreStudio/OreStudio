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
#ifndef ORES_REPORTING_API_MESSAGING_ANALYTIC_TYPE_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_ANALYTIC_TYPE_PROTOCOL_HPP

#include "ores.reporting.api/domain/analytic_type.hpp"
#include "ores.reporting.api/export.hpp"
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

namespace ores::reporting::messaging {

struct analytic_type_key {
    std::string code;

    friend bool operator==(const analytic_type_key&, const analytic_type_key&) = default;
};

struct analytic_type_write {
    std::string code;
    std::string name;
    std::string description;
    std::optional<std::string> parameter_entity;
    std::optional<std::string> requires_analytic_type_code;
    int display_order;

    friend bool operator==(const analytic_type_write&, const analytic_type_write&) = default;
};

struct analytic_type_change {
    analytic_type_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const analytic_type_change&, const analytic_type_change&) = default;
};

struct analytic_type_removal {
    analytic_type_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const analytic_type_removal&, const analytic_type_removal&) = default;
};

struct analytic_type_lookup {
    analytic_type_key key;
    std::optional<ores::reporting::domain::analytic_type> analytic_type;

    friend bool operator==(const analytic_type_lookup&, const analytic_type_lookup&) = default;
};

struct analytic_types_filter {
    std::optional<std::vector<std::string>> code_one_of;

    friend bool operator==(const analytic_types_filter&, const analytic_types_filter&) = default;
};

struct analytic_type_event {
    boost::uuids::uuid event_id;
    analytic_type_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const analytic_type_event&, const analytic_type_event&) = default;
};

struct analytic_type_version_key {
    analytic_type_key analytic_type;
    std::uint32_t version;

    friend bool operator==(const analytic_type_version_key&,
                           const analytic_type_version_key&) = default;
};

struct analytic_type_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const analytic_type_versions_filter&,
                           const analytic_type_versions_filter&) = default;
};

struct list_analytic_types_request {
    using response_type = struct list_analytic_types_response;
    static constexpr std::string_view nats_subject = "reporting.v1.analytic_types.list";
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
    std::optional<analytic_types_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_analytic_types_request&,
                           const list_analytic_types_request&) = default;
};

struct list_analytic_types_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::analytic_type> types;
    std::uint64_t total;

    friend bool operator==(const list_analytic_types_response&,
                           const list_analytic_types_response&) = default;
};

struct get_analytic_type_request {
    using response_type = struct get_analytic_type_response;
    static constexpr std::string_view nats_subject = "reporting.v1.analytic_types.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    analytic_type_key key;

    friend bool operator==(const get_analytic_type_request&,
                           const get_analytic_type_request&) = default;
};

struct get_analytic_type_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::analytic_type> analytic_type;

    friend bool operator==(const get_analytic_type_response&,
                           const get_analytic_type_response&) = default;
};

struct get_many_analytic_types_request {
    using response_type = struct get_many_analytic_types_response;
    static constexpr std::string_view nats_subject = "reporting.v1.analytic_types.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<analytic_type_key> keys;

    friend bool operator==(const get_many_analytic_types_request&,
                           const get_many_analytic_types_request&) = default;
};

struct get_many_analytic_types_response {
    ores::utility::domain::result result;
    std::vector<analytic_type_lookup> entries;

    friend bool operator==(const get_many_analytic_types_response&,
                           const get_many_analytic_types_response&) = default;
};

struct put_analytic_type_request {
    using response_type = struct put_analytic_type_response;
    static constexpr std::string_view nats_subject = "reporting.v1.analytic_types.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    analytic_type_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_analytic_type_request&,
                           const put_analytic_type_request&) = default;
};

struct put_analytic_type_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::analytic_type> analytic_type;

    friend bool operator==(const put_analytic_type_response&,
                           const put_analytic_type_response&) = default;
};

struct put_many_analytic_types_request {
    using response_type = struct put_many_analytic_types_response;
    static constexpr std::string_view nats_subject = "reporting.v1.analytic_types.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<analytic_type_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_analytic_types_request&,
                           const put_many_analytic_types_request&) = default;
};

struct put_many_analytic_types_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::analytic_type> types;

    friend bool operator==(const put_many_analytic_types_response&,
                           const put_many_analytic_types_response&) = default;
};

struct delete_analytic_type_request {
    using response_type = struct delete_analytic_type_response;
    static constexpr std::string_view nats_subject = "reporting.v1.analytic_types.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    analytic_type_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_analytic_type_request&,
                           const delete_analytic_type_request&) = default;
};

struct delete_analytic_type_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_analytic_type_response&,
                           const delete_analytic_type_response&) = default;
};

struct delete_many_analytic_types_request {
    using response_type = struct delete_many_analytic_types_response;
    static constexpr std::string_view nats_subject = "reporting.v1.analytic_types.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<analytic_type_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_analytic_types_request&,
                           const delete_many_analytic_types_request&) = default;
};

struct delete_many_analytic_types_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_analytic_types_response&,
                           const delete_many_analytic_types_response&) = default;
};

struct list_analytic_type_versions_request {
    using response_type = struct list_analytic_type_versions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.analytic_types_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    analytic_type_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<analytic_type_versions_filter> filter;

    friend bool operator==(const list_analytic_type_versions_request&,
                           const list_analytic_type_versions_request&) = default;
};

struct list_analytic_type_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::analytic_type> versions;
    std::uint64_t total;

    friend bool operator==(const list_analytic_type_versions_response&,
                           const list_analytic_type_versions_response&) = default;
};

struct get_analytic_type_version_request {
    using response_type = struct get_analytic_type_version_response;
    static constexpr std::string_view nats_subject = "reporting.v1.analytic_types_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    analytic_type_version_key key;

    friend bool operator==(const get_analytic_type_version_request&,
                           const get_analytic_type_version_request&) = default;
};

struct get_analytic_type_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::analytic_type> version;

    friend bool operator==(const get_analytic_type_version_response&,
                           const get_analytic_type_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace analytic_type_event_subjects {
inline constexpr std::string_view created = "reporting.v1.analytic_types_events.created";
inline constexpr std::string_view updated = "reporting.v1.analytic_types_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.analytic_types_events.deleted";
}
/**
 * @brief The members of analytic_type_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const analytic_type_key&) {
    return std::tuple{
        utility::codec::field{"code", &analytic_type_key::code},
    };
}

/**
 * @brief The members of analytic_type_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const analytic_type_write&) {
    return std::tuple{
        utility::codec::field{"code", &analytic_type_write::code},
        utility::codec::field{"name", &analytic_type_write::name},
        utility::codec::field{"description", &analytic_type_write::description},
        utility::codec::field{"parameter_entity", &analytic_type_write::parameter_entity},
        utility::codec::field{"requires_analytic_type_code",
                              &analytic_type_write::requires_analytic_type_code},
        utility::codec::field{"display_order", &analytic_type_write::display_order},
    };
}

/**
 * @brief The members of analytic_type_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const analytic_type_change&) {
    return std::tuple{
        utility::codec::field{"write", &analytic_type_change::write},
        utility::codec::field{"precondition", &analytic_type_change::precondition},
    };
}

/**
 * @brief The members of analytic_type_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const analytic_type_removal&) {
    return std::tuple{
        utility::codec::field{"key", &analytic_type_removal::key},
        utility::codec::field{"precondition", &analytic_type_removal::precondition},
    };
}

/**
 * @brief The members of analytic_type_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const analytic_type_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &analytic_type_lookup::key},
        utility::codec::field{"analytic_type", &analytic_type_lookup::analytic_type},
    };
}

/**
 * @brief The members of analytic_types_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const analytic_types_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &analytic_types_filter::code_one_of},
    };
}

/**
 * @brief The members of analytic_type_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const analytic_type_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &analytic_type_event::event_id},
        utility::codec::field{"key", &analytic_type_event::key},
        utility::codec::field{"action", &analytic_type_event::action},
        utility::codec::field{"version", &analytic_type_event::version},
        utility::codec::field{"occurred_at", &analytic_type_event::occurred_at},
        utility::codec::field{"correlation_id", &analytic_type_event::correlation_id},
    };
}

/**
 * @brief The members of analytic_type_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const analytic_type_version_key&) {
    return std::tuple{
        utility::codec::field{"analytic_type", &analytic_type_version_key::analytic_type},
        utility::codec::field{"version", &analytic_type_version_key::version},
    };
}

/**
 * @brief The members of analytic_type_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const analytic_type_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &analytic_type_versions_filter::version},
        utility::codec::field{"from_version", &analytic_type_versions_filter::from_version},
        utility::codec::field{"to_version", &analytic_type_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_analytic_types_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_analytic_types_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_analytic_types_request::offset},
        utility::codec::field{"limit", &list_analytic_types_request::limit},
        utility::codec::field{"order", &list_analytic_types_request::order},
        utility::codec::field{"filter", &list_analytic_types_request::filter},
        utility::codec::field{"as_of", &list_analytic_types_request::as_of},
    };
}

/**
 * @brief The members of list_analytic_types_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_analytic_types_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_analytic_types_response::result},
        utility::codec::field{"types", &list_analytic_types_response::types},
        utility::codec::field{"total", &list_analytic_types_response::total},
    };
}

/**
 * @brief The members of get_analytic_type_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_analytic_type_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_analytic_type_request::key},
    };
}

/**
 * @brief The members of get_analytic_type_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_analytic_type_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_analytic_type_response::result},
        utility::codec::field{"analytic_type", &get_analytic_type_response::analytic_type},
    };
}

/**
 * @brief The members of get_many_analytic_types_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_analytic_types_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_analytic_types_request::keys},
    };
}

/**
 * @brief The members of get_many_analytic_types_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_analytic_types_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_analytic_types_response::result},
        utility::codec::field{"entries", &get_many_analytic_types_response::entries},
    };
}

/**
 * @brief The members of put_analytic_type_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_analytic_type_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_analytic_type_request::change},
        utility::codec::field{"intent", &put_analytic_type_request::intent},
    };
}

/**
 * @brief The members of put_analytic_type_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_analytic_type_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_analytic_type_response::result},
        utility::codec::field{"analytic_type", &put_analytic_type_response::analytic_type},
    };
}

/**
 * @brief The members of put_many_analytic_types_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_analytic_types_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_analytic_types_request::changes},
        utility::codec::field{"intent", &put_many_analytic_types_request::intent},
    };
}

/**
 * @brief The members of put_many_analytic_types_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_analytic_types_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_analytic_types_response::result},
        utility::codec::field{"types", &put_many_analytic_types_response::types},
    };
}

/**
 * @brief The members of delete_analytic_type_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_analytic_type_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_analytic_type_request::removal},
        utility::codec::field{"intent", &delete_analytic_type_request::intent},
    };
}

/**
 * @brief The members of delete_analytic_type_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_analytic_type_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_analytic_type_response::result},
    };
}

/**
 * @brief The members of delete_many_analytic_types_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_analytic_types_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_analytic_types_request::removals},
        utility::codec::field{"intent", &delete_many_analytic_types_request::intent},
    };
}

/**
 * @brief The members of delete_many_analytic_types_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_analytic_types_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_analytic_types_response::result},
    };
}

/**
 * @brief The members of list_analytic_type_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_analytic_type_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_analytic_type_versions_request::key},
        utility::codec::field{"offset", &list_analytic_type_versions_request::offset},
        utility::codec::field{"limit", &list_analytic_type_versions_request::limit},
        utility::codec::field{"order", &list_analytic_type_versions_request::order},
        utility::codec::field{"filter", &list_analytic_type_versions_request::filter},
    };
}

/**
 * @brief The members of list_analytic_type_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_analytic_type_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_analytic_type_versions_response::result},
        utility::codec::field{"versions", &list_analytic_type_versions_response::versions},
        utility::codec::field{"total", &list_analytic_type_versions_response::total},
    };
}

/**
 * @brief The members of get_analytic_type_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_analytic_type_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_analytic_type_version_request::key},
    };
}

/**
 * @brief The members of get_analytic_type_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_analytic_type_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_analytic_type_version_response::result},
        utility::codec::field{"version", &get_analytic_type_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::analytic_type_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::analytic_type_key& v);
    static ores::reporting::messaging::analytic_type_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::analytic_type_key& v);
    static ores::reporting::messaging::analytic_type_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::analytic_type_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::analytic_type_write& v);
    static ores::reporting::messaging::analytic_type_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::analytic_type_write& v);
    static ores::reporting::messaging::analytic_type_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::analytic_type_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::analytic_type_change& v);
    static ores::reporting::messaging::analytic_type_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::analytic_type_change& v);
    static ores::reporting::messaging::analytic_type_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::analytic_type_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::analytic_type_removal& v);
    static ores::reporting::messaging::analytic_type_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::analytic_type_removal& v);
    static ores::reporting::messaging::analytic_type_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::analytic_type_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::analytic_type_lookup& v);
    static ores::reporting::messaging::analytic_type_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::analytic_type_lookup& v);
    static ores::reporting::messaging::analytic_type_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::analytic_types_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::analytic_types_filter& v);
    static ores::reporting::messaging::analytic_types_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::analytic_types_filter& v);
    static ores::reporting::messaging::analytic_types_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::analytic_type_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::analytic_type_event& v);
    static ores::reporting::messaging::analytic_type_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::analytic_type_event& v);
    static ores::reporting::messaging::analytic_type_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::analytic_type_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::analytic_type_version_key& v);
    static ores::reporting::messaging::analytic_type_version_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::analytic_type_version_key& v);
    static ores::reporting::messaging::analytic_type_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::analytic_type_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::analytic_type_versions_filter& v);
    static ores::reporting::messaging::analytic_type_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::analytic_type_versions_filter& v);
    static ores::reporting::messaging::analytic_type_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_analytic_types_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::list_analytic_types_request& v);
    static ores::reporting::messaging::list_analytic_types_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_analytic_types_request& v);
    static ores::reporting::messaging::list_analytic_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_analytic_types_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_analytic_types_response& v);
    static ores::reporting::messaging::list_analytic_types_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_analytic_types_response& v);
    static ores::reporting::messaging::list_analytic_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_analytic_type_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_analytic_type_request& v);
    static ores::reporting::messaging::get_analytic_type_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_analytic_type_request& v);
    static ores::reporting::messaging::get_analytic_type_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_analytic_type_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_analytic_type_response& v);
    static ores::reporting::messaging::get_analytic_type_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_analytic_type_response& v);
    static ores::reporting::messaging::get_analytic_type_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_analytic_types_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_analytic_types_request& v);
    static ores::reporting::messaging::get_many_analytic_types_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_analytic_types_request& v);
    static ores::reporting::messaging::get_many_analytic_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_analytic_types_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_analytic_types_response& v);
    static ores::reporting::messaging::get_many_analytic_types_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_analytic_types_response& v);
    static ores::reporting::messaging::get_many_analytic_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_analytic_type_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_analytic_type_request& v);
    static ores::reporting::messaging::put_analytic_type_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_analytic_type_request& v);
    static ores::reporting::messaging::put_analytic_type_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_analytic_type_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_analytic_type_response& v);
    static ores::reporting::messaging::put_analytic_type_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_analytic_type_response& v);
    static ores::reporting::messaging::put_analytic_type_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_analytic_types_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_analytic_types_request& v);
    static ores::reporting::messaging::put_many_analytic_types_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_analytic_types_request& v);
    static ores::reporting::messaging::put_many_analytic_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_analytic_types_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_analytic_types_response& v);
    static ores::reporting::messaging::put_many_analytic_types_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_analytic_types_response& v);
    static ores::reporting::messaging::put_many_analytic_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_analytic_type_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_analytic_type_request& v);
    static ores::reporting::messaging::delete_analytic_type_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_analytic_type_request& v);
    static ores::reporting::messaging::delete_analytic_type_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_analytic_type_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_analytic_type_response& v);
    static ores::reporting::messaging::delete_analytic_type_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_analytic_type_response& v);
    static ores::reporting::messaging::delete_analytic_type_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_analytic_types_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_analytic_types_request& v);
    static ores::reporting::messaging::delete_many_analytic_types_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_analytic_types_request& v);
    static ores::reporting::messaging::delete_many_analytic_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_analytic_types_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_analytic_types_response& v);
    static ores::reporting::messaging::delete_many_analytic_types_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_analytic_types_response& v);
    static ores::reporting::messaging::delete_many_analytic_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_analytic_type_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_analytic_type_versions_request& v);
    static ores::reporting::messaging::list_analytic_type_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_analytic_type_versions_request& v);
    static ores::reporting::messaging::list_analytic_type_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_analytic_type_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_analytic_type_versions_response& v);
    static ores::reporting::messaging::list_analytic_type_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_analytic_type_versions_response& v);
    static ores::reporting::messaging::list_analytic_type_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_analytic_type_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_analytic_type_version_request& v);
    static ores::reporting::messaging::get_analytic_type_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_analytic_type_version_request& v);
    static ores::reporting::messaging::get_analytic_type_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_analytic_type_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_analytic_type_version_response& v);
    static ores::reporting::messaging::get_analytic_type_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_analytic_type_version_response& v);
    static ores::reporting::messaging::get_analytic_type_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
