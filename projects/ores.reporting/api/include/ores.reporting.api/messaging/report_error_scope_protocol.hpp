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
#ifndef ORES_REPORTING_API_MESSAGING_REPORT_ERROR_SCOPE_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_REPORT_ERROR_SCOPE_PROTOCOL_HPP

#include "ores.reporting.api/domain/report_error_scope.hpp"
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

struct report_error_scope_key {
    std::string code;
};

struct report_error_scope_write {
    std::string code;
    std::string name;
    std::string description;
    int display_order;
};

struct report_error_scope_change {
    report_error_scope_write write;
    ores::utility::domain::precondition precondition;
};

struct report_error_scope_removal {
    report_error_scope_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct report_error_scope_lookup {
    report_error_scope_key key;
    std::optional<ores::reporting::domain::report_error_scope> report_error_scope;
};

struct report_error_scopes_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct report_error_scope_event {
    boost::uuids::uuid event_id;
    report_error_scope_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct report_error_scope_version_key {
    report_error_scope_key report_error_scope;
    std::uint32_t version;
};

struct report_error_scope_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_report_error_scopes_request {
    using response_type = struct list_report_error_scopes_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_error_scopes.list";
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
    std::optional<report_error_scopes_filter> filter;
    std::optional<std::string> as_of;
};

struct list_report_error_scopes_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_error_scope> scopes;
    std::uint64_t total;
};

struct get_report_error_scope_request {
    using response_type = struct get_report_error_scope_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_error_scopes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_error_scope_key key;
};

struct get_report_error_scope_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_error_scope> report_error_scope;
};

struct get_many_report_error_scopes_request {
    using response_type = struct get_many_report_error_scopes_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_error_scopes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_error_scope_key> keys;
};

struct get_many_report_error_scopes_response {
    ores::utility::domain::result result;
    std::vector<report_error_scope_lookup> entries;
};

struct put_report_error_scope_request {
    using response_type = struct put_report_error_scope_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_error_scopes.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_error_scope_change change;
    ores::utility::domain::change_intent intent;
};

struct put_report_error_scope_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_error_scope> report_error_scope;
};

struct put_many_report_error_scopes_request {
    using response_type = struct put_many_report_error_scopes_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_error_scopes.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_error_scope_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_report_error_scopes_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_error_scope> scopes;
};

struct delete_report_error_scope_request {
    using response_type = struct delete_report_error_scope_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_error_scopes.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_error_scope_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_report_error_scope_response {
    ores::utility::domain::result result;
};

struct delete_many_report_error_scopes_request {
    using response_type = struct delete_many_report_error_scopes_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_error_scopes.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_error_scope_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_report_error_scopes_response {
    ores::utility::domain::result result;
};

struct list_report_error_scope_versions_request {
    using response_type = struct list_report_error_scope_versions_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report_error_scopes_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_error_scope_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<report_error_scope_versions_filter> filter;
};

struct list_report_error_scope_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_error_scope> versions;
    std::uint64_t total;
};

struct get_report_error_scope_version_request {
    using response_type = struct get_report_error_scope_version_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report_error_scopes_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_error_scope_version_key key;
};

struct get_report_error_scope_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_error_scope> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace report_error_scope_event_subjects {
inline constexpr std::string_view created = "reporting.v1.report_error_scopes_events.created";
inline constexpr std::string_view updated = "reporting.v1.report_error_scopes_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.report_error_scopes_events.deleted";
}
/**
 * @brief The members of report_error_scope_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_error_scope_key&) {
    return std::tuple{
        utility::codec::field{"code", &report_error_scope_key::code},
    };
}

/**
 * @brief The members of report_error_scope_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_error_scope_write&) {
    return std::tuple{
        utility::codec::field{"code", &report_error_scope_write::code},
        utility::codec::field{"name", &report_error_scope_write::name},
        utility::codec::field{"description", &report_error_scope_write::description},
        utility::codec::field{"display_order", &report_error_scope_write::display_order},
    };
}

/**
 * @brief The members of report_error_scope_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_error_scope_change&) {
    return std::tuple{
        utility::codec::field{"write", &report_error_scope_change::write},
        utility::codec::field{"precondition", &report_error_scope_change::precondition},
    };
}

/**
 * @brief The members of report_error_scope_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_error_scope_removal&) {
    return std::tuple{
        utility::codec::field{"key", &report_error_scope_removal::key},
        utility::codec::field{"precondition", &report_error_scope_removal::precondition},
    };
}

/**
 * @brief The members of report_error_scope_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_error_scope_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &report_error_scope_lookup::key},
        utility::codec::field{"report_error_scope", &report_error_scope_lookup::report_error_scope},
    };
}

/**
 * @brief The members of report_error_scopes_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_error_scopes_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &report_error_scopes_filter::code_one_of},
    };
}

/**
 * @brief The members of report_error_scope_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_error_scope_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &report_error_scope_event::event_id},
        utility::codec::field{"key", &report_error_scope_event::key},
        utility::codec::field{"action", &report_error_scope_event::action},
        utility::codec::field{"version", &report_error_scope_event::version},
        utility::codec::field{"occurred_at", &report_error_scope_event::occurred_at},
        utility::codec::field{"correlation_id", &report_error_scope_event::correlation_id},
    };
}

/**
 * @brief The members of report_error_scope_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_error_scope_version_key&) {
    return std::tuple{
        utility::codec::field{"report_error_scope",
                              &report_error_scope_version_key::report_error_scope},
        utility::codec::field{"version", &report_error_scope_version_key::version},
    };
}

/**
 * @brief The members of report_error_scope_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_error_scope_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &report_error_scope_versions_filter::version},
        utility::codec::field{"from_version", &report_error_scope_versions_filter::from_version},
        utility::codec::field{"to_version", &report_error_scope_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_report_error_scopes_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_report_error_scopes_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_report_error_scopes_request::offset},
        utility::codec::field{"limit", &list_report_error_scopes_request::limit},
        utility::codec::field{"order", &list_report_error_scopes_request::order},
        utility::codec::field{"filter", &list_report_error_scopes_request::filter},
        utility::codec::field{"as_of", &list_report_error_scopes_request::as_of},
    };
}

/**
 * @brief The members of list_report_error_scopes_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_report_error_scopes_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_report_error_scopes_response::result},
        utility::codec::field{"scopes", &list_report_error_scopes_response::scopes},
        utility::codec::field{"total", &list_report_error_scopes_response::total},
    };
}

/**
 * @brief The members of get_report_error_scope_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_report_error_scope_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_report_error_scope_request::key},
    };
}

/**
 * @brief The members of get_report_error_scope_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_report_error_scope_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_report_error_scope_response::result},
        utility::codec::field{"report_error_scope",
                              &get_report_error_scope_response::report_error_scope},
    };
}

/**
 * @brief The members of get_many_report_error_scopes_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_report_error_scopes_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_report_error_scopes_request::keys},
    };
}

/**
 * @brief The members of get_many_report_error_scopes_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_report_error_scopes_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_report_error_scopes_response::result},
        utility::codec::field{"entries", &get_many_report_error_scopes_response::entries},
    };
}

/**
 * @brief The members of put_report_error_scope_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_report_error_scope_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_report_error_scope_request::change},
        utility::codec::field{"intent", &put_report_error_scope_request::intent},
    };
}

/**
 * @brief The members of put_report_error_scope_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_report_error_scope_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_report_error_scope_response::result},
        utility::codec::field{"report_error_scope",
                              &put_report_error_scope_response::report_error_scope},
    };
}

/**
 * @brief The members of put_many_report_error_scopes_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_report_error_scopes_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_report_error_scopes_request::changes},
        utility::codec::field{"intent", &put_many_report_error_scopes_request::intent},
    };
}

/**
 * @brief The members of put_many_report_error_scopes_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_report_error_scopes_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_report_error_scopes_response::result},
        utility::codec::field{"scopes", &put_many_report_error_scopes_response::scopes},
    };
}

/**
 * @brief The members of delete_report_error_scope_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_report_error_scope_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_report_error_scope_request::removal},
        utility::codec::field{"intent", &delete_report_error_scope_request::intent},
    };
}

/**
 * @brief The members of delete_report_error_scope_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_report_error_scope_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_report_error_scope_response::result},
    };
}

/**
 * @brief The members of delete_many_report_error_scopes_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_report_error_scopes_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_report_error_scopes_request::removals},
        utility::codec::field{"intent", &delete_many_report_error_scopes_request::intent},
    };
}

/**
 * @brief The members of delete_many_report_error_scopes_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_report_error_scopes_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_report_error_scopes_response::result},
    };
}

/**
 * @brief The members of list_report_error_scope_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_report_error_scope_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_report_error_scope_versions_request::key},
        utility::codec::field{"offset", &list_report_error_scope_versions_request::offset},
        utility::codec::field{"limit", &list_report_error_scope_versions_request::limit},
        utility::codec::field{"order", &list_report_error_scope_versions_request::order},
        utility::codec::field{"filter", &list_report_error_scope_versions_request::filter},
    };
}

/**
 * @brief The members of list_report_error_scope_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_report_error_scope_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_report_error_scope_versions_response::result},
        utility::codec::field{"versions", &list_report_error_scope_versions_response::versions},
        utility::codec::field{"total", &list_report_error_scope_versions_response::total},
    };
}

/**
 * @brief The members of get_report_error_scope_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_report_error_scope_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_report_error_scope_version_request::key},
    };
}

/**
 * @brief The members of get_report_error_scope_version_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_report_error_scope_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_report_error_scope_version_response::result},
        utility::codec::field{"version", &get_report_error_scope_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_error_scope_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_error_scope_key& v);
    static ores::reporting::messaging::report_error_scope_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_error_scope_key& v);
    static ores::reporting::messaging::report_error_scope_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_error_scope_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_error_scope_write& v);
    static ores::reporting::messaging::report_error_scope_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_error_scope_write& v);
    static ores::reporting::messaging::report_error_scope_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::report_error_scope_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_error_scope_change& v);
    static ores::reporting::messaging::report_error_scope_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::report_error_scope_change& v);
    static ores::reporting::messaging::report_error_scope_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::report_error_scope_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_error_scope_removal& v);
    static ores::reporting::messaging::report_error_scope_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::report_error_scope_removal& v);
    static ores::reporting::messaging::report_error_scope_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::report_error_scope_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_error_scope_lookup& v);
    static ores::reporting::messaging::report_error_scope_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::report_error_scope_lookup& v);
    static ores::reporting::messaging::report_error_scope_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::report_error_scopes_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_error_scopes_filter& v);
    static ores::reporting::messaging::report_error_scopes_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::report_error_scopes_filter& v);
    static ores::reporting::messaging::report_error_scopes_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_error_scope_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_error_scope_event& v);
    static ores::reporting::messaging::report_error_scope_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_error_scope_event& v);
    static ores::reporting::messaging::report_error_scope_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::report_error_scope_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::report_error_scope_version_key& v);
    static ores::reporting::messaging::report_error_scope_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::report_error_scope_version_key& v);
    static ores::reporting::messaging::report_error_scope_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::report_error_scope_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::report_error_scope_versions_filter& v);
    static ores::reporting::messaging::report_error_scope_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::report_error_scope_versions_filter& v);
    static ores::reporting::messaging::report_error_scope_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_report_error_scopes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_report_error_scopes_request& v);
    static ores::reporting::messaging::list_report_error_scopes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_report_error_scopes_request& v);
    static ores::reporting::messaging::list_report_error_scopes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_report_error_scopes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_report_error_scopes_response& v);
    static ores::reporting::messaging::list_report_error_scopes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_report_error_scopes_response& v);
    static ores::reporting::messaging::list_report_error_scopes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_error_scope_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_report_error_scope_request& v);
    static ores::reporting::messaging::get_report_error_scope_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_error_scope_request& v);
    static ores::reporting::messaging::get_report_error_scope_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_error_scope_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_report_error_scope_response& v);
    static ores::reporting::messaging::get_report_error_scope_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_error_scope_response& v);
    static ores::reporting::messaging::get_report_error_scope_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_report_error_scopes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_report_error_scopes_request& v);
    static ores::reporting::messaging::get_many_report_error_scopes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_report_error_scopes_request& v);
    static ores::reporting::messaging::get_many_report_error_scopes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_report_error_scopes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_report_error_scopes_response& v);
    static ores::reporting::messaging::get_many_report_error_scopes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_report_error_scopes_response& v);
    static ores::reporting::messaging::get_many_report_error_scopes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_report_error_scope_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_report_error_scope_request& v);
    static ores::reporting::messaging::put_report_error_scope_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_report_error_scope_request& v);
    static ores::reporting::messaging::put_report_error_scope_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_report_error_scope_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_report_error_scope_response& v);
    static ores::reporting::messaging::put_report_error_scope_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_report_error_scope_response& v);
    static ores::reporting::messaging::put_report_error_scope_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_report_error_scopes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_report_error_scopes_request& v);
    static ores::reporting::messaging::put_many_report_error_scopes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_report_error_scopes_request& v);
    static ores::reporting::messaging::put_many_report_error_scopes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_report_error_scopes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_report_error_scopes_response& v);
    static ores::reporting::messaging::put_many_report_error_scopes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_report_error_scopes_response& v);
    static ores::reporting::messaging::put_many_report_error_scopes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_report_error_scope_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_report_error_scope_request& v);
    static ores::reporting::messaging::delete_report_error_scope_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_report_error_scope_request& v);
    static ores::reporting::messaging::delete_report_error_scope_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_report_error_scope_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_report_error_scope_response& v);
    static ores::reporting::messaging::delete_report_error_scope_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_report_error_scope_response& v);
    static ores::reporting::messaging::delete_report_error_scope_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_report_error_scopes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_report_error_scopes_request& v);
    static ores::reporting::messaging::delete_many_report_error_scopes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_report_error_scopes_request& v);
    static ores::reporting::messaging::delete_many_report_error_scopes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_report_error_scopes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_report_error_scopes_response& v);
    static ores::reporting::messaging::delete_many_report_error_scopes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_report_error_scopes_response& v);
    static ores::reporting::messaging::delete_many_report_error_scopes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_report_error_scope_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_report_error_scope_versions_request& v);
    static ores::reporting::messaging::list_report_error_scope_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_report_error_scope_versions_request& v);
    static ores::reporting::messaging::list_report_error_scope_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_report_error_scope_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_report_error_scope_versions_response& v);
    static ores::reporting::messaging::list_report_error_scope_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_report_error_scope_versions_response& v);
    static ores::reporting::messaging::list_report_error_scope_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_error_scope_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_report_error_scope_version_request& v);
    static ores::reporting::messaging::get_report_error_scope_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_error_scope_version_request& v);
    static ores::reporting::messaging::get_report_error_scope_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_error_scope_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_report_error_scope_version_response& v);
    static ores::reporting::messaging::get_report_error_scope_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_error_scope_version_response& v);
    static ores::reporting::messaging::get_report_error_scope_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
