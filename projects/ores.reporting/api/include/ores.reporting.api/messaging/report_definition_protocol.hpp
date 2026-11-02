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
#ifndef ORES_REPORTING_API_MESSAGING_REPORT_DEFINITION_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_REPORT_DEFINITION_PROTOCOL_HPP

#include "ores.reporting.api/domain/report_definition.hpp"
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

struct report_definition_key {
    std::string name;
};

struct report_definition_write {
    boost::uuids::uuid id;
    std::string name;
    boost::uuids::uuid party_id;
    std::string description;
    std::string report_type;
    std::optional<boost::uuids::uuid> fsm_state_id;
    std::string schedule_expression;
    std::string concurrency_policy;
    std::optional<boost::uuids::uuid> scheduler_job_id;
    std::optional<boost::uuids::uuid> run_grant_id;
    std::string pre_processing;
    std::string prepared_input_key;
    std::string post_processing;
    std::string skipped_families;
    bool is_official;
    std::optional<std::string> regulatory_book_type;
    std::optional<std::string> cut_kind_code;
    std::optional<boost::uuids::uuid> cut_id;
};

struct report_definition_change {
    report_definition_write write;
    ores::utility::domain::precondition precondition;
};

struct report_definition_removal {
    report_definition_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct report_definition_lookup {
    report_definition_key key;
    std::optional<ores::reporting::domain::report_definition> report_definition;
};

struct report_definitions_filter {
    std::optional<boost::uuids::uuid> party_id;
    std::optional<std::optional<std::string>> cut_kind_code;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> party_id_one_of;
    std::optional<std::vector<std::string>> cut_kind_code_one_of;
    std::optional<std::string> search;
};

struct report_definition_event {
    boost::uuids::uuid event_id;
    report_definition_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct report_definition_version_key {
    report_definition_key report_definition;
    std::uint32_t version;
};

struct report_definition_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_report_definitions_request {
    using response_type = struct list_report_definitions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_definitions.list";
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
    std::optional<report_definitions_filter> filter;
    std::optional<std::string> as_of;
};

struct list_report_definitions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_definition> definitions;
    std::uint64_t total;
};

struct get_report_definition_request {
    using response_type = struct get_report_definition_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_definitions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_definition_key key;
};

struct get_report_definition_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_definition> report_definition;
};

struct get_many_report_definitions_request {
    using response_type = struct get_many_report_definitions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_definitions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_definition_key> keys;
};

struct get_many_report_definitions_response {
    ores::utility::domain::result result;
    std::vector<report_definition_lookup> entries;
};

struct put_report_definition_request {
    using response_type = struct put_report_definition_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_definitions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_definition_change change;
    ores::utility::domain::change_intent intent;
};

struct put_report_definition_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_definition> report_definition;
};

struct put_many_report_definitions_request {
    using response_type = struct put_many_report_definitions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_definitions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_definition_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_report_definitions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_definition> definitions;
};

struct delete_report_definition_request {
    using response_type = struct delete_report_definition_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_definitions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_definition_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_report_definition_response {
    ores::utility::domain::result result;
};

struct delete_many_report_definitions_request {
    using response_type = struct delete_many_report_definitions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_definitions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_definition_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_report_definitions_response {
    ores::utility::domain::result result;
};

struct list_report_definition_versions_request {
    using response_type = struct list_report_definition_versions_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report_definitions_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_definition_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<report_definition_versions_filter> filter;
};

struct list_report_definition_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_definition> versions;
    std::uint64_t total;
};

struct get_report_definition_version_request {
    using response_type = struct get_report_definition_version_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_definitions_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_definition_version_key key;
};

struct get_report_definition_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_definition> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace report_definition_event_subjects {
inline constexpr std::string_view created = "reporting.v1.report_definitions_events.created";
inline constexpr std::string_view updated = "reporting.v1.report_definitions_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.report_definitions_events.deleted";
}
/**
 * @brief The members of report_definition_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_definition_key&) {
    return std::tuple{
        utility::codec::field{"name", &report_definition_key::name},
    };
}

/**
 * @brief The members of report_definition_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_definition_write&) {
    return std::tuple{
        utility::codec::field{"id", &report_definition_write::id},
        utility::codec::field{"name", &report_definition_write::name},
        utility::codec::field{"party_id", &report_definition_write::party_id},
        utility::codec::field{"description", &report_definition_write::description},
        utility::codec::field{"report_type", &report_definition_write::report_type},
        utility::codec::field{"fsm_state_id", &report_definition_write::fsm_state_id},
        utility::codec::field{"schedule_expression", &report_definition_write::schedule_expression},
        utility::codec::field{"concurrency_policy", &report_definition_write::concurrency_policy},
        utility::codec::field{"scheduler_job_id", &report_definition_write::scheduler_job_id},
        utility::codec::field{"run_grant_id", &report_definition_write::run_grant_id},
        utility::codec::field{"pre_processing", &report_definition_write::pre_processing},
        utility::codec::field{"prepared_input_key", &report_definition_write::prepared_input_key},
        utility::codec::field{"post_processing", &report_definition_write::post_processing},
        utility::codec::field{"skipped_families", &report_definition_write::skipped_families},
        utility::codec::field{"is_official", &report_definition_write::is_official},
        utility::codec::field{"regulatory_book_type",
                              &report_definition_write::regulatory_book_type},
        utility::codec::field{"cut_kind_code", &report_definition_write::cut_kind_code},
        utility::codec::field{"cut_id", &report_definition_write::cut_id},
    };
}

/**
 * @brief The members of report_definition_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_definition_change&) {
    return std::tuple{
        utility::codec::field{"write", &report_definition_change::write},
        utility::codec::field{"precondition", &report_definition_change::precondition},
    };
}

/**
 * @brief The members of report_definition_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_definition_removal&) {
    return std::tuple{
        utility::codec::field{"key", &report_definition_removal::key},
        utility::codec::field{"precondition", &report_definition_removal::precondition},
    };
}

/**
 * @brief The members of report_definition_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_definition_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &report_definition_lookup::key},
        utility::codec::field{"report_definition", &report_definition_lookup::report_definition},
    };
}

/**
 * @brief The members of report_definitions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_definitions_filter&) {
    return std::tuple{
        utility::codec::field{"party_id", &report_definitions_filter::party_id},
        utility::codec::field{"cut_kind_code", &report_definitions_filter::cut_kind_code},
        utility::codec::field{"id_one_of", &report_definitions_filter::id_one_of},
        utility::codec::field{"party_id_one_of", &report_definitions_filter::party_id_one_of},
        utility::codec::field{"cut_kind_code_one_of",
                              &report_definitions_filter::cut_kind_code_one_of},
        utility::codec::field{"search", &report_definitions_filter::search},
    };
}

/**
 * @brief The members of report_definition_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_definition_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &report_definition_event::event_id},
        utility::codec::field{"key", &report_definition_event::key},
        utility::codec::field{"action", &report_definition_event::action},
        utility::codec::field{"version", &report_definition_event::version},
        utility::codec::field{"occurred_at", &report_definition_event::occurred_at},
        utility::codec::field{"correlation_id", &report_definition_event::correlation_id},
    };
}

/**
 * @brief The members of report_definition_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_definition_version_key&) {
    return std::tuple{
        utility::codec::field{"report_definition",
                              &report_definition_version_key::report_definition},
        utility::codec::field{"version", &report_definition_version_key::version},
    };
}

/**
 * @brief The members of report_definition_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_definition_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &report_definition_versions_filter::version},
        utility::codec::field{"from_version", &report_definition_versions_filter::from_version},
        utility::codec::field{"to_version", &report_definition_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_report_definitions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_report_definitions_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_report_definitions_request::offset},
        utility::codec::field{"limit", &list_report_definitions_request::limit},
        utility::codec::field{"order", &list_report_definitions_request::order},
        utility::codec::field{"filter", &list_report_definitions_request::filter},
        utility::codec::field{"as_of", &list_report_definitions_request::as_of},
    };
}

/**
 * @brief The members of list_report_definitions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_report_definitions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_report_definitions_response::result},
        utility::codec::field{"definitions", &list_report_definitions_response::definitions},
        utility::codec::field{"total", &list_report_definitions_response::total},
    };
}

/**
 * @brief The members of get_report_definition_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_report_definition_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_report_definition_request::key},
    };
}

/**
 * @brief The members of get_report_definition_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_report_definition_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_report_definition_response::result},
        utility::codec::field{"report_definition",
                              &get_report_definition_response::report_definition},
    };
}

/**
 * @brief The members of get_many_report_definitions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_report_definitions_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_report_definitions_request::keys},
    };
}

/**
 * @brief The members of get_many_report_definitions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_report_definitions_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_report_definitions_response::result},
        utility::codec::field{"entries", &get_many_report_definitions_response::entries},
    };
}

/**
 * @brief The members of put_report_definition_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_report_definition_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_report_definition_request::change},
        utility::codec::field{"intent", &put_report_definition_request::intent},
    };
}

/**
 * @brief The members of put_report_definition_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_report_definition_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_report_definition_response::result},
        utility::codec::field{"report_definition",
                              &put_report_definition_response::report_definition},
    };
}

/**
 * @brief The members of put_many_report_definitions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_report_definitions_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_report_definitions_request::changes},
        utility::codec::field{"intent", &put_many_report_definitions_request::intent},
    };
}

/**
 * @brief The members of put_many_report_definitions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_report_definitions_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_report_definitions_response::result},
        utility::codec::field{"definitions", &put_many_report_definitions_response::definitions},
    };
}

/**
 * @brief The members of delete_report_definition_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_report_definition_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_report_definition_request::removal},
        utility::codec::field{"intent", &delete_report_definition_request::intent},
    };
}

/**
 * @brief The members of delete_report_definition_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_report_definition_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_report_definition_response::result},
    };
}

/**
 * @brief The members of delete_many_report_definitions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_report_definitions_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_report_definitions_request::removals},
        utility::codec::field{"intent", &delete_many_report_definitions_request::intent},
    };
}

/**
 * @brief The members of delete_many_report_definitions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_report_definitions_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_report_definitions_response::result},
    };
}

/**
 * @brief The members of list_report_definition_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_report_definition_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_report_definition_versions_request::key},
        utility::codec::field{"offset", &list_report_definition_versions_request::offset},
        utility::codec::field{"limit", &list_report_definition_versions_request::limit},
        utility::codec::field{"order", &list_report_definition_versions_request::order},
        utility::codec::field{"filter", &list_report_definition_versions_request::filter},
    };
}

/**
 * @brief The members of list_report_definition_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_report_definition_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_report_definition_versions_response::result},
        utility::codec::field{"versions", &list_report_definition_versions_response::versions},
        utility::codec::field{"total", &list_report_definition_versions_response::total},
    };
}

/**
 * @brief The members of get_report_definition_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_report_definition_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_report_definition_version_request::key},
    };
}

/**
 * @brief The members of get_report_definition_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_report_definition_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_report_definition_version_response::result},
        utility::codec::field{"version", &get_report_definition_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_definition_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_definition_key& v);
    static ores::reporting::messaging::report_definition_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_definition_key& v);
    static ores::reporting::messaging::report_definition_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_definition_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_definition_write& v);
    static ores::reporting::messaging::report_definition_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_definition_write& v);
    static ores::reporting::messaging::report_definition_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_definition_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_definition_change& v);
    static ores::reporting::messaging::report_definition_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_definition_change& v);
    static ores::reporting::messaging::report_definition_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::report_definition_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_definition_removal& v);
    static ores::reporting::messaging::report_definition_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::report_definition_removal& v);
    static ores::reporting::messaging::report_definition_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_definition_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_definition_lookup& v);
    static ores::reporting::messaging::report_definition_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_definition_lookup& v);
    static ores::reporting::messaging::report_definition_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::report_definitions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_definitions_filter& v);
    static ores::reporting::messaging::report_definitions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::report_definitions_filter& v);
    static ores::reporting::messaging::report_definitions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_definition_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_definition_event& v);
    static ores::reporting::messaging::report_definition_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_definition_event& v);
    static ores::reporting::messaging::report_definition_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::report_definition_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::report_definition_version_key& v);
    static ores::reporting::messaging::report_definition_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::report_definition_version_key& v);
    static ores::reporting::messaging::report_definition_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::report_definition_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::report_definition_versions_filter& v);
    static ores::reporting::messaging::report_definition_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::report_definition_versions_filter& v);
    static ores::reporting::messaging::report_definition_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_report_definitions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_report_definitions_request& v);
    static ores::reporting::messaging::list_report_definitions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_report_definitions_request& v);
    static ores::reporting::messaging::list_report_definitions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_report_definitions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_report_definitions_response& v);
    static ores::reporting::messaging::list_report_definitions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_report_definitions_response& v);
    static ores::reporting::messaging::list_report_definitions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_definition_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_report_definition_request& v);
    static ores::reporting::messaging::get_report_definition_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_definition_request& v);
    static ores::reporting::messaging::get_report_definition_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_definition_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_report_definition_response& v);
    static ores::reporting::messaging::get_report_definition_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_definition_response& v);
    static ores::reporting::messaging::get_report_definition_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_report_definitions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_report_definitions_request& v);
    static ores::reporting::messaging::get_many_report_definitions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_report_definitions_request& v);
    static ores::reporting::messaging::get_many_report_definitions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_report_definitions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_report_definitions_response& v);
    static ores::reporting::messaging::get_many_report_definitions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_report_definitions_response& v);
    static ores::reporting::messaging::get_many_report_definitions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_report_definition_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_report_definition_request& v);
    static ores::reporting::messaging::put_report_definition_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_report_definition_request& v);
    static ores::reporting::messaging::put_report_definition_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_report_definition_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_report_definition_response& v);
    static ores::reporting::messaging::put_report_definition_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_report_definition_response& v);
    static ores::reporting::messaging::put_report_definition_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_report_definitions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_report_definitions_request& v);
    static ores::reporting::messaging::put_many_report_definitions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_report_definitions_request& v);
    static ores::reporting::messaging::put_many_report_definitions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_report_definitions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_report_definitions_response& v);
    static ores::reporting::messaging::put_many_report_definitions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_report_definitions_response& v);
    static ores::reporting::messaging::put_many_report_definitions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_report_definition_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_report_definition_request& v);
    static ores::reporting::messaging::delete_report_definition_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_report_definition_request& v);
    static ores::reporting::messaging::delete_report_definition_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_report_definition_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_report_definition_response& v);
    static ores::reporting::messaging::delete_report_definition_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_report_definition_response& v);
    static ores::reporting::messaging::delete_report_definition_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_report_definitions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_report_definitions_request& v);
    static ores::reporting::messaging::delete_many_report_definitions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_report_definitions_request& v);
    static ores::reporting::messaging::delete_many_report_definitions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_report_definitions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_report_definitions_response& v);
    static ores::reporting::messaging::delete_many_report_definitions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_report_definitions_response& v);
    static ores::reporting::messaging::delete_many_report_definitions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_report_definition_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_report_definition_versions_request& v);
    static ores::reporting::messaging::list_report_definition_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_report_definition_versions_request& v);
    static ores::reporting::messaging::list_report_definition_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_report_definition_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_report_definition_versions_response& v);
    static ores::reporting::messaging::list_report_definition_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_report_definition_versions_response& v);
    static ores::reporting::messaging::list_report_definition_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_definition_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_report_definition_version_request& v);
    static ores::reporting::messaging::get_report_definition_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_definition_version_request& v);
    static ores::reporting::messaging::get_report_definition_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_definition_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_report_definition_version_response& v);
    static ores::reporting::messaging::get_report_definition_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_definition_version_response& v);
    static ores::reporting::messaging::get_report_definition_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
