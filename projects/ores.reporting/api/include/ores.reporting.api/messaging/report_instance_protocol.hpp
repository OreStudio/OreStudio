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
#ifndef ORES_REPORTING_API_MESSAGING_REPORT_INSTANCE_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_REPORT_INSTANCE_PROTOCOL_HPP

#include "ores.reporting.api/domain/report_instance.hpp"
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

struct report_instance_key {
    std::string name;
};

struct report_instance_write {
    boost::uuids::uuid id;
    std::string name;
    std::string description;
    boost::uuids::uuid definition_id;
    std::string kind;
    std::optional<boost::uuids::uuid> run_grant_id;
    std::optional<boost::uuids::uuid> fsm_state_id;
    boost::uuids::uuid trigger_id;
    std::string output_message;
    std::string output_storage_key;
    std::optional<std::chrono::system_clock::time_point> priced_at;
    std::string price_note;
    std::optional<boost::uuids::uuid> engine_settings_id;
    std::optional<int> engine_settings_version;
    std::optional<boost::uuids::uuid> cut_id;
    std::optional<std::string> cut_label;
    std::optional<std::string> cut_kind_code;
    std::optional<std::chrono::system_clock::time_point> cut_timepoint;
};

struct report_instance_change {
    report_instance_write write;
    ores::utility::domain::precondition precondition;
};

struct report_instance_removal {
    report_instance_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct report_instance_lookup {
    report_instance_key key;
    std::optional<ores::reporting::domain::report_instance> report_instance;
};

struct report_instances_filter {
    std::optional<boost::uuids::uuid> definition_id;
    std::optional<std::string> kind;
    std::optional<std::optional<boost::uuids::uuid>> cut_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> definition_id_one_of;
    std::optional<std::vector<std::string>> kind_one_of;
    std::optional<std::vector<boost::uuids::uuid>> cut_id_one_of;
};

struct report_instance_event {
    boost::uuids::uuid event_id;
    report_instance_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct report_instance_version_key {
    report_instance_key report_instance;
    std::uint32_t version;
};

struct report_instance_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_report_instances_request {
    using response_type = struct list_report_instances_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_instances.list";
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
    std::optional<report_instances_filter> filter;
    std::optional<std::string> as_of;
};

struct list_report_instances_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_instance> instances;
    std::uint64_t total;
};

struct get_report_instance_request {
    using response_type = struct get_report_instance_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_instances.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_instance_key key;
};

struct get_report_instance_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_instance> report_instance;
};

struct get_many_report_instances_request {
    using response_type = struct get_many_report_instances_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_instances.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_instance_key> keys;
};

struct get_many_report_instances_response {
    ores::utility::domain::result result;
    std::vector<report_instance_lookup> entries;
};

struct put_report_instance_request {
    using response_type = struct put_report_instance_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_instances.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_instance_change change;
    ores::utility::domain::change_intent intent;
};

struct put_report_instance_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_instance> report_instance;
};

struct put_many_report_instances_request {
    using response_type = struct put_many_report_instances_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_instances.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_instance_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_report_instances_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_instance> instances;
};

struct delete_report_instance_request {
    using response_type = struct delete_report_instance_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_instances.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_instance_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_report_instance_response {
    ores::utility::domain::result result;
};

struct delete_many_report_instances_request {
    using response_type = struct delete_many_report_instances_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_instances.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_instance_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_report_instances_response {
    ores::utility::domain::result result;
};

struct list_report_instance_versions_request {
    using response_type = struct list_report_instance_versions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_instances_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_instance_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<report_instance_versions_filter> filter;
};

struct list_report_instance_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_instance> versions;
    std::uint64_t total;
};

struct get_report_instance_version_request {
    using response_type = struct get_report_instance_version_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_instances_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_instance_version_key key;
};

struct get_report_instance_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_instance> version;
};

/**
 * @brief Reads the run of each named definition that started last.
 *
 * The caller asks once with the ids a read of the definitions returned, so it
 * pays for one row per definition. The request names no party: the session
 * scopes the answer through the row-level security on the table.
 */
struct get_latest_report_instances_by_definition_request {
    using response_type = struct get_latest_report_instances_by_definition_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.ops.get_latest_report_instances_by_definition";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief The definitions to answer for, in any order. */
    std::vector<boost::uuids::uuid> definition_ids;
};

struct get_latest_report_instances_by_definition_response {
    ores::utility::domain::result result;
    /**
     * @brief At most one run per definition asked for, and no entry for a
     * definition that has never run.
     */
    std::vector<ores::reporting::domain::report_instance> instances;
};

struct report_instance_health {
    /** @brief The run the word is of. */
    boost::uuids::uuid report_instance_id;
    /**
     * @brief The name of the run's lifecycle state, as the store holds it.
     *
     * Absent when the run has no state, or when the session cannot read the
     * state: the word is then absent too, and the screen is told what the word
     * was derived from rather than left guessing.
     */
    std::string state_name;
    /**
     * @brief One of =failed=, =completed with errors=, =running= or
     * =completed clean=, or empty when the state has no word.
     *
     * =never run= is not a word here: a definition that never ran has no run,
     * so it has no entry in the reply.
     */
    std::string health;
};

/**
 * @brief Reads the health word of each named run.
 *
 * The caller asks once with the runs a read of the latest runs returned, so it
 * pays for one row per run. The request names no party: the session scopes the
 * answer through the row-level security on the tables.
 */
struct get_report_instance_health_request {
    using response_type = struct get_report_instance_health_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.get_report_instance_health";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief The runs to state the health of, in any order. */
    std::vector<boost::uuids::uuid> instance_ids;
};

struct get_report_instance_health_response {
    ores::utility::domain::result result;
    /**
     * @brief One entry per run the session can read, and no entry for a run
     * that does not exist or is not the caller's to read.
     */
    std::vector<report_instance_health> health;
};

struct report_health_count {
    /**
     * @brief One of =failed=, =completed with errors=, =running=, =never run=
     * or =completed clean=, or empty for the definitions whose latest run is in
     * a state that has no word yet.
     */
    std::string health;
    /** @brief How many of the counted definitions have this word. */
    std::int64_t count;
};

/**
 * @brief Counts the definitions at each health word.
 *
 * The request names the same party and search the list of definitions is
 * narrowed by, and the definitions counted are the ones that list would read.
 * It names no tenant: the session scopes the answer through the row-level
 * security on the tables.
 */
struct count_report_definitions_by_health_request {
    using response_type = struct count_report_definitions_by_health_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.ops.count_report_definitions_by_health";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief Counts only the definitions of this party, or every party in scope when absent. */
    std::optional<boost::uuids::uuid> party_id;
    /** @brief Counts only the definitions whose name or description holds this text. */
    std::optional<std::string> search;
};

struct count_report_definitions_by_health_response {
    ores::utility::domain::result result;
    /**
     * @brief One entry per word, in the order failed, completed with errors,
     * running, never run, completed clean, then the entry with no word. A word
     * no definition holds is stated with a count of zero.
     */
    std::vector<report_health_count> counts;
    /** @brief How many definitions were counted, which is the sum of the counts. */
    std::int64_t total;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace report_instance_event_subjects {
inline constexpr std::string_view created = "reporting.v1.report_instances_events.created";
inline constexpr std::string_view updated = "reporting.v1.report_instances_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.report_instances_events.deleted";
}
/**
 * @brief The members of report_instance_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_instance_key&) {
    return std::tuple{
        utility::codec::field{"name", &report_instance_key::name},
    };
}

/**
 * @brief The members of report_instance_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_instance_write&) {
    return std::tuple{
        utility::codec::field{"id", &report_instance_write::id},
        utility::codec::field{"name", &report_instance_write::name},
        utility::codec::field{"description", &report_instance_write::description},
        utility::codec::field{"definition_id", &report_instance_write::definition_id},
        utility::codec::field{"kind", &report_instance_write::kind},
        utility::codec::field{"run_grant_id", &report_instance_write::run_grant_id},
        utility::codec::field{"fsm_state_id", &report_instance_write::fsm_state_id},
        utility::codec::field{"trigger_id", &report_instance_write::trigger_id},
        utility::codec::field{"output_message", &report_instance_write::output_message},
        utility::codec::field{"output_storage_key", &report_instance_write::output_storage_key},
        utility::codec::field{"priced_at", &report_instance_write::priced_at},
        utility::codec::field{"price_note", &report_instance_write::price_note},
        utility::codec::field{"engine_settings_id", &report_instance_write::engine_settings_id},
        utility::codec::field{"engine_settings_version",
                              &report_instance_write::engine_settings_version},
        utility::codec::field{"cut_id", &report_instance_write::cut_id},
        utility::codec::field{"cut_label", &report_instance_write::cut_label},
        utility::codec::field{"cut_kind_code", &report_instance_write::cut_kind_code},
        utility::codec::field{"cut_timepoint", &report_instance_write::cut_timepoint},
    };
}

/**
 * @brief The members of report_instance_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_instance_change&) {
    return std::tuple{
        utility::codec::field{"write", &report_instance_change::write},
        utility::codec::field{"precondition", &report_instance_change::precondition},
    };
}

/**
 * @brief The members of report_instance_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_instance_removal&) {
    return std::tuple{
        utility::codec::field{"key", &report_instance_removal::key},
        utility::codec::field{"precondition", &report_instance_removal::precondition},
    };
}

/**
 * @brief The members of report_instance_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_instance_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &report_instance_lookup::key},
        utility::codec::field{"report_instance", &report_instance_lookup::report_instance},
    };
}

/**
 * @brief The members of report_instances_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_instances_filter&) {
    return std::tuple{
        utility::codec::field{"definition_id", &report_instances_filter::definition_id},
        utility::codec::field{"kind", &report_instances_filter::kind},
        utility::codec::field{"cut_id", &report_instances_filter::cut_id},
        utility::codec::field{"id_one_of", &report_instances_filter::id_one_of},
        utility::codec::field{"definition_id_one_of",
                              &report_instances_filter::definition_id_one_of},
        utility::codec::field{"kind_one_of", &report_instances_filter::kind_one_of},
        utility::codec::field{"cut_id_one_of", &report_instances_filter::cut_id_one_of},
    };
}

/**
 * @brief The members of report_instance_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_instance_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &report_instance_event::event_id},
        utility::codec::field{"key", &report_instance_event::key},
        utility::codec::field{"action", &report_instance_event::action},
        utility::codec::field{"version", &report_instance_event::version},
        utility::codec::field{"occurred_at", &report_instance_event::occurred_at},
        utility::codec::field{"correlation_id", &report_instance_event::correlation_id},
    };
}

/**
 * @brief The members of report_instance_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_instance_version_key&) {
    return std::tuple{
        utility::codec::field{"report_instance", &report_instance_version_key::report_instance},
        utility::codec::field{"version", &report_instance_version_key::version},
    };
}

/**
 * @brief The members of report_instance_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_instance_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &report_instance_versions_filter::version},
        utility::codec::field{"from_version", &report_instance_versions_filter::from_version},
        utility::codec::field{"to_version", &report_instance_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_report_instances_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_report_instances_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_report_instances_request::offset},
        utility::codec::field{"limit", &list_report_instances_request::limit},
        utility::codec::field{"order", &list_report_instances_request::order},
        utility::codec::field{"filter", &list_report_instances_request::filter},
        utility::codec::field{"as_of", &list_report_instances_request::as_of},
    };
}

/**
 * @brief The members of list_report_instances_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_report_instances_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_report_instances_response::result},
        utility::codec::field{"instances", &list_report_instances_response::instances},
        utility::codec::field{"total", &list_report_instances_response::total},
    };
}

/**
 * @brief The members of get_report_instance_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_report_instance_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_report_instance_request::key},
    };
}

/**
 * @brief The members of get_report_instance_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_report_instance_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_report_instance_response::result},
        utility::codec::field{"report_instance", &get_report_instance_response::report_instance},
    };
}

/**
 * @brief The members of get_many_report_instances_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_report_instances_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_report_instances_request::keys},
    };
}

/**
 * @brief The members of get_many_report_instances_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_report_instances_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_report_instances_response::result},
        utility::codec::field{"entries", &get_many_report_instances_response::entries},
    };
}

/**
 * @brief The members of put_report_instance_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_report_instance_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_report_instance_request::change},
        utility::codec::field{"intent", &put_report_instance_request::intent},
    };
}

/**
 * @brief The members of put_report_instance_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_report_instance_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_report_instance_response::result},
        utility::codec::field{"report_instance", &put_report_instance_response::report_instance},
    };
}

/**
 * @brief The members of put_many_report_instances_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_report_instances_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_report_instances_request::changes},
        utility::codec::field{"intent", &put_many_report_instances_request::intent},
    };
}

/**
 * @brief The members of put_many_report_instances_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_report_instances_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_report_instances_response::result},
        utility::codec::field{"instances", &put_many_report_instances_response::instances},
    };
}

/**
 * @brief The members of delete_report_instance_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_report_instance_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_report_instance_request::removal},
        utility::codec::field{"intent", &delete_report_instance_request::intent},
    };
}

/**
 * @brief The members of delete_report_instance_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_report_instance_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_report_instance_response::result},
    };
}

/**
 * @brief The members of delete_many_report_instances_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_report_instances_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_report_instances_request::removals},
        utility::codec::field{"intent", &delete_many_report_instances_request::intent},
    };
}

/**
 * @brief The members of delete_many_report_instances_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_report_instances_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_report_instances_response::result},
    };
}

/**
 * @brief The members of list_report_instance_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_report_instance_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_report_instance_versions_request::key},
        utility::codec::field{"offset", &list_report_instance_versions_request::offset},
        utility::codec::field{"limit", &list_report_instance_versions_request::limit},
        utility::codec::field{"order", &list_report_instance_versions_request::order},
        utility::codec::field{"filter", &list_report_instance_versions_request::filter},
    };
}

/**
 * @brief The members of list_report_instance_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_report_instance_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_report_instance_versions_response::result},
        utility::codec::field{"versions", &list_report_instance_versions_response::versions},
        utility::codec::field{"total", &list_report_instance_versions_response::total},
    };
}

/**
 * @brief The members of get_report_instance_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_report_instance_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_report_instance_version_request::key},
    };
}

/**
 * @brief The members of get_report_instance_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_report_instance_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_report_instance_version_response::result},
        utility::codec::field{"version", &get_report_instance_version_response::version},
    };
}

/**
 * @brief The members of get_latest_report_instances_by_definition_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_latest_report_instances_by_definition_request&) {
    return std::tuple{
        utility::codec::field{"definition_ids",
                              &get_latest_report_instances_by_definition_request::definition_ids},
    };
}

/**
 * @brief The members of get_latest_report_instances_by_definition_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_latest_report_instances_by_definition_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_latest_report_instances_by_definition_response::result},
        utility::codec::field{"instances",
                              &get_latest_report_instances_by_definition_response::instances},
    };
}

/**
 * @brief The members of report_instance_health, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_instance_health&) {
    return std::tuple{
        utility::codec::field{"report_instance_id", &report_instance_health::report_instance_id},
        utility::codec::field{"state_name", &report_instance_health::state_name},
        utility::codec::field{"health", &report_instance_health::health},
    };
}

/**
 * @brief The members of get_report_instance_health_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_report_instance_health_request&) {
    return std::tuple{
        utility::codec::field{"instance_ids", &get_report_instance_health_request::instance_ids},
    };
}

/**
 * @brief The members of get_report_instance_health_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_report_instance_health_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_report_instance_health_response::result},
        utility::codec::field{"health", &get_report_instance_health_response::health},
    };
}

/**
 * @brief The members of report_health_count, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const report_health_count&) {
    return std::tuple{
        utility::codec::field{"health", &report_health_count::health},
        utility::codec::field{"count", &report_health_count::count},
    };
}

/**
 * @brief The members of count_report_definitions_by_health_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const count_report_definitions_by_health_request&) {
    return std::tuple{
        utility::codec::field{"party_id", &count_report_definitions_by_health_request::party_id},
        utility::codec::field{"search", &count_report_definitions_by_health_request::search},
    };
}

/**
 * @brief The members of count_report_definitions_by_health_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const count_report_definitions_by_health_response&) {
    return std::tuple{
        utility::codec::field{"result", &count_report_definitions_by_health_response::result},
        utility::codec::field{"counts", &count_report_definitions_by_health_response::counts},
        utility::codec::field{"total", &count_report_definitions_by_health_response::total},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_instance_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_instance_key& v);
    static ores::reporting::messaging::report_instance_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_instance_key& v);
    static ores::reporting::messaging::report_instance_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_instance_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_instance_write& v);
    static ores::reporting::messaging::report_instance_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_instance_write& v);
    static ores::reporting::messaging::report_instance_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_instance_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_instance_change& v);
    static ores::reporting::messaging::report_instance_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_instance_change& v);
    static ores::reporting::messaging::report_instance_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_instance_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_instance_removal& v);
    static ores::reporting::messaging::report_instance_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_instance_removal& v);
    static ores::reporting::messaging::report_instance_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_instance_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_instance_lookup& v);
    static ores::reporting::messaging::report_instance_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_instance_lookup& v);
    static ores::reporting::messaging::report_instance_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_instances_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_instances_filter& v);
    static ores::reporting::messaging::report_instances_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_instances_filter& v);
    static ores::reporting::messaging::report_instances_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_instance_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_instance_event& v);
    static ores::reporting::messaging::report_instance_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_instance_event& v);
    static ores::reporting::messaging::report_instance_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::report_instance_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_instance_version_key& v);
    static ores::reporting::messaging::report_instance_version_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::report_instance_version_key& v);
    static ores::reporting::messaging::report_instance_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::report_instance_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::report_instance_versions_filter& v);
    static ores::reporting::messaging::report_instance_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::report_instance_versions_filter& v);
    static ores::reporting::messaging::report_instance_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_report_instances_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_report_instances_request& v);
    static ores::reporting::messaging::list_report_instances_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_report_instances_request& v);
    static ores::reporting::messaging::list_report_instances_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_report_instances_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_report_instances_response& v);
    static ores::reporting::messaging::list_report_instances_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_report_instances_response& v);
    static ores::reporting::messaging::list_report_instances_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_instance_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_report_instance_request& v);
    static ores::reporting::messaging::get_report_instance_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_instance_request& v);
    static ores::reporting::messaging::get_report_instance_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_instance_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_report_instance_response& v);
    static ores::reporting::messaging::get_report_instance_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_instance_response& v);
    static ores::reporting::messaging::get_report_instance_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_report_instances_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_report_instances_request& v);
    static ores::reporting::messaging::get_many_report_instances_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_report_instances_request& v);
    static ores::reporting::messaging::get_many_report_instances_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_report_instances_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_report_instances_response& v);
    static ores::reporting::messaging::get_many_report_instances_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_report_instances_response& v);
    static ores::reporting::messaging::get_many_report_instances_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_report_instance_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_report_instance_request& v);
    static ores::reporting::messaging::put_report_instance_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_report_instance_request& v);
    static ores::reporting::messaging::put_report_instance_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_report_instance_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_report_instance_response& v);
    static ores::reporting::messaging::put_report_instance_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_report_instance_response& v);
    static ores::reporting::messaging::put_report_instance_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_report_instances_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_report_instances_request& v);
    static ores::reporting::messaging::put_many_report_instances_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_report_instances_request& v);
    static ores::reporting::messaging::put_many_report_instances_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_report_instances_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_report_instances_response& v);
    static ores::reporting::messaging::put_many_report_instances_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_report_instances_response& v);
    static ores::reporting::messaging::put_many_report_instances_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_report_instance_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_report_instance_request& v);
    static ores::reporting::messaging::delete_report_instance_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_report_instance_request& v);
    static ores::reporting::messaging::delete_report_instance_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_report_instance_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_report_instance_response& v);
    static ores::reporting::messaging::delete_report_instance_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_report_instance_response& v);
    static ores::reporting::messaging::delete_report_instance_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_report_instances_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_report_instances_request& v);
    static ores::reporting::messaging::delete_many_report_instances_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_report_instances_request& v);
    static ores::reporting::messaging::delete_many_report_instances_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_report_instances_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_report_instances_response& v);
    static ores::reporting::messaging::delete_many_report_instances_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_report_instances_response& v);
    static ores::reporting::messaging::delete_many_report_instances_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_report_instance_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_report_instance_versions_request& v);
    static ores::reporting::messaging::list_report_instance_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_report_instance_versions_request& v);
    static ores::reporting::messaging::list_report_instance_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_report_instance_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_report_instance_versions_response& v);
    static ores::reporting::messaging::list_report_instance_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_report_instance_versions_response& v);
    static ores::reporting::messaging::list_report_instance_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_instance_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_report_instance_version_request& v);
    static ores::reporting::messaging::get_report_instance_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_instance_version_request& v);
    static ores::reporting::messaging::get_report_instance_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_instance_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_report_instance_version_response& v);
    static ores::reporting::messaging::get_report_instance_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_instance_version_response& v);
    static ores::reporting::messaging::get_report_instance_version_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_latest_report_instances_by_definition_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::get_latest_report_instances_by_definition_request& v);
    static ores::reporting::messaging::get_latest_report_instances_by_definition_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::get_latest_report_instances_by_definition_request& v);
    static ores::reporting::messaging::get_latest_report_instances_by_definition_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_latest_report_instances_by_definition_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::get_latest_report_instances_by_definition_response& v);
    static ores::reporting::messaging::get_latest_report_instances_by_definition_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::get_latest_report_instances_by_definition_response& v);
    static ores::reporting::messaging::get_latest_report_instances_by_definition_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_instance_health> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_instance_health& v);
    static ores::reporting::messaging::report_instance_health read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_instance_health& v);
    static ores::reporting::messaging::report_instance_health
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_instance_health_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_report_instance_health_request& v);
    static ores::reporting::messaging::get_report_instance_health_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_instance_health_request& v);
    static ores::reporting::messaging::get_report_instance_health_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_instance_health_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_report_instance_health_response& v);
    static ores::reporting::messaging::get_report_instance_health_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_instance_health_response& v);
    static ores::reporting::messaging::get_report_instance_health_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_health_count> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_health_count& v);
    static ores::reporting::messaging::report_health_count read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_health_count& v);
    static ores::reporting::messaging::report_health_count
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::count_report_definitions_by_health_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::count_report_definitions_by_health_request& v);
    static ores::reporting::messaging::count_report_definitions_by_health_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::count_report_definitions_by_health_request& v);
    static ores::reporting::messaging::count_report_definitions_by_health_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::count_report_definitions_by_health_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::count_report_definitions_by_health_response& v);
    static ores::reporting::messaging::count_report_definitions_by_health_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::count_report_definitions_by_health_response& v);
    static ores::reporting::messaging::count_report_definitions_by_health_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
