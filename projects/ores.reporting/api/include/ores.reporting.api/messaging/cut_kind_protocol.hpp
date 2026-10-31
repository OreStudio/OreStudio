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
#ifndef ORES_REPORTING_API_MESSAGING_CUT_KIND_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_CUT_KIND_PROTOCOL_HPP

#include "ores.reporting.api/domain/cut_kind.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

struct cut_kind_key {
    std::string code;
};

struct cut_kind_write {
    std::string code;
    std::string name;
    std::string description;
    std::string family_code;
    std::optional<std::string> approval_kind_code;
    bool output_is_indicative;
    std::string resolution_rule_code;
    int retention_months;
    std::string retention_destination_code;
    bool legal_hold_default;
    int display_order;
};

struct cut_kind_change {
    cut_kind_write write;
    ores::utility::domain::precondition precondition;
};

struct cut_kind_removal {
    cut_kind_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct cut_kind_lookup {
    cut_kind_key key;
    std::optional<ores::reporting::domain::cut_kind> cut_kind;
};

struct cut_kinds_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct cut_kind_event {
    boost::uuids::uuid event_id;
    cut_kind_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct cut_kind_version_key {
    cut_kind_key cut_kind;
    std::uint32_t version;
};

struct cut_kind_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_cut_kinds_request {
    using response_type = struct list_cut_kinds_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_kinds.list";
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
    std::optional<cut_kinds_filter> filter;
    std::optional<std::string> as_of;
};

struct list_cut_kinds_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::cut_kind> kinds;
    std::uint64_t total;
};

struct get_cut_kind_request {
    using response_type = struct get_cut_kind_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_kinds.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_kind_key key;
};

struct get_cut_kind_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::cut_kind> cut_kind;
};

struct get_many_cut_kinds_request {
    using response_type = struct get_many_cut_kinds_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_kinds.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cut_kind_key> keys;
};

struct get_many_cut_kinds_response {
    ores::utility::domain::result result;
    std::vector<cut_kind_lookup> entries;
};

struct put_cut_kind_request {
    using response_type = struct put_cut_kind_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_kinds.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_kind_change change;
    ores::utility::domain::change_intent intent;
};

struct put_cut_kind_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::cut_kind> cut_kind;
};

struct put_many_cut_kinds_request {
    using response_type = struct put_many_cut_kinds_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_kinds.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cut_kind_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_cut_kinds_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::cut_kind> kinds;
};

struct delete_cut_kind_request {
    using response_type = struct delete_cut_kind_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_kinds.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_kind_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_cut_kind_response {
    ores::utility::domain::result result;
};

struct delete_many_cut_kinds_request {
    using response_type = struct delete_many_cut_kinds_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_kinds.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cut_kind_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_cut_kinds_response {
    ores::utility::domain::result result;
};

struct list_cut_kind_versions_request {
    using response_type = struct list_cut_kind_versions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_kinds_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_kind_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<cut_kind_versions_filter> filter;
};

struct list_cut_kind_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::cut_kind> versions;
    std::uint64_t total;
};

struct get_cut_kind_version_request {
    using response_type = struct get_cut_kind_version_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_kinds_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_kind_version_key key;
};

struct get_cut_kind_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::cut_kind> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace cut_kind_event_subjects {
inline constexpr std::string_view created = "reporting.v1.cut_kinds_events.created";
inline constexpr std::string_view updated = "reporting.v1.cut_kinds_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.cut_kinds_events.deleted";
}

}

#endif
