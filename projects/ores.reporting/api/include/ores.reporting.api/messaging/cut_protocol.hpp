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
#ifndef ORES_REPORTING_API_MESSAGING_CUT_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_CUT_PROTOCOL_HPP

#include "ores.reporting.api/domain/cut.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

struct cut_key {
    boost::uuids::uuid id;
};

struct cut_write {
    boost::uuids::uuid id;
    std::string kind_code;
    std::string scope_code;
    std::chrono::system_clock::time_point timepoint;
    int take_ordinal;
    std::string business_centre_code;
    std::string timepoint_timezone;
    std::optional<std::string> cut_time_code;
    std::optional<boost::uuids::uuid> market_data_snapshot_id;
    std::optional<boost::uuids::uuid> deal_population_id;
    std::optional<std::string> reference_data_version;
    std::optional<std::string> engine_version;
    std::optional<std::string> code_version;
    std::string taken_by;
    std::optional<std::chrono::system_clock::time_point> taken_at;
    bool requires_authorisation;
    std::optional<boost::uuids::uuid> approval_request_id;
    std::string approved_by;
    std::optional<std::chrono::system_clock::time_point> approved_at;
    std::optional<std::chrono::system_clock::time_point> refused_at;
    std::string refusal_comment;
    int retention_months;
    std::string retention_destination_code;
    bool legal_hold;
};

struct cut_change {
    cut_write write;
    ores::utility::domain::precondition precondition;
};

struct cut_removal {
    cut_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct cut_lookup {
    cut_key key;
    std::optional<ores::reporting::domain::cut> cut;
};

struct cuts_filter {
    std::optional<std::string> kind_code;
    std::optional<boost::uuids::uuid> party_id;
    std::optional<std::string> business_centre_code;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<std::string>> kind_code_one_of;
    std::optional<std::vector<boost::uuids::uuid>> party_id_one_of;
    std::optional<std::vector<std::string>> business_centre_code_one_of;
};

struct cut_event {
    boost::uuids::uuid event_id;
    cut_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct cut_version_key {
    cut_key cut;
    std::uint32_t version;
};

struct cut_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_cuts_request {
    using response_type = struct list_cuts_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cuts.list";
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
    std::optional<cuts_filter> filter;
    std::optional<std::string> as_of;
};

struct list_cuts_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::cut> cuts;
    std::uint64_t total;
};

struct get_cut_request {
    using response_type = struct get_cut_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cuts.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_key key;
};

struct get_cut_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::cut> cut;
};

struct get_many_cuts_request {
    using response_type = struct get_many_cuts_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cuts.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cut_key> keys;
};

struct get_many_cuts_response {
    ores::utility::domain::result result;
    std::vector<cut_lookup> entries;
};

struct put_cut_request {
    using response_type = struct put_cut_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cuts.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_change change;
    ores::utility::domain::change_intent intent;
};

struct put_cut_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::cut> cut;
};

struct put_many_cuts_request {
    using response_type = struct put_many_cuts_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cuts.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cut_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_cuts_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::cut> cuts;
};

struct delete_cut_request {
    using response_type = struct delete_cut_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cuts.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_cut_response {
    ores::utility::domain::result result;
};

struct delete_many_cuts_request {
    using response_type = struct delete_many_cuts_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cuts.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cut_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_cuts_response {
    ores::utility::domain::result result;
};

struct list_by_kind_code_cuts_request {
    using response_type = struct list_by_kind_code_cuts_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cuts.list_by_kind_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string kind_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<cuts_filter> filter;
};

struct list_by_kind_code_cuts_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::cut> cuts;
    std::uint64_t total;
};

struct list_cut_versions_request {
    using response_type = struct list_cut_versions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cuts_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<cut_versions_filter> filter;
};

struct list_cut_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::cut> versions;
    std::uint64_t total;
};

struct get_cut_version_request {
    using response_type = struct get_cut_version_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cuts_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_version_key key;
};

struct get_cut_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::cut> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace cut_event_subjects {
inline constexpr std::string_view created = "reporting.v1.cuts_events.created";
inline constexpr std::string_view updated = "reporting.v1.cuts_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.cuts_events.deleted";
}

}

#endif
