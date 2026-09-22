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
#ifndef ORES_REFDATA_API_MESSAGING_SERIES_SUBCLASS_CODE_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_SERIES_SUBCLASS_CODE_PROTOCOL_HPP

#include "ores.refdata.api/domain/series_subclass_code.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct series_subclass_code_key {
    std::string code;
};

struct series_subclass_code_write {
    std::string code;
    std::string name;
    std::string description;
    int display_order;
};

struct series_subclass_code_change {
    series_subclass_code_write write;
    ores::utility::domain::precondition precondition;
};

struct series_subclass_code_removal {
    series_subclass_code_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct series_subclass_code_lookup {
    series_subclass_code_key key;
    std::optional<ores::refdata::domain::series_subclass_code> series_subclass_code;
};

struct series_subclass_code_event {
    boost::uuids::uuid event_id;
    series_subclass_code_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct series_subclass_code_version_key {
    series_subclass_code_key series_subclass_code;
    std::uint32_t version;
};

struct series_subclass_code_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_series_subclass_codes_request {
    using response_type = struct list_series_subclass_codes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.series_subclass_codes.list";
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
};

struct list_series_subclass_codes_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::series_subclass_code> series_subclasses;
    std::uint64_t total;
};

struct get_series_subclass_code_request {
    using response_type = struct get_series_subclass_code_response;
    static constexpr std::string_view nats_subject = "refdata.v1.series_subclass_codes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    series_subclass_code_key key;
};

struct get_series_subclass_code_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::series_subclass_code> series_subclass_code;
};

struct get_many_series_subclass_codes_request {
    using response_type = struct get_many_series_subclass_codes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.series_subclass_codes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<series_subclass_code_key> keys;
};

struct get_many_series_subclass_codes_response {
    ores::utility::domain::result result;
    std::vector<series_subclass_code_lookup> entries;
};

struct put_series_subclass_code_request {
    using response_type = struct put_series_subclass_code_response;
    static constexpr std::string_view nats_subject = "refdata.v1.series_subclass_codes.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    series_subclass_code_change change;
    ores::utility::domain::change_intent intent;
};

struct put_series_subclass_code_response {
    ores::utility::domain::result result;
    ores::refdata::domain::series_subclass_code series_subclass_code;
};

struct put_many_series_subclass_codes_request {
    using response_type = struct put_many_series_subclass_codes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.series_subclass_codes.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<series_subclass_code_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_series_subclass_codes_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::series_subclass_code> series_subclasses;
};

struct delete_series_subclass_code_request {
    using response_type = struct delete_series_subclass_code_response;
    static constexpr std::string_view nats_subject = "refdata.v1.series_subclass_codes.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    series_subclass_code_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_series_subclass_code_response {
    ores::utility::domain::result result;
};

struct delete_many_series_subclass_codes_request {
    using response_type = struct delete_many_series_subclass_codes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.series_subclass_codes.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<series_subclass_code_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_series_subclass_codes_response {
    ores::utility::domain::result result;
};

struct list_series_subclass_code_versions_request {
    using response_type = struct list_series_subclass_code_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.series_subclass_codes_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    series_subclass_code_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<series_subclass_code_versions_filter> filter;
};

struct list_series_subclass_code_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::series_subclass_code> versions;
    std::uint64_t total;
};

struct get_series_subclass_code_version_request {
    using response_type = struct get_series_subclass_code_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.series_subclass_codes_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    series_subclass_code_version_key key;
};

struct get_series_subclass_code_version_response {
    ores::utility::domain::result result;
    ores::refdata::domain::series_subclass_code version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace series_subclass_code_event_subjects {
inline constexpr std::string_view created = "refdata.v1.series_subclass_codes_events.created";
inline constexpr std::string_view updated = "refdata.v1.series_subclass_codes_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.series_subclass_codes_events.deleted";
}

}

#endif
