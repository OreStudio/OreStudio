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
#ifndef ORES_REFDATA_API_MESSAGING_BUSINESS_CENTRE_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_BUSINESS_CENTRE_PROTOCOL_HPP

#include "ores.refdata.api/domain/business_centre.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct business_centre_key {
    std::string code;
};

struct business_centre_write {
    std::string code;
    std::string source;
    std::string description;
    std::string city_name;
    std::string country_alpha2_code;
    std::string coding_scheme_code;
};

struct business_centre_change {
    business_centre_write write;
    ores::utility::domain::precondition precondition;
};

struct business_centre_removal {
    business_centre_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct business_centre_lookup {
    business_centre_key key;
    std::optional<ores::refdata::domain::business_centre> business_centre;
};

struct business_centre_event {
    boost::uuids::uuid event_id;
    business_centre_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct business_centre_version_key {
    business_centre_key business_centre;
    std::uint32_t version;
};

struct business_centre_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_business_centres_request {
    using response_type = struct list_business_centres_response;
    static constexpr std::string_view nats_subject = "refdata.v1.business_centres.list";
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

struct list_business_centres_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::business_centre> centres;
    std::uint64_t total;
};

struct get_business_centre_request {
    using response_type = struct get_business_centre_response;
    static constexpr std::string_view nats_subject = "refdata.v1.business_centres.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    business_centre_key key;
};

struct get_business_centre_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::business_centre> business_centre;
};

struct get_many_business_centres_request {
    using response_type = struct get_many_business_centres_response;
    static constexpr std::string_view nats_subject = "refdata.v1.business_centres.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<business_centre_key> keys;
};

struct get_many_business_centres_response {
    ores::utility::domain::result result;
    std::vector<business_centre_lookup> entries;
};

struct put_business_centre_request {
    using response_type = struct put_business_centre_response;
    static constexpr std::string_view nats_subject = "refdata.v1.business_centres.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    business_centre_change change;
    ores::utility::domain::change_intent intent;
};

struct put_business_centre_response {
    ores::utility::domain::result result;
    ores::refdata::domain::business_centre business_centre;
};

struct put_many_business_centres_request {
    using response_type = struct put_many_business_centres_response;
    static constexpr std::string_view nats_subject = "refdata.v1.business_centres.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<business_centre_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_business_centres_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::business_centre> centres;
};

struct delete_business_centre_request {
    using response_type = struct delete_business_centre_response;
    static constexpr std::string_view nats_subject = "refdata.v1.business_centres.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    business_centre_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_business_centre_response {
    ores::utility::domain::result result;
};

struct delete_many_business_centres_request {
    using response_type = struct delete_many_business_centres_response;
    static constexpr std::string_view nats_subject = "refdata.v1.business_centres.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<business_centre_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_business_centres_response {
    ores::utility::domain::result result;
};

struct list_business_centre_versions_request {
    using response_type = struct list_business_centre_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.business_centres_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    business_centre_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<business_centre_versions_filter> filter;
};

struct list_business_centre_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::business_centre> versions;
    std::uint64_t total;
};

struct get_business_centre_version_request {
    using response_type = struct get_business_centre_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.business_centres_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    business_centre_version_key key;
};

struct get_business_centre_version_response {
    ores::utility::domain::result result;
    ores::refdata::domain::business_centre version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace business_centre_event_subjects {
inline constexpr std::string_view created = "refdata.v1.business_centres_events.created";
inline constexpr std::string_view updated = "refdata.v1.business_centres_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.business_centres_events.deleted";
}

}

#endif
