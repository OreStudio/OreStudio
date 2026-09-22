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
#ifndef ORES_REFDATA_API_MESSAGING_COUNTRY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_COUNTRY_PROTOCOL_HPP

#include "ores.refdata.api/domain/country.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct country_key {
    std::string alpha2_code;
};

struct country_write {
    std::string alpha2_code;
    std::string alpha3_code;
    std::string numeric_code;
    std::string name;
    std::string official_name;
    std::optional<boost::uuids::uuid> image_id;
    std::optional<std::string> coding_scheme_code;
};

struct country_change {
    country_write write;
    ores::utility::domain::precondition precondition;
};

struct country_removal {
    country_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct country_lookup {
    country_key key;
    std::optional<ores::refdata::domain::country> country;
};

struct country_event {
    boost::uuids::uuid event_id;
    country_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct country_version_key {
    country_key country;
    std::uint32_t version;
};

struct country_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_countries_request {
    using response_type = struct list_countries_response;
    static constexpr std::string_view nats_subject = "refdata.v1.countries.list";
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
    std::optional<std::string> as_of;
};

struct list_countries_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::country> countries;
    std::uint64_t total;
};

struct get_country_request {
    using response_type = struct get_country_response;
    static constexpr std::string_view nats_subject = "refdata.v1.countries.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    country_key key;
};

struct get_country_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::country> country;
};

struct get_many_countries_request {
    using response_type = struct get_many_countries_response;
    static constexpr std::string_view nats_subject = "refdata.v1.countries.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<country_key> keys;
};

struct get_many_countries_response {
    ores::utility::domain::result result;
    std::vector<country_lookup> entries;
};

struct put_country_request {
    using response_type = struct put_country_response;
    static constexpr std::string_view nats_subject = "refdata.v1.countries.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    country_change change;
    ores::utility::domain::change_intent intent;
};

struct put_country_response {
    ores::utility::domain::result result;
    ores::refdata::domain::country country;
};

struct put_many_countries_request {
    using response_type = struct put_many_countries_response;
    static constexpr std::string_view nats_subject = "refdata.v1.countries.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<country_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_countries_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::country> countries;
};

struct delete_country_request {
    using response_type = struct delete_country_response;
    static constexpr std::string_view nats_subject = "refdata.v1.countries.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    country_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_country_response {
    ores::utility::domain::result result;
};

struct delete_many_countries_request {
    using response_type = struct delete_many_countries_response;
    static constexpr std::string_view nats_subject = "refdata.v1.countries.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<country_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_countries_response {
    ores::utility::domain::result result;
};

struct list_country_versions_request {
    using response_type = struct list_country_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.countries_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    country_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<country_versions_filter> filter;
};

struct list_country_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::country> versions;
    std::uint64_t total;
};

struct get_country_version_request {
    using response_type = struct get_country_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.countries_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    country_version_key key;
};

struct get_country_version_response {
    ores::utility::domain::result result;
    ores::refdata::domain::country version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace country_event_subjects {
inline constexpr std::string_view created = "refdata.v1.countries_events.created";
inline constexpr std::string_view updated = "refdata.v1.countries_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.countries_events.deleted";
}

}

#endif
