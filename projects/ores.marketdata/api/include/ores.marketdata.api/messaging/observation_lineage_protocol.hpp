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
#ifndef ORES_MARKETDATA_API_MESSAGING_OBSERVATION_LINEAGE_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_OBSERVATION_LINEAGE_PROTOCOL_HPP

#include "ores.marketdata.api/domain/observation_lineage.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

struct observation_lineage_key {
    boost::uuids::uuid id;
};

struct observation_lineage_write {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    boost::uuids::uuid series_id;
    std::chrono::system_clock::time_point observation_datetime;
    std::string oresmd_uri;
    boost::uuids::uuid derivation_config_id;
    int derivation_config_version;
    std::chrono::system_clock::time_point source_as_of;
    std::string source_series_ids;
};

struct observation_lineage_change {
    observation_lineage_write write;
    ores::utility::domain::precondition precondition;
};

struct observation_lineage_removal {
    observation_lineage_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct observation_lineage_lookup {
    observation_lineage_key key;
    std::optional<ores::marketdata::domain::observation_lineage> observation_lineage;
};

struct observation_lineage_event {
    boost::uuids::uuid event_id;
    observation_lineage_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct observation_lineage_version_key {
    observation_lineage_key observation_lineage;
    std::uint32_t version;
};

struct observation_lineage_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_observation_lineages_request {
    using response_type = struct list_observation_lineages_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.observation_lineages.list";
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

struct list_observation_lineages_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::observation_lineage> observation_lineages;
    std::uint64_t total;
};

struct get_observation_lineage_request {
    using response_type = struct get_observation_lineage_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.observation_lineages.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    observation_lineage_key key;
};

struct get_observation_lineage_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::observation_lineage> observation_lineage;
};

struct get_many_observation_lineages_request {
    using response_type = struct get_many_observation_lineages_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.observation_lineages.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<observation_lineage_key> keys;
};

struct get_many_observation_lineages_response {
    ores::utility::domain::result result;
    std::vector<observation_lineage_lookup> entries;
};

struct put_observation_lineage_request {
    using response_type = struct put_observation_lineage_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.observation_lineages.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    observation_lineage_change change;
    ores::utility::domain::change_intent intent;
};

struct put_observation_lineage_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::observation_lineage> observation_lineage;
};

struct put_many_observation_lineages_request {
    using response_type = struct put_many_observation_lineages_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.observation_lineages.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<observation_lineage_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_observation_lineages_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::observation_lineage> observation_lineages;
};

struct delete_observation_lineage_request {
    using response_type = struct delete_observation_lineage_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.observation_lineages.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    observation_lineage_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_observation_lineage_response {
    ores::utility::domain::result result;
};

struct delete_many_observation_lineages_request {
    using response_type = struct delete_many_observation_lineages_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.observation_lineages.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<observation_lineage_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_observation_lineages_response {
    ores::utility::domain::result result;
};

struct list_observation_lineage_versions_request {
    using response_type = struct list_observation_lineage_versions_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.observation_lineages_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    observation_lineage_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<observation_lineage_versions_filter> filter;
};

struct list_observation_lineage_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::observation_lineage> versions;
    std::uint64_t total;
};

struct get_observation_lineage_version_request {
    using response_type = struct get_observation_lineage_version_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.observation_lineages_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    observation_lineage_version_key key;
};

struct get_observation_lineage_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::observation_lineage> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace observation_lineage_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.observation_lineages_events.created";
inline constexpr std::string_view updated = "marketdata.v1.observation_lineages_events.updated";
inline constexpr std::string_view deleted = "marketdata.v1.observation_lineages_events.deleted";
}

}

#endif
