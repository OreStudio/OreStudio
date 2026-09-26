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
#ifndef ORES_SYNTHETIC_API_MESSAGING_GMM_COMPONENT_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_GMM_COMPONENT_PROTOCOL_HPP

#include "ores.synthetic.api/domain/gmm_component.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::messaging {

struct gmm_component_key {
    boost::uuids::uuid id;
};

struct gmm_component_write {
    boost::uuids::uuid id;
    boost::uuids::uuid fx_spot_config_id;
    int component_index;
    std::string description;
    double mean;
    double stdev;
    double weight;
};

struct gmm_component_change {
    gmm_component_write write;
    ores::utility::domain::precondition precondition;
};

struct gmm_component_removal {
    gmm_component_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct gmm_component_lookup {
    gmm_component_key key;
    std::optional<ores::synthetic::domain::gmm_component> gmm_component;
};

struct gmm_component_event {
    boost::uuids::uuid event_id;
    gmm_component_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct gmm_component_version_key {
    gmm_component_key gmm_component;
    std::uint32_t version;
};

struct gmm_component_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_gmm_components_request {
    using response_type = struct list_gmm_components_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.gmm_components.list";
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

struct list_gmm_components_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::gmm_component> gmm_components;
    std::uint64_t total;
};

struct get_gmm_component_request {
    using response_type = struct get_gmm_component_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.gmm_components.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    gmm_component_key key;
};

struct get_gmm_component_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::gmm_component> gmm_component;
};

struct get_many_gmm_components_request {
    using response_type = struct get_many_gmm_components_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.gmm_components.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<gmm_component_key> keys;
};

struct get_many_gmm_components_response {
    ores::utility::domain::result result;
    std::vector<gmm_component_lookup> entries;
};

struct put_gmm_component_request {
    using response_type = struct put_gmm_component_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.gmm_components.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    gmm_component_change change;
    ores::utility::domain::change_intent intent;
};

struct put_gmm_component_response {
    ores::utility::domain::result result;
    ores::synthetic::domain::gmm_component gmm_component;
};

struct put_many_gmm_components_request {
    using response_type = struct put_many_gmm_components_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.gmm_components.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<gmm_component_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_gmm_components_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::gmm_component> gmm_components;
};

struct delete_gmm_component_request {
    using response_type = struct delete_gmm_component_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.gmm_components.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    gmm_component_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_gmm_component_response {
    ores::utility::domain::result result;
};

struct delete_many_gmm_components_request {
    using response_type = struct delete_many_gmm_components_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.gmm_components.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<gmm_component_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_gmm_components_response {
    ores::utility::domain::result result;
};

struct list_gmm_component_versions_request {
    using response_type = struct list_gmm_component_versions_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.gmm_components_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    gmm_component_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<gmm_component_versions_filter> filter;
};

struct list_gmm_component_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::gmm_component> versions;
    std::uint64_t total;
};

struct get_gmm_component_version_request {
    using response_type = struct get_gmm_component_version_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.gmm_components_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    gmm_component_version_key key;
};

struct get_gmm_component_version_response {
    ores::utility::domain::result result;
    ores::synthetic::domain::gmm_component version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace gmm_component_event_subjects {
inline constexpr std::string_view created = "synthetic.v1.gmm_components_events.created";
inline constexpr std::string_view updated = "synthetic.v1.gmm_components_events.updated";
inline constexpr std::string_view deleted = "synthetic.v1.gmm_components_events.deleted";
}

}

#endif
