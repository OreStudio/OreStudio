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
#ifndef ORES_MARKETDATA_API_MESSAGING_RATE_CARD_COMPONENT_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_RATE_CARD_COMPONENT_PROTOCOL_HPP

#include "ores.marketdata.api/domain/rate_card_component.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

struct rate_card_component_key {
    std::string driver;
};

struct rate_card_component_write {
    boost::uuids::uuid id;
    boost::uuids::uuid feed_id;
    std::string driver;
    std::string licence_right;
    std::chrono::system_clock::time_point effective_from;
    std::string currency;
    std::int64_t charge_micros;
    std::optional<std::chrono::system_clock::time_point> effective_to;
};

struct rate_card_component_change {
    rate_card_component_write write;
    ores::utility::domain::precondition precondition;
};

struct rate_card_component_removal {
    rate_card_component_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct rate_card_component_lookup {
    rate_card_component_key key;
    std::optional<ores::marketdata::domain::rate_card_component> rate_card_component;
};

struct rate_card_components_filter {
    std::optional<boost::uuids::uuid> feed_id;
    std::optional<std::string> driver;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> feed_id_one_of;
    std::optional<std::vector<std::string>> driver_one_of;
};

struct rate_card_component_event {
    boost::uuids::uuid event_id;
    rate_card_component_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct rate_card_component_version_key {
    rate_card_component_key rate_card_component;
    std::uint32_t version;
};

struct rate_card_component_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_rate_card_components_request {
    using response_type = struct list_rate_card_components_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.rate_card_components.list";
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
    std::optional<rate_card_components_filter> filter;
    std::optional<std::string> as_of;
};

struct list_rate_card_components_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::rate_card_component> rate_card_components;
    std::uint64_t total;
};

struct get_rate_card_component_request {
    using response_type = struct get_rate_card_component_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.rate_card_components.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    rate_card_component_key key;
};

struct get_rate_card_component_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::rate_card_component> rate_card_component;
};

struct get_many_rate_card_components_request {
    using response_type = struct get_many_rate_card_components_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.rate_card_components.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<rate_card_component_key> keys;
};

struct get_many_rate_card_components_response {
    ores::utility::domain::result result;
    std::vector<rate_card_component_lookup> entries;
};

struct put_rate_card_component_request {
    using response_type = struct put_rate_card_component_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.rate_card_components.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    rate_card_component_change change;
    ores::utility::domain::change_intent intent;
};

struct put_rate_card_component_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::rate_card_component> rate_card_component;
};

struct put_many_rate_card_components_request {
    using response_type = struct put_many_rate_card_components_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.rate_card_components.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<rate_card_component_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_rate_card_components_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::rate_card_component> rate_card_components;
};

struct delete_rate_card_component_request {
    using response_type = struct delete_rate_card_component_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.rate_card_components.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    rate_card_component_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_rate_card_component_response {
    ores::utility::domain::result result;
};

struct delete_many_rate_card_components_request {
    using response_type = struct delete_many_rate_card_components_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.rate_card_components.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<rate_card_component_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_rate_card_components_response {
    ores::utility::domain::result result;
};

struct list_rate_card_component_versions_request {
    using response_type = struct list_rate_card_component_versions_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.rate_card_components_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    rate_card_component_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<rate_card_component_versions_filter> filter;
};

struct list_rate_card_component_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::rate_card_component> versions;
    std::uint64_t total;
};

struct get_rate_card_component_version_request {
    using response_type = struct get_rate_card_component_version_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.rate_card_components_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    rate_card_component_version_key key;
};

struct get_rate_card_component_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::rate_card_component> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace rate_card_component_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.rate_card_components_events.created";
inline constexpr std::string_view updated = "marketdata.v1.rate_card_components_events.updated";
inline constexpr std::string_view deleted = "marketdata.v1.rate_card_components_events.deleted";
}

}

#endif
