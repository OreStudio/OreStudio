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
#ifndef ORES_ANALYTICS_API_MESSAGING_PRICING_MODEL_PRODUCT_PARAMETER_PROTOCOL_HPP
#define ORES_ANALYTICS_API_MESSAGING_PRICING_MODEL_PRODUCT_PARAMETER_PROTOCOL_HPP

#include "ores.analytics.api/domain/pricing_model_product_parameter.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::analytics::messaging {

struct pricing_model_product_parameter_key {
    std::string parameter_name;
};

struct pricing_model_product_parameter_write {
    boost::uuids::uuid id;
    boost::uuids::uuid pricing_model_config_id;
    std::optional<boost::uuids::uuid> pricing_model_product_id;
    std::string parameter_scope;
    std::string parameter_name;
    std::string parameter_value;
};

struct pricing_model_product_parameter_change {
    pricing_model_product_parameter_write write;
    ores::utility::domain::precondition precondition;
};

struct pricing_model_product_parameter_removal {
    pricing_model_product_parameter_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct pricing_model_product_parameter_lookup {
    pricing_model_product_parameter_key key;
    std::optional<ores::analytics::domain::pricing_model_product_parameter>
        pricing_model_product_parameter;
};

struct pricing_model_product_parameter_event {
    boost::uuids::uuid event_id;
    pricing_model_product_parameter_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct pricing_model_product_parameter_version_key {
    pricing_model_product_parameter_key pricing_model_product_parameter;
    std::uint32_t version;
};

struct pricing_model_product_parameter_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_pricing_model_product_parameters_request {
    using response_type = struct list_pricing_model_product_parameters_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_model_product_parameters.list";
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

struct list_pricing_model_product_parameters_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::pricing_model_product_parameter> parameters;
    std::uint64_t total;
};

struct get_pricing_model_product_parameter_request {
    using response_type = struct get_pricing_model_product_parameter_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_model_product_parameters.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    pricing_model_product_parameter_key key;
};

struct get_pricing_model_product_parameter_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::pricing_model_product_parameter>
        pricing_model_product_parameter;
};

struct get_many_pricing_model_product_parameters_request {
    using response_type = struct get_many_pricing_model_product_parameters_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_model_product_parameters.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<pricing_model_product_parameter_key> keys;
};

struct get_many_pricing_model_product_parameters_response {
    ores::utility::domain::result result;
    std::vector<pricing_model_product_parameter_lookup> entries;
};

struct put_pricing_model_product_parameter_request {
    using response_type = struct put_pricing_model_product_parameter_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_model_product_parameters.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    pricing_model_product_parameter_change change;
    ores::utility::domain::change_intent intent;
};

struct put_pricing_model_product_parameter_response {
    ores::utility::domain::result result;
    ores::analytics::domain::pricing_model_product_parameter pricing_model_product_parameter;
};

struct put_many_pricing_model_product_parameters_request {
    using response_type = struct put_many_pricing_model_product_parameters_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_model_product_parameters.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<pricing_model_product_parameter_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_pricing_model_product_parameters_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::pricing_model_product_parameter> parameters;
};

struct delete_pricing_model_product_parameter_request {
    using response_type = struct delete_pricing_model_product_parameter_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_model_product_parameters.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    pricing_model_product_parameter_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_pricing_model_product_parameter_response {
    ores::utility::domain::result result;
};

struct delete_many_pricing_model_product_parameters_request {
    using response_type = struct delete_many_pricing_model_product_parameters_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_model_product_parameters.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<pricing_model_product_parameter_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_pricing_model_product_parameters_response {
    ores::utility::domain::result result;
};

struct list_pricing_model_product_parameter_versions_request {
    using response_type = struct list_pricing_model_product_parameter_versions_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_model_product_parameters_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    pricing_model_product_parameter_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<pricing_model_product_parameter_versions_filter> filter;
};

struct list_pricing_model_product_parameter_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::pricing_model_product_parameter> versions;
    std::uint64_t total;
};

struct get_pricing_model_product_parameter_version_request {
    using response_type = struct get_pricing_model_product_parameter_version_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_model_product_parameters_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    pricing_model_product_parameter_version_key key;
};

struct get_pricing_model_product_parameter_version_response {
    ores::utility::domain::result result;
    ores::analytics::domain::pricing_model_product_parameter version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace pricing_model_product_parameter_event_subjects {
inline constexpr std::string_view created =
    "analytics.v1.pricing_model_product_parameters_events.created";
inline constexpr std::string_view updated =
    "analytics.v1.pricing_model_product_parameters_events.updated";
inline constexpr std::string_view deleted =
    "analytics.v1.pricing_model_product_parameters_events.deleted";
}

}

#endif
