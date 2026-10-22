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
#ifndef ORES_ANALYTICS_API_MESSAGING_PRICING_MODEL_PROTOCOL_HPP
#define ORES_ANALYTICS_API_MESSAGING_PRICING_MODEL_PROTOCOL_HPP

#include "ores.analytics.api/domain/pricing_model.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::analytics::messaging {

struct pricing_model_key {
    std::string code;
};

struct pricing_model_write {
    std::string code;
    std::string description;
    std::string detail;
    std::string source;
};

struct pricing_model_change {
    pricing_model_write write;
    ores::utility::domain::precondition precondition;
};

struct pricing_model_removal {
    pricing_model_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct pricing_model_lookup {
    pricing_model_key key;
    std::optional<ores::analytics::domain::pricing_model> pricing_model;
};

struct pricing_models_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct pricing_model_event {
    boost::uuids::uuid event_id;
    pricing_model_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct pricing_model_version_key {
    pricing_model_key pricing_model;
    std::uint32_t version;
};

struct pricing_model_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_pricing_models_request {
    using response_type = struct list_pricing_models_response;
    static constexpr std::string_view nats_subject = "analytics.v1.pricing_models.list";
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
    std::optional<pricing_models_filter> filter;
    std::optional<std::string> as_of;
};

struct list_pricing_models_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::pricing_model> models;
    std::uint64_t total;
};

struct get_pricing_model_request {
    using response_type = struct get_pricing_model_response;
    static constexpr std::string_view nats_subject = "analytics.v1.pricing_models.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    pricing_model_key key;
};

struct get_pricing_model_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::pricing_model> pricing_model;
};

struct get_many_pricing_models_request {
    using response_type = struct get_many_pricing_models_response;
    static constexpr std::string_view nats_subject = "analytics.v1.pricing_models.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<pricing_model_key> keys;
};

struct get_many_pricing_models_response {
    ores::utility::domain::result result;
    std::vector<pricing_model_lookup> entries;
};

struct put_pricing_model_request {
    using response_type = struct put_pricing_model_response;
    static constexpr std::string_view nats_subject = "analytics.v1.pricing_models.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    pricing_model_change change;
    ores::utility::domain::change_intent intent;
};

struct put_pricing_model_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::pricing_model> pricing_model;
};

struct put_many_pricing_models_request {
    using response_type = struct put_many_pricing_models_response;
    static constexpr std::string_view nats_subject = "analytics.v1.pricing_models.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<pricing_model_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_pricing_models_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::pricing_model> models;
};

struct delete_pricing_model_request {
    using response_type = struct delete_pricing_model_response;
    static constexpr std::string_view nats_subject = "analytics.v1.pricing_models.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    pricing_model_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_pricing_model_response {
    ores::utility::domain::result result;
};

struct delete_many_pricing_models_request {
    using response_type = struct delete_many_pricing_models_response;
    static constexpr std::string_view nats_subject = "analytics.v1.pricing_models.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<pricing_model_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_pricing_models_response {
    ores::utility::domain::result result;
};

struct list_pricing_model_versions_request {
    using response_type = struct list_pricing_model_versions_response;
    static constexpr std::string_view nats_subject = "analytics.v1.pricing_models_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    pricing_model_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<pricing_model_versions_filter> filter;
};

struct list_pricing_model_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::pricing_model> versions;
    std::uint64_t total;
};

struct get_pricing_model_version_request {
    using response_type = struct get_pricing_model_version_response;
    static constexpr std::string_view nats_subject = "analytics.v1.pricing_models_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    pricing_model_version_key key;
};

struct get_pricing_model_version_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::pricing_model> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace pricing_model_event_subjects {
inline constexpr std::string_view created = "analytics.v1.pricing_models_events.created";
inline constexpr std::string_view updated = "analytics.v1.pricing_models_events.updated";
inline constexpr std::string_view deleted = "analytics.v1.pricing_models_events.deleted";
}

}

#endif
