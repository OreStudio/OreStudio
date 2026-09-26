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
#ifndef ORES_SYNTHETIC_API_MESSAGING_IR_CURVE_GENERATION_CONFIG_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_IR_CURVE_GENERATION_CONFIG_PROTOCOL_HPP

#include "ores.synthetic.api/domain/ir_curve_generation_config.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::messaging {

struct ir_curve_generation_config_key {
    boost::uuids::uuid id;
};

struct ir_curve_generation_config_write {
    boost::uuids::uuid id;
    boost::uuids::uuid config_id;
    std::string currency_code;
    std::string index_family;
    std::string tenor;
    std::string role;
    std::string process_type;
    int ticks_per_hour;
    bool enabled;
    bool auto_start;
    std::string price_source;
    std::string vintage_source;
    std::string vintage_date;
    std::string description;
    std::string fixed_leg_payment_frequency_code;
    std::string source_name;
    std::optional<boost::uuids::uuid> folder_id;
};

struct ir_curve_generation_config_change {
    ir_curve_generation_config_write write;
    ores::utility::domain::precondition precondition;
};

struct ir_curve_generation_config_removal {
    ir_curve_generation_config_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct ir_curve_generation_config_lookup {
    ir_curve_generation_config_key key;
    std::optional<ores::synthetic::domain::ir_curve_generation_config> ir_curve_generation_config;
};

struct ir_curve_generation_config_event {
    boost::uuids::uuid event_id;
    ir_curve_generation_config_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct ir_curve_generation_config_version_key {
    ir_curve_generation_config_key ir_curve_generation_config;
    std::uint32_t version;
};

struct ir_curve_generation_config_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_ir_curve_generation_configs_request {
    using response_type = struct list_ir_curve_generation_configs_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.ir_curve_generation_configs.list";
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

struct list_ir_curve_generation_configs_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::ir_curve_generation_config> ir_curve_generation_configs;
    std::uint64_t total;
};

struct get_ir_curve_generation_config_request {
    using response_type = struct get_ir_curve_generation_config_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ir_curve_generation_configs.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ir_curve_generation_config_key key;
};

struct get_ir_curve_generation_config_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::ir_curve_generation_config> ir_curve_generation_config;
};

struct get_many_ir_curve_generation_configs_request {
    using response_type = struct get_many_ir_curve_generation_configs_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.ir_curve_generation_configs.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ir_curve_generation_config_key> keys;
};

struct get_many_ir_curve_generation_configs_response {
    ores::utility::domain::result result;
    std::vector<ir_curve_generation_config_lookup> entries;
};

struct put_ir_curve_generation_config_request {
    using response_type = struct put_ir_curve_generation_config_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ir_curve_generation_configs.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ir_curve_generation_config_change change;
    ores::utility::domain::change_intent intent;
};

struct put_ir_curve_generation_config_response {
    ores::utility::domain::result result;
    ores::synthetic::domain::ir_curve_generation_config ir_curve_generation_config;
};

struct put_many_ir_curve_generation_configs_request {
    using response_type = struct put_many_ir_curve_generation_configs_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.ir_curve_generation_configs.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ir_curve_generation_config_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_ir_curve_generation_configs_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::ir_curve_generation_config> ir_curve_generation_configs;
};

struct delete_ir_curve_generation_config_request {
    using response_type = struct delete_ir_curve_generation_config_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.ir_curve_generation_configs.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ir_curve_generation_config_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_ir_curve_generation_config_response {
    ores::utility::domain::result result;
};

struct delete_many_ir_curve_generation_configs_request {
    using response_type = struct delete_many_ir_curve_generation_configs_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.ir_curve_generation_configs.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ir_curve_generation_config_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_ir_curve_generation_configs_response {
    ores::utility::domain::result result;
};

struct list_ir_curve_generation_config_versions_request {
    using response_type = struct list_ir_curve_generation_config_versions_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.ir_curve_generation_configs_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ir_curve_generation_config_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<ir_curve_generation_config_versions_filter> filter;
};

struct list_ir_curve_generation_config_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::ir_curve_generation_config> versions;
    std::uint64_t total;
};

struct get_ir_curve_generation_config_version_request {
    using response_type = struct get_ir_curve_generation_config_version_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.ir_curve_generation_configs_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ir_curve_generation_config_version_key key;
};

struct get_ir_curve_generation_config_version_response {
    ores::utility::domain::result result;
    ores::synthetic::domain::ir_curve_generation_config version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace ir_curve_generation_config_event_subjects {
inline constexpr std::string_view created =
    "synthetic.v1.ir_curve_generation_configs_events.created";
inline constexpr std::string_view updated =
    "synthetic.v1.ir_curve_generation_configs_events.updated";
inline constexpr std::string_view deleted =
    "synthetic.v1.ir_curve_generation_configs_events.deleted";
}

}

#endif
