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
#ifndef ORES_MARKETDATA_API_MESSAGING_ADAPTER_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_ADAPTER_PROTOCOL_HPP

#include "ores.marketdata.api/domain/adapter.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

struct adapter_key {
    std::string code;
};

struct adapter_write {
    std::string code;
    std::string name;
    std::string module_version;
    std::string description;
    std::string process;
    std::string asset_classes;
    std::string services;
    std::string configuration_schema;
    int display_order;
};

struct adapter_change {
    adapter_write write;
    ores::utility::domain::precondition precondition;
};

struct adapter_removal {
    adapter_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct adapter_lookup {
    adapter_key key;
    std::optional<ores::marketdata::domain::adapter> adapter;
};

struct adapters_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct adapter_event {
    boost::uuids::uuid event_id;
    adapter_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct adapter_version_key {
    adapter_key adapter;
    std::uint32_t version;
};

struct adapter_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_adapters_request {
    using response_type = struct list_adapters_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.adapters.list";
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
    std::optional<adapters_filter> filter;
    std::optional<std::string> as_of;
};

struct list_adapters_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::adapter> adapters;
    std::uint64_t total;
};

struct get_adapter_request {
    using response_type = struct get_adapter_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.adapters.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    adapter_key key;
};

struct get_adapter_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::adapter> adapter;
};

struct get_many_adapters_request {
    using response_type = struct get_many_adapters_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.adapters.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<adapter_key> keys;
};

struct get_many_adapters_response {
    ores::utility::domain::result result;
    std::vector<adapter_lookup> entries;
};

struct put_adapter_request {
    using response_type = struct put_adapter_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.adapters.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    adapter_change change;
    ores::utility::domain::change_intent intent;
};

struct put_adapter_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::adapter> adapter;
};

struct put_many_adapters_request {
    using response_type = struct put_many_adapters_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.adapters.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<adapter_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_adapters_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::adapter> adapters;
};

struct delete_adapter_request {
    using response_type = struct delete_adapter_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.adapters.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    adapter_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_adapter_response {
    ores::utility::domain::result result;
};

struct delete_many_adapters_request {
    using response_type = struct delete_many_adapters_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.adapters.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<adapter_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_adapters_response {
    ores::utility::domain::result result;
};

struct list_adapter_versions_request {
    using response_type = struct list_adapter_versions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.adapters_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    adapter_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<adapter_versions_filter> filter;
};

struct list_adapter_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::adapter> versions;
    std::uint64_t total;
};

struct get_adapter_version_request {
    using response_type = struct get_adapter_version_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.adapters_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    adapter_version_key key;
};

struct get_adapter_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::adapter> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace adapter_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.adapters_events.created";
inline constexpr std::string_view updated = "marketdata.v1.adapters_events.updated";
inline constexpr std::string_view deleted = "marketdata.v1.adapters_events.deleted";
}

}

#endif
