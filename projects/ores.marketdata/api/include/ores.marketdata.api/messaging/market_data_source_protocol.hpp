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
#ifndef ORES_MARKETDATA_API_MESSAGING_MARKET_DATA_SOURCE_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_MARKET_DATA_SOURCE_PROTOCOL_HPP

#include "ores.marketdata.api/domain/market_data_source.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

struct market_data_source_key {
    std::string name;
};

struct market_data_source_write {
    boost::uuids::uuid id;
    std::string name;
    std::string provider_code;
    std::string kind;
    bool is_default;
    std::string description;
};

struct market_data_source_change {
    market_data_source_write write;
    ores::utility::domain::precondition precondition;
};

struct market_data_source_removal {
    market_data_source_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct market_data_source_lookup {
    market_data_source_key key;
    std::optional<ores::marketdata::domain::market_data_source> market_data_source;
};

struct market_data_sources_filter {
    std::optional<std::string> provider_code;
    std::optional<std::string> kind;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<std::string>> provider_code_one_of;
    std::optional<std::vector<std::string>> kind_one_of;
    std::optional<std::string> search;
};

struct market_data_source_event {
    boost::uuids::uuid event_id;
    market_data_source_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct market_data_source_version_key {
    market_data_source_key market_data_source;
    std::uint32_t version;
};

struct market_data_source_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_market_data_sources_request {
    using response_type = struct list_market_data_sources_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_data_sources.list";
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
    std::optional<market_data_sources_filter> filter;
    std::optional<std::string> as_of;
};

struct list_market_data_sources_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_data_source> sources;
    std::uint64_t total;
};

struct get_market_data_source_request {
    using response_type = struct get_market_data_source_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_data_sources.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_data_source_key key;
};

struct get_market_data_source_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_data_source> market_data_source;
};

struct get_many_market_data_sources_request {
    using response_type = struct get_many_market_data_sources_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_data_sources.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_data_source_key> keys;
};

struct get_many_market_data_sources_response {
    ores::utility::domain::result result;
    std::vector<market_data_source_lookup> entries;
};

struct put_market_data_source_request {
    using response_type = struct put_market_data_source_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_data_sources.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_data_source_change change;
    ores::utility::domain::change_intent intent;
};

struct put_market_data_source_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_data_source> market_data_source;
};

struct put_many_market_data_sources_request {
    using response_type = struct put_many_market_data_sources_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_data_sources.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_data_source_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_market_data_sources_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_data_source> sources;
};

struct delete_market_data_source_request {
    using response_type = struct delete_market_data_source_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_data_sources.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_data_source_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_market_data_source_response {
    ores::utility::domain::result result;
};

struct delete_many_market_data_sources_request {
    using response_type = struct delete_many_market_data_sources_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_data_sources.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_data_source_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_market_data_sources_response {
    ores::utility::domain::result result;
};

struct list_market_data_source_versions_request {
    using response_type = struct list_market_data_source_versions_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_data_sources_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_data_source_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<market_data_source_versions_filter> filter;
};

struct list_market_data_source_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_data_source> versions;
    std::uint64_t total;
};

struct get_market_data_source_version_request {
    using response_type = struct get_market_data_source_version_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_data_sources_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_data_source_version_key key;
};

struct get_market_data_source_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_data_source> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace market_data_source_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.market_data_sources_events.created";
inline constexpr std::string_view updated = "marketdata.v1.market_data_sources_events.updated";
inline constexpr std::string_view deleted = "marketdata.v1.market_data_sources_events.deleted";
}

}

#endif
