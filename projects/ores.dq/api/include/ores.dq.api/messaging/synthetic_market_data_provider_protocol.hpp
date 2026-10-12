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
#ifndef ORES_DQ_API_MESSAGING_SYNTHETIC_MARKET_DATA_PROVIDER_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_SYNTHETIC_MARKET_DATA_PROVIDER_PROTOCOL_HPP

#include "ores.dq.api/domain/synthetic_market_data_provider.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::messaging {

struct synthetic_market_data_provider_key {
    std::string code;
};

struct synthetic_market_data_provider_lookup {
    synthetic_market_data_provider_key key;
    std::optional<ores::dq::domain::synthetic_market_data_provider> synthetic_market_data_provider;
};

struct synthetic_market_data_providers_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct synthetic_market_data_provider_event {
    boost::uuids::uuid event_id;
    synthetic_market_data_provider_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_synthetic_market_data_providers_request {
    using response_type = struct list_synthetic_market_data_providers_response;
    static constexpr std::string_view nats_subject = "dq.v1.synthetic_market_data_providers.list";
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
    std::optional<synthetic_market_data_providers_filter> filter;
};

struct list_synthetic_market_data_providers_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::synthetic_market_data_provider> providers;
    std::uint64_t total;
};

struct get_synthetic_market_data_provider_request {
    using response_type = struct get_synthetic_market_data_provider_response;
    static constexpr std::string_view nats_subject = "dq.v1.synthetic_market_data_providers.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    synthetic_market_data_provider_key key;
};

struct get_synthetic_market_data_provider_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::synthetic_market_data_provider> synthetic_market_data_provider;
};

struct get_many_synthetic_market_data_providers_request {
    using response_type = struct get_many_synthetic_market_data_providers_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.synthetic_market_data_providers.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<synthetic_market_data_provider_key> keys;
};

struct get_many_synthetic_market_data_providers_response {
    ores::utility::domain::result result;
    std::vector<synthetic_market_data_provider_lookup> entries;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace synthetic_market_data_provider_event_subjects {
inline constexpr std::string_view created = "dq.v1.synthetic_market_data_providers_events.created";
inline constexpr std::string_view updated = "dq.v1.synthetic_market_data_providers_events.updated";
inline constexpr std::string_view deleted = "dq.v1.synthetic_market_data_providers_events.deleted";
}

}

#endif
