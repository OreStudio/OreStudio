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
#ifndef ORES_MARKETDATA_API_MESSAGING_MARKET_OBSERVATION_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_MARKET_OBSERVATION_PROTOCOL_HPP

#include "ores.marketdata.api/domain/market_observation.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

struct get_market_observations_request {
    using response_type = struct get_market_observations_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_observations.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_market_observations_response {
    std::vector<ores::marketdata::domain::market_observation> market_observations;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_market_observation_request {
    using response_type = struct save_market_observation_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_observations.save";
    ores::marketdata::domain::market_observation data;

    static save_market_observation_request from(ores::marketdata::domain::market_observation v) {
        return {.data = std::move(v)};
    }
};

struct save_market_observation_response {
    bool success = false;
    std::string message;
};

struct delete_market_observation_request {
    using response_type = struct delete_market_observation_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_observations.delete";
    std::vector<std::string> ids;
};

struct delete_market_observation_response {
    bool success = false;
    std::string message;
};

struct get_market_observation_history_request {
    using response_type = struct get_market_observation_history_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_observations.history";
    std::string id;
};

struct get_market_observation_history_response {
    std::vector<ores::marketdata::domain::market_observation> history;
    bool success = false;
    std::string message;
};

}

#endif
