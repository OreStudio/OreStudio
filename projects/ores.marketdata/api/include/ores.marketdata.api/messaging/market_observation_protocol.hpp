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

#include <string>
#include <string_view>
#include <vector>
#include "ores.marketdata.api/domain/market_observation.hpp"

namespace ores::marketdata::messaging {

struct get_market_observations_request {
    using response_type = struct get_market_observations_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.observations.list";
    std::string series_id;
    std::string from_date;  // ISO "YYYY-MM-DD", empty = no lower bound
    std::string to_date;    // ISO "YYYY-MM-DD", empty = no upper bound
};

struct get_market_observations_response {
    std::vector<domain::market_observation> observations;
    int total_available_count = 0;
};

struct save_market_observations_request {
    using response_type = struct save_market_observations_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.observations.save";
    std::vector<domain::market_observation> observations;
};

struct save_market_observations_response {
    bool success = false;
    std::string message;
    int saved_count = 0;
};

struct delete_market_observations_request {
    using response_type = struct delete_market_observations_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.observations.delete";
    std::string series_id;
};

struct delete_market_observations_response {
    bool success = false;
    std::string message;
};

}

#endif
