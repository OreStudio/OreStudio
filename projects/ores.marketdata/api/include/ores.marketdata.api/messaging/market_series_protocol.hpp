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
#ifndef ORES_MARKETDATA_API_MESSAGING_MARKET_SERIES_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_MARKET_SERIES_PROTOCOL_HPP

#include "ores.marketdata.api/domain/market_series.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

struct get_market_series_request {
    using response_type = struct get_market_series_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_series.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_market_series_response {
    std::vector<ores::marketdata::domain::market_series> market_series;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_market_series_request {
    using response_type = struct save_market_series_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_series.save";
    ores::marketdata::domain::market_series data;

    static save_market_series_request from(ores::marketdata::domain::market_series v) {
        return {.data = std::move(v)};
    }
};

struct save_market_series_response {
    bool success = false;
    std::string message;
};

struct delete_market_series_request {
    using response_type = struct delete_market_series_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_series.delete";
    std::vector<std::string> ids;
};

struct delete_market_series_response {
    bool success = false;
    std::string message;
};

struct get_market_series_history_request {
    using response_type = struct get_market_series_history_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_series.history";
    std::string id;
};

struct get_market_series_history_response {
    std::vector<ores::marketdata::domain::market_series> history;
    bool success = false;
    std::string message;
};

}

#endif
