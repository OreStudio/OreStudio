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
#ifndef ORES_SYNTHETIC_API_MESSAGING_MARKET_DATA_GENERATION_CONFIG_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_MARKET_DATA_GENERATION_CONFIG_PROTOCOL_HPP

#include "ores.synthetic.api/domain/market_data_generation_config.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::synthetic::messaging {

struct get_market_data_generation_configs_request {
    using response_type = struct get_market_data_generation_configs_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.market_data_generation_configs.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_market_data_generation_configs_response {
    std::vector<ores::synthetic::domain::market_data_generation_config>
        market_data_generation_configs;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_market_data_generation_config_request {
    using response_type = struct save_market_data_generation_config_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.market_data_generation_configs.save";
    ores::synthetic::domain::market_data_generation_config data;

    static save_market_data_generation_config_request
    from(ores::synthetic::domain::market_data_generation_config v) {
        return {.data = std::move(v)};
    }
};

struct save_market_data_generation_config_response {
    bool success = false;
    std::string message;
};

struct delete_market_data_generation_config_request {
    using response_type = struct delete_market_data_generation_config_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.market_data_generation_configs.delete";
    std::vector<std::string> ids;
};

struct delete_market_data_generation_config_response {
    bool success = false;
    std::string message;
};

struct get_market_data_generation_config_history_request {
    using response_type = struct get_market_data_generation_config_history_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.market_data_generation_configs.history";
    std::string id;
};

struct get_market_data_generation_config_history_response {
    std::vector<ores::synthetic::domain::market_data_generation_config> history;
    bool success = false;
    std::string message;
};

}

#endif
