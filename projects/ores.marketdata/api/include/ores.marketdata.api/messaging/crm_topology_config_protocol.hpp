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
#ifndef ORES_MARKETDATA_API_MESSAGING_CRM_TOPOLOGY_CONFIG_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_CRM_TOPOLOGY_CONFIG_PROTOCOL_HPP

#include "ores.marketdata.api/domain/crm_topology_config.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

struct get_crm_topology_configs_request {
    using response_type = struct get_crm_topology_configs_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.crm_topology_configs.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_crm_topology_configs_response {
    std::vector<ores::marketdata::domain::crm_topology_config> crm_topology_configs;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_crm_topology_config_request {
    using response_type = struct save_crm_topology_config_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.crm_topology_configs.save";
    ores::marketdata::domain::crm_topology_config data;

    static save_crm_topology_config_request from(ores::marketdata::domain::crm_topology_config v) {
        return {.data = std::move(v)};
    }
};

struct save_crm_topology_config_response {
    bool success = false;
    std::string message;
};

struct delete_crm_topology_config_request {
    using response_type = struct delete_crm_topology_config_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.crm_topology_configs.delete";
    std::vector<std::string> ids;
};

struct delete_crm_topology_config_response {
    bool success = false;
    std::string message;
};

struct get_crm_topology_config_history_request {
    using response_type = struct get_crm_topology_config_history_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.crm_topology_configs.history";
    std::string id;
};

struct get_crm_topology_config_history_response {
    std::vector<ores::marketdata::domain::crm_topology_config> history;
    bool success = false;
    std::string message;
};

}

#endif
