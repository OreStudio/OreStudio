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
#ifndef ORES_DQ_API_MESSAGING_DATA_DOMAIN_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_DATA_DOMAIN_PROTOCOL_HPP

#include "ores.dq.api/domain/data_domain.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::dq::messaging {

struct get_data_domains_request {
    using response_type = struct get_data_domains_response;
    static constexpr std::string_view nats_subject = "dq.v1.data_domains.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_data_domains_response {
    std::vector<ores::dq::domain::data_domain> domains;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_data_domain_request {
    using response_type = struct save_data_domain_response;
    static constexpr std::string_view nats_subject = "dq.v1.data_domains.save";
    ores::dq::domain::data_domain data;

    static save_data_domain_request from(ores::dq::domain::data_domain v) {
        return {.data = std::move(v)};
    }
};

struct save_data_domain_response {
    bool success = false;
    std::string message;
};

struct delete_data_domain_request {
    using response_type = struct delete_data_domain_response;
    static constexpr std::string_view nats_subject = "dq.v1.data_domains.delete";
    std::vector<std::string> names;
};

struct delete_data_domain_response {
    bool success = false;
    std::string message;
};

struct get_data_domain_history_request {
    using response_type = struct get_data_domain_history_response;
    static constexpr std::string_view nats_subject = "dq.v1.data_domains.history";
    std::string name;
};

struct get_data_domain_history_response {
    std::vector<ores::dq::domain::data_domain> history;
    bool success = false;
    std::string message;
};

}

#endif
