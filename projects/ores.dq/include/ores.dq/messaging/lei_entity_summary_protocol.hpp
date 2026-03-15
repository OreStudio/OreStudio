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
#ifndef ORES_DQ_MESSAGING_LEI_ENTITY_SUMMARY_PROTOCOL_HPP
#define ORES_DQ_MESSAGING_LEI_ENTITY_SUMMARY_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>

namespace ores::dq::messaging {

struct lei_entity_summary {
    std::string lei;
    std::string entity_legal_name;
    std::string entity_category;
    std::string country;
};

struct get_lei_entities_summary_request {
    using response_type = struct get_lei_entities_summary_response;
    static constexpr std::string_view nats_subject =
        "ores.dq.v1.lei-entities.summary";
    std::string country_filter;
    int offset = 0;
    int limit = 1000;
};

struct get_lei_entities_summary_response {
    bool success = false;
    std::string error_message;
    std::vector<lei_entity_summary> entities;
};

}

#endif
