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
#ifndef ORES_DQ_API_MESSAGING_LEI_ENTITY_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_LEI_ENTITY_PROTOCOL_HPP

#include "ores.dq.api/domain/lei_entity.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::dq::messaging {

struct get_lei_entities_request {
    using response_type = struct get_lei_entities_response;
    static constexpr std::string_view nats_subject = "dq.v1.lei_entities.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_lei_entities_response {
    std::vector<ores::dq::domain::lei_entity> entities;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_lei_entity_request {
    using response_type = struct save_lei_entity_response;
    static constexpr std::string_view nats_subject = "dq.v1.lei_entities.save";
    ores::dq::domain::lei_entity data;

    static save_lei_entity_request from(ores::dq::domain::lei_entity v) {
        return {.data = std::move(v)};
    }
};

struct save_lei_entity_response {
    bool success = false;
    std::string message;
};

struct delete_lei_entity_request {
    using response_type = struct delete_lei_entity_response;
    static constexpr std::string_view nats_subject = "dq.v1.lei_entities.delete";
    std::vector<std::string> leis;
};

struct delete_lei_entity_response {
    bool success = false;
    std::string message;
};

struct get_lei_entity_history_request {
    using response_type = struct get_lei_entity_history_response;
    static constexpr std::string_view nats_subject = "dq.v1.lei_entities.history";
    std::string lei;
};

struct get_lei_entity_history_response {
    std::vector<ores::dq::domain::lei_entity> history;
    bool success = false;
    std::string message;
};

}

#endif
