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
#ifndef ORES_DQ_API_MESSAGING_CATALOG_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_CATALOG_PROTOCOL_HPP

#include "ores.dq.api/domain/catalog.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::dq::messaging {

struct get_catalogs_request {
    using response_type = struct get_catalogs_response;
    static constexpr std::string_view nats_subject = "dq.v1.catalogs.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_catalogs_response {
    std::vector<ores::dq::domain::catalog> catalogs;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_catalog_request {
    using response_type = struct save_catalog_response;
    static constexpr std::string_view nats_subject = "dq.v1.catalogs.save";
    ores::dq::domain::catalog data;

    static save_catalog_request from(ores::dq::domain::catalog v) {
        return {.data = std::move(v)};
    }
};

struct save_catalog_response {
    bool success = false;
    std::string message;
};

struct delete_catalog_request {
    using response_type = struct delete_catalog_response;
    static constexpr std::string_view nats_subject = "dq.v1.catalogs.delete";
    std::vector<std::string> names;
};

struct delete_catalog_response {
    bool success = false;
    std::string message;
};

struct get_catalog_history_request {
    using response_type = struct get_catalog_history_response;
    static constexpr std::string_view nats_subject = "dq.v1.catalogs.history";
    std::string name;
};

struct get_catalog_history_response {
    std::vector<ores::dq::domain::catalog> history;
    bool success = false;
    std::string message;
};

}

#endif
