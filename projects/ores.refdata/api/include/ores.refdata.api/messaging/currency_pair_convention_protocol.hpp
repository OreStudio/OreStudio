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
#ifndef ORES_REFDATA_API_MESSAGING_CURRENCY_PAIR_CONVENTION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CURRENCY_PAIR_CONVENTION_PROTOCOL_HPP

#include "ores.refdata.api/domain/currency_pair_convention.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct get_currency_pair_conventions_request {
    using response_type = struct get_currency_pair_conventions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currency_pair_conventions.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_currency_pair_conventions_response {
    std::vector<ores::refdata::domain::currency_pair_convention> conventions;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_currency_pair_convention_request {
    using response_type = struct save_currency_pair_convention_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currency_pair_conventions.save";
    ores::refdata::domain::currency_pair_convention data;

    static save_currency_pair_convention_request
    from(ores::refdata::domain::currency_pair_convention v) {
        return {.data = std::move(v)};
    }
};

struct save_currency_pair_convention_response {
    bool success = false;
    std::string message;
};

struct delete_currency_pair_convention_request {
    using response_type = struct delete_currency_pair_convention_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currency_pair_conventions.delete";
    std::vector<std::string> pair_codes;
};

struct delete_currency_pair_convention_response {
    bool success = false;
    std::string message;
};

struct get_currency_pair_convention_history_request {
    using response_type = struct get_currency_pair_convention_history_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currency_pair_conventions.history";
    std::string pair_code;
};

struct get_currency_pair_convention_history_response {
    std::vector<ores::refdata::domain::currency_pair_convention> history;
    bool success = false;
    std::string message;
};

/**
 * @brief Reads all active currency_pair_conventions for a tenant — used by
 * client-side caches to warm up without multiple round-trips.
 */
struct read_currency_pair_conventions_for_cache_request {
    using response_type = struct read_currency_pair_conventions_for_cache_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currency_pair_conventions.read";
    std::string tenant_id;
};

struct read_currency_pair_conventions_for_cache_response {
    bool success = false;
    std::string message;
    std::vector<ores::refdata::domain::currency_pair_convention> conventions;
};

}

#endif
