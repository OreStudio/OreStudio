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
#ifndef ORES_REFDATA_MESSAGING_DEPOSIT_CONVENTION_PROTOCOL_HPP
#define ORES_REFDATA_MESSAGING_DEPOSIT_CONVENTION_PROTOCOL_HPP

#include <string>
#include <vector>
#include "ores.refdata.api/domain/deposit_convention.hpp"

namespace ores::refdata::messaging {

struct get_deposit_conventions_request {
    using response_type = struct get_deposit_conventions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.deposit_conventions.list";
};

struct get_deposit_conventions_response {
    std::vector<ores::refdata::domain::deposit_convention> deposit_conventions;
    int total_available_count = 0;
    bool success = true;
    std::string message;
};

struct save_deposit_convention_request {
    using response_type = struct save_deposit_convention_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.deposit_conventions.save";
    ores::refdata::domain::deposit_convention data;
};

struct save_deposit_convention_response {
    bool success = false;
    std::string message;
};

struct delete_deposit_convention_request {
    using response_type = struct delete_deposit_convention_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.deposit_conventions.delete";
    std::vector<std::string> codes;
};

struct delete_deposit_convention_response {
    bool success = false;
    std::string message;
};

struct get_deposit_convention_history_request {
    using response_type = struct get_deposit_convention_history_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.deposit_conventions.history";
    std::string id;
};

struct get_deposit_convention_history_response {
    std::vector<ores::refdata::domain::deposit_convention> deposit_conventions;
    bool success = false;
    std::string message;
};

}

#endif
