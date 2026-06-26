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
#ifndef ORES_REFDATA_API_MESSAGING_COUNTERPARTY_IDENTIFIER_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_COUNTERPARTY_IDENTIFIER_PROTOCOL_HPP

#include <cstdint>
#include <string>
#include <vector>
#include "ores.refdata.api/domain/counterparty_identifier.hpp"

namespace ores::refdata::messaging {

struct get_counterparty_identifiers_request {
    using response_type = struct get_counterparty_identifiers_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_identifiers.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_counterparty_identifiers_response {
    std::vector<ores::refdata::domain::counterparty_identifier> counterparty_identifiers;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_counterparty_identifier_request {
    using response_type = struct save_counterparty_identifier_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_identifiers.save";
    ores::refdata::domain::counterparty_identifier data;

    static save_counterparty_identifier_request from(ores::refdata::domain::counterparty_identifier v) {
        return {.data = std::move(v)};
    }
};

struct save_counterparty_identifier_response {
    bool success = false;
    std::string message;
};

struct delete_counterparty_identifier_request {
    using response_type = struct delete_counterparty_identifier_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_identifiers.delete";
    std::vector<std::string> ids;
};

struct delete_counterparty_identifier_response {
    bool success = false;
    std::string message;
};

struct get_counterparty_identifier_history_request {
    using response_type = struct get_counterparty_identifier_history_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_identifiers.history";
    std::string id;
};

struct get_counterparty_identifier_history_response {
    std::vector<ores::refdata::domain::counterparty_identifier> history;
    bool success = false;
    std::string message;
};

}

#endif
