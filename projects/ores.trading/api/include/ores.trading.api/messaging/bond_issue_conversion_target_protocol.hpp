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
#ifndef ORES_TRADING_API_MESSAGING_BOND_ISSUE_CONVERSION_TARGET_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_BOND_ISSUE_CONVERSION_TARGET_PROTOCOL_HPP

#include "ores.trading.api/domain/bond_issue_conversion_target.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct get_bond_issue_conversion_targets_request {
    using response_type = struct get_bond_issue_conversion_targets_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.bond_issue_conversion_targets.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_bond_issue_conversion_targets_response {
    std::vector<ores::trading::domain::bond_issue_conversion_target> conversion_targets;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_bond_issue_conversion_target_request {
    using response_type = struct save_bond_issue_conversion_target_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.bond_issue_conversion_targets.save";
    ores::trading::domain::bond_issue_conversion_target data;

    static save_bond_issue_conversion_target_request
    from(ores::trading::domain::bond_issue_conversion_target v) {
        return {.data = std::move(v)};
    }
};

struct save_bond_issue_conversion_target_response {
    bool success = false;
    std::string message;
};

struct delete_bond_issue_conversion_target_request {
    using response_type = struct delete_bond_issue_conversion_target_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.bond_issue_conversion_targets.delete";
    std::vector<std::string> ids;
    std::vector<std::string> sequence_numbers;
};

struct delete_bond_issue_conversion_target_response {
    bool success = false;
    std::string message;
};

struct get_bond_issue_conversion_target_history_request {
    using response_type = struct get_bond_issue_conversion_target_history_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.bond_issue_conversion_targets.history";
    std::string issue_id;
    std::string sequence_number;
};

struct get_bond_issue_conversion_target_history_response {
    std::vector<ores::trading::domain::bond_issue_conversion_target> history;
    bool success = false;
    std::string message;
};

}

#endif
