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
#ifndef ORES_TRADING_API_MESSAGING_BOND_ISSUE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_BOND_ISSUE_PROTOCOL_HPP

#include "ores.trading.api/domain/bond_issue.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct get_bond_issues_request {
    using response_type = struct get_bond_issues_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issues.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_bond_issues_response {
    std::vector<ores::trading::domain::bond_issue> issues;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_bond_issue_request {
    using response_type = struct save_bond_issue_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issues.save";
    ores::trading::domain::bond_issue data;

    static save_bond_issue_request from(ores::trading::domain::bond_issue v) {
        return {.data = std::move(v)};
    }
};

struct save_bond_issue_response {
    bool success = false;
    std::string message;
};

struct delete_bond_issue_request {
    using response_type = struct delete_bond_issue_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issues.delete";
    std::vector<std::string> ids;
};

struct delete_bond_issue_response {
    bool success = false;
    std::string message;
};

struct get_bond_issue_history_request {
    using response_type = struct get_bond_issue_history_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issues.history";
    std::string issue_id;
};

struct get_bond_issue_history_response {
    std::vector<ores::trading::domain::bond_issue> history;
    bool success = false;
    std::string message;
};

}

#endif
