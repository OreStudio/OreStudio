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
#ifndef ORES_ORE_API_MESSAGING_ORE_IMPORT_ENGINE_PROTOCOL_HPP
#define ORES_ORE_API_MESSAGING_ORE_IMPORT_ENGINE_PROTOCOL_HPP

#include <string>
#include <vector>
#include <string_view>
#include "ores.ore.api/messaging/ore_import_protocol.hpp"

namespace ores::ore::messaging {

/**
 * @brief Workflow step command: execute the full ORE import.
 *
 * Published by the workflow engine to ore.v1.ore.import.execute when the
 * ore_import_workflow starts.  The handler in ores.ore.service fetches the
 * packed tarball, scans, plans, and saves all items, then calls
 * publish_step_completion.
 *
 * @note bearer_token is carried here so the handler can delegate the
 *       original caller's identity to downstream domain services.
 */
struct ore_import_execute_request {
    static constexpr std::string_view nats_subject = "ore.v1.ore.import.execute";

    std::string request_id;           ///< UUID; storage key root for the tarball
    std::string import_choices_json;  ///< JSON-serialised import_choices
    std::string correlation_id;
    std::string bearer_token;         ///< Caller's JWT — used for delegated NATS calls
};

/**
 * @brief Internal result stored as workflow_step.response_json for step 0.
 *
 * Includes everything needed for compensation (saved entity IDs) plus the
 * public-facing item_errors list.  Not sent over NATS directly.
 */
struct ore_import_execute_result {
    bool success = false;
    std::string message;
    std::string correlation_id;
    std::vector<ore_import_item_error> item_errors;
    std::vector<std::string> saved_currency_iso_codes;
    std::vector<std::string> saved_portfolio_ids;
    std::vector<std::string> saved_book_ids;
    std::vector<std::string> saved_trade_ids;
};

/**
 * @brief Workflow compensation command: roll back a completed ORE import.
 *
 * Published by the workflow engine to ore.v1.ore.import.rollback when step 0
 * compensation is triggered.  The handler deletes all saved entities in
 * reverse order and calls publish_step_completion.
 */
struct ore_import_rollback_request {
    static constexpr std::string_view nats_subject = "ore.v1.ore.import.rollback";

    std::string correlation_id;
    std::string bearer_token;         ///< Caller's JWT — used for delegated NATS calls
    std::vector<std::string> saved_currency_iso_codes;
    std::vector<std::string> saved_portfolio_ids;
    std::vector<std::string> saved_book_ids;
    std::vector<std::string> saved_trade_ids;
};

}

#endif
