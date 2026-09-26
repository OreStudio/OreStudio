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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_ORE_API_MESSAGING_ORE_IMPORT_ENGINE_PROTOCOL_HPP
#define ORES_ORE_API_MESSAGING_ORE_IMPORT_ENGINE_PROTOCOL_HPP

#include "ores.ore.api/messaging/ore_import_protocol.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::messaging {

/**
 * @brief Workflow step command: execute the full ORE import.
 *
 * Published by the workflow engine when the ore_import_workflow starts. The
 * handler fetches the packed tarball, scans, plans and saves every item, then
 * calls publish_step_completion.
 */
struct ore_import_execute_request {
    static constexpr std::string_view nats_subject = "ore.v1.ore.import.execute";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string request_id;
    std::string import_choices_json;
    std::string correlation_id;
    /** The caller's JWT, so the handler can delegate the caller's identity downstream. */
    std::string bearer_token;
};

/**
 * @brief The step's stored response.
 *
 * Carries everything compensation needs -- the identifiers of every entity
 * the step saved -- alongside the caller-facing item_errors list.
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
 * Published by the workflow engine when step 0's compensation is triggered.
 * The handler deletes every saved entity in reverse order and calls
 * publish_step_completion.
 */
struct ore_import_rollback_request {
    static constexpr std::string_view nats_subject = "ore.v1.ore.import.rollback";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string correlation_id;
    /** The caller's JWT, so the handler can delegate the caller's identity downstream. */
    std::string bearer_token;
    std::vector<std::string> saved_currency_iso_codes;
    std::vector<std::string> saved_portfolio_ids;
    std::vector<std::string> saved_book_ids;
    std::vector<std::string> saved_trade_ids;
};

}

#endif
