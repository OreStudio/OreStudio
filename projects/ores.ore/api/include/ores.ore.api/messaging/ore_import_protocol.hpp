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
#ifndef ORES_ORE_API_MESSAGING_ORE_IMPORT_PROTOCOL_HPP
#define ORES_ORE_API_MESSAGING_ORE_IMPORT_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::messaging {

/**
 * @brief Error for a single item within an ORE import.
 *
 * Carries the exact source file and item identifier so the caller can present
 * a precise failure location, such as "trades.xml / trade-0042".
 */
struct ore_import_item_error {
    std::string source_file;
    std::string item_id;
    std::string message;
};

/**
 * @brief Request to import an ORE directory already uploaded to storage.
 *
 * The caller uploads the packed directory to the ore-imports bucket as
 * {request_id}.tar.gz before sending this message.
 */
struct ore_import_request {
    using response_type = struct ore_import_response;
    static constexpr std::string_view nats_subject = "workflow.v1.ore.import";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** UUID; also the storage key root. */
    std::string request_id;
    std::string import_choices_json;
    std::string correlation_id;
};

/**
 * @brief Response for an ore_import_request.
 *
 * On partial failure success is still true and item_errors is non-empty; a
 * false success means the saga itself failed and compensation ran.
 */
struct ore_import_response {
    bool success = false;
    std::string message;
    std::vector<ore_import_item_error> item_errors;
    /**
     * Set when the import was dispatched asynchronously through the workflow
     * engine. The import then runs in the background, item_errors is empty in
     * this response, and this id queries the workflow's status.
     */
    std::string correlation_id;
    std::string workflow_instance_id;
};

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
