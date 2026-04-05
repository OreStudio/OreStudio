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
#ifndef ORES_ORE_API_MESSAGING_ORE_IMPORT_PROTOCOL_HPP
#define ORES_ORE_API_MESSAGING_ORE_IMPORT_PROTOCOL_HPP

#include <string>
#include <vector>
#include <string_view>

namespace ores::ore::messaging {

/**
 * @brief Error for a single item within an ORE import.
 *
 * Carries the exact source file and item identifier so the client can
 * present the user with a precise failure location (e.g. "trades.xml /
 * trade-0042").
 */
struct ore_import_item_error {
    std::string source_file;  ///< Relative path within the unpacked ORE directory
    std::string item_id;      ///< Trade ID, ISO code, series key, etc.
    std::string message;      ///< Human-readable error from the downstream service
};

/**
 * @brief Request to import an ORE directory that has been uploaded to storage.
 *
 * The client uploads the packed ORE directory tarball to
 * "ore-imports/{request_id}.tar.gz" before sending this message.
 */
struct ore_import_request {
    using response_type = struct ore_import_response;
    static constexpr std::string_view nats_subject = "workflow.v1.ore.import";

    std::string request_id;           ///< UUID; also the storage key root
    std::string import_choices_json;  ///< JSON-serialised import_choices
    std::string correlation_id;       ///< Propagated from the Qt client for log correlation
};

/**
 * @brief Response for an ore_import_request.
 *
 * On partial failure (some trades rejected), @p success is still true and
 * @p item_errors is non-empty.  A false @p success means the saga itself
 * failed (infrastructure error, compensation triggered).
 */
struct ore_import_response {
    bool success = false;
    std::string message;
    std::vector<ore_import_item_error> item_errors;
    std::string correlation_id;  ///< Echoed back for display and log lookup
    /**
     * @brief Workflow instance UUID, non-empty when the import was dispatched
     *        asynchronously via the workflow engine.
     *
     * When set, the import is running in the background and item_errors is
     * always empty in this response.  Use this ID to query workflow status.
     */
    std::string workflow_instance_id;
};

}

#endif
