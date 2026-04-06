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
#ifndef ORES_REPORTING_API_MESSAGING_REPORT_EXECUTION_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_REPORT_EXECUTION_PROTOCOL_HPP

#include <string>
#include <vector>
#include <string_view>

namespace ores::reporting::messaging {

/**
 * @brief Stored as workflow_instance.request_json for report execution.
 *
 * Contains everything the step command builders need to dispatch
 * commands without accessing the database.
 */
struct report_execution_request {
    std::string report_instance_id;
    std::string definition_id;
    std::string tenant_id;
    std::string correlation_id;
};

/**
 * @brief Step 0: gather trades from the trading service.
 *
 * Loads the risk_report_config for the definition, resolves book/portfolio
 * scope, and fetches all trades with resolved instruments via
 * trading.v1.trades.portfolio.export.
 */
struct gather_trades_request {
    static constexpr std::string_view nats_subject =
        "reporting.v1.report.gather-trades";
    std::string report_instance_id;
    std::string definition_id;
    std::string tenant_id;
    std::string correlation_id;
};

struct gather_trades_result {
    bool success = false;
    std::string message;
    int trade_count = 0;
    std::string storage_key;  ///< Object storage key for MsgPack-serialised trades
};

/**
 * @brief Step 1: gather market data series.
 *
 * Fetches market data series relevant to the trade portfolio from
 * marketdata.v1.series.list.
 */
struct gather_market_data_request {
    static constexpr std::string_view nats_subject =
        "reporting.v1.report.gather-market-data";
    std::string report_instance_id;
    std::string definition_id;
    std::string tenant_id;
    std::string correlation_id;
};

struct gather_market_data_result {
    bool success = false;
    std::string message;
    int series_count = 0;
    std::string storage_key;  ///< Object storage key for MsgPack-serialised market data
};

/**
 * @brief Step 2: assemble and persist the report input bundle.
 *
 * Aggregates the object-storage keys produced by gather_trades and
 * gather_market_data into a report_input_bundle record and persists it.
 * Storage keys and counts are forwarded from prior step results by the
 * workflow engine via the build_command lambda.
 */
struct assemble_bundle_request {
    static constexpr std::string_view nats_subject =
        "reporting.v1.report.assemble-bundle";
    std::string report_instance_id;
    std::string definition_id;
    std::string tenant_id;
    std::string correlation_id;
    std::string trades_storage_key;
    std::string market_data_storage_key;
    int trade_count = 0;
    int series_count = 0;
};

struct assemble_bundle_result {
    bool success = false;
    std::string message;
    std::string bundle_id;   ///< UUID of the persisted report_input_bundle
};

/**
 * @brief Step 3: ORE mapping and tarball packaging (ores.ore.service).
 *
 * Storage keys are forwarded by the workflow engine from the
 * assemble_bundle_result so that ore.service can download the blobs
 * without an extra round-trip to the reporting database.
 */
struct prepare_ore_package_request {
    static constexpr std::string_view nats_subject =
        "ore.v1.report.prepare-package";
    std::string report_instance_id;
    std::string bundle_id;
    std::string tenant_id;
    std::string correlation_id;
    std::string trades_storage_key;
    std::string market_data_storage_key;
};

struct prepare_ore_package_result {
    bool success = false;
    std::string message;
    std::vector<std::string> tarball_uris;
};

/**
 * @brief Step 4: submit tarballs to the compute grid (ores.compute.service).
 */
struct submit_compute_request {
    static constexpr std::string_view nats_subject =
        "compute.v1.report.submit";
    std::string report_instance_id;
    std::string tenant_id;
    std::string correlation_id;
    std::vector<std::string> tarball_uris;
};

struct submit_compute_result {
    bool success = false;
    std::string message;
    std::string batch_id;
};

/**
 * @brief Final step: mark the report instance as completed.
 */
struct finalise_report_request {
    static constexpr std::string_view nats_subject =
        "reporting.v1.report.finalise";
    std::string report_instance_id;
    std::string tenant_id;
    std::string correlation_id;
};

struct finalise_report_result {
    bool success = false;
    std::string message;
};

/**
 * @brief Compensation: mark report instance as failed.
 *
 * Published by the workflow engine when compensation is triggered.
 * Sets the report instance FSM to failed with the error in output_message.
 */
struct fail_report_request {
    static constexpr std::string_view nats_subject =
        "reporting.v1.report.fail";
    std::string report_instance_id;
    std::string tenant_id;
    std::string correlation_id;
    std::string error_message;
};

struct fail_report_result {
    bool success = false;
    std::string message;
};

}

#endif
