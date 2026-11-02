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
#include "ores.marketdata.core/messaging/market_state_handler.hpp"
#include "ores.marketdata.core/repository/market_calibration_value_repository.hpp"
#include "ores.marketdata.core/repository/market_scenario_value_repository.hpp"
#include "ores.marketdata.core/service/market_state_reader.hpp"
#include "ores.ore.core/results/output_catalogue.hpp"
#include "ores.ore.core/results/output_manifest.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.storage.api/net/storage_paths.hpp"
#include "ores.storage.core/net/storage_transfer.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cstdint>
#include <format>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {
namespace {

/**
 * @brief The run's market-state files, fetched by name through each manifest.
 *
 * A manifest names every file one result wrote. Only the market-state files are
 * fetched, because a result file can be far larger than the state; the rest are
 * named, so the reply can say what was left alone.
 */
bool collect_output_files(const messaging::store_market_state_request& req,
                          const ores::ore::results::family_selection& families,
                          const std::string& http_base_url,
                          std::vector<service::market_state_file>& files,
                          std::string& failure) {
    ores::storage::net::storage_transfer transfer(http_base_url, req.storage_token);
    boost::uuids::string_generator sg;

    for (const auto& uri : req.manifest_uris) {
        std::string bucket;
        std::string key;
        if (!ores::storage::net::storage_paths::split_object_path(uri, bucket, key)) {
            failure = std::format("The manifest '{}' names no readable location.", uri);
            return false;
        }

        try {
            const auto text = transfer.download_blob(bucket, key);
            const auto manifest =
                ores::ore::results::manifest_from_json(std::string_view(text.data(), text.size()));
            const auto located = ores::ore::results::located_files(manifest, key);
            const auto result_id = sg(manifest.result_id);
            for (const auto& file : located) {
                if (!ores::ore::results::is_market_state_file(file) ||
                    !families.stores(file.family)) {
                    files.push_back(
                        service::market_state_file{.result_id = result_id, .name = file.name});
                    continue;
                }
                const auto body = transfer.download_blob(bucket, file.key);
                std::string text_body(body.begin(), body.end());
                ores::ore::results::verify_body(file, text_body);
                files.push_back(service::market_state_file{
                    .result_id = result_id, .name = file.name, .body = std::move(text_body)});
            }
        } catch (const std::exception& e) {
            failure = std::format("The output named by '{}' was not read: {}", uri, e.what());
            return false;
        }
    }

    return true;
}

}

void market_state_handler::store(ores::nats::message msg, const std::string& http_base_url) {
    [[maybe_unused]] const auto correlation_id = log_handler_entry(market_state_handler_lg(), msg);
    auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
    if (!ctx_expected) {
        error_reply(nats_, msg, ctx_expected.error());
        return;
    }
    const auto& ctx = *ctx_expected;
    if (!has_permission(ctx, "marketdata::market_scenario_values:write") ||
        !has_permission(ctx, "marketdata::market_calibration_values:write")) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }
    auto req = decode<store_market_state_request>(msg);
    if (!req) {
        BOOST_LOG_SEV(market_state_handler_lg(), warn) << "Failed to decode: " << msg.subject;
        return;
    }

    store_market_state_response response;
    const auto refuse = [&](const std::string& failure) {
        response.result.outcome = ores::utility::domain::outcome::unavailable;
        response.result.message = failure;
        BOOST_LOG_SEV(market_state_handler_lg(), error) << failure;
        reply(nats_, msg, response);
    };

    boost::uuids::string_generator sg;
    service::market_state_run run;
    try {
        run = service::market_state_run{
            .party_id = sg(req->party_id),
            .report_instance_id = sg(req->report_instance_id),
            .report_definition_id = sg(req->report_definition_id),
            .as_of = std::chrono::sys_days{
                ores::platform::time::datetime::from_iso8601_date(req->as_of)},
            .families = ores::ore::results::family_selection_of(req->skipped_families)};
    } catch (const std::exception& e) {
        refuse(std::string("The request names no readable run: ") + e.what());
        return;
    }

    std::vector<service::market_state_file> files;
    std::string failure;
    if (!collect_output_files(*req, run.families, http_base_url, files, failure)) {
        refuse(failure);
        return;
    }
    auto outcome = service::read_market_state(files, run);
    response.left_alone = outcome.left_alone;
    response.left_alone.insert(
        response.left_alone.end(), outcome.skipped.begin(), outcome.skipped.end());
    for (const auto& skipped : outcome.skipped)
        BOOST_LOG_SEV(market_state_handler_lg(), info)
            << "store_market_state | instance=" << req->report_instance_id << " skipped "
            << skipped << ": its definition does not store the family";
    response.refusals = outcome.refusals;
    // A run's state is stored whole or not at all, because a partial store
    // reads as the state the run priced against.
    if (!outcome.refusals.empty()) {
        refuse("The run's market state was refused: " + outcome.refusals.front());
        return;
    }
    response.families = outcome.families;
    response.files = static_cast<int>(outcome.families.size());
    response.rows = static_cast<std::int64_t>(outcome.scenario_values.size() +
                                              outcome.calibration_values.size());
    if (response.files == 0) {
        response.result.message = "The run wrote no market state, so there is nothing to store.";
        reply(nats_, msg, response);
        return;
    }

    try {
        if (!outcome.scenario_values.empty()) {
            repository::market_scenario_value_repository repo;
            repo.insert(ctx, outcome.scenario_values);
        }
        if (!outcome.calibration_values.empty()) {
            repository::market_calibration_value_repository repo;
            repo.insert(ctx, outcome.calibration_values);
        }
    } catch (const std::exception& e) {
        refuse(std::string("The run's market state was not stored: ") + e.what());
        return;
    }

    response.result.outcome = ores::utility::domain::outcome::ok;
    response.result.message = std::format(
        "Stored {} market-state value(s) from {} file(s).", response.rows, response.files);
    BOOST_LOG_SEV(market_state_handler_lg(), info)
        << "store_market_state | instance=" << req->report_instance_id
        << " files=" << response.files << " rows=" << response.rows;
    reply(nats_, msg, response);
}

}
