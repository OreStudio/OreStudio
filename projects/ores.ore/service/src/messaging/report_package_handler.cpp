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
#include "ores.ore.service/messaging/report_package_handler.hpp"
#include "ores.iam.client/client/run_token_minter.hpp"
#include "ores.ore.core/store/run_store.hpp"
#include "ores.ore.service/messaging/run_configuration_operations.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.storage.api/net/object_keys.hpp"
#include "ores.storage.api/net/storage_paths.hpp"
#include "ores.storage.core/net/storage_transfer.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <filesystem>
#include <format>
#include <fstream>
#include <rfl/json.hpp>

namespace ores::ore::service::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;
using namespace ores::reporting::messaging;

namespace {

constexpr std::string_view platform_bucket = ores::storage::api::object_keys::ores_bucket;

std::string tarball_storage_key(const std::string& instance_id) {
    return ores::storage::api::object_keys::make(
        "ore", "packages", instance_id, "ore_package.tar.gz");
}

} // namespace

report_package_handler::report_package_handler(ores::nats::service::client& nats,
                                               ores::database::context ctx,
                                               std::string http_base_url,
                                               ores::nats::service::nats_client service_nats)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , http_base_url_(std::move(http_base_url))
    , service_nats_(std::move(service_nats))
    , run_tokens_(ores::iam::client::make_run_token_minter(service_nats_)) {}

void report_package_handler::prepare_package(ores::nats::message msg) {
    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf)
        return;

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<prepare_ore_package_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode prepare_ore_package_request");
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "prepare_ore_package starting | instance="
                              << req.report_instance_id;

    try {
        if (req.definition_id.empty()) {
            wf->fail("prepare_ore_package: definition_id is missing");
            return;
        }
        if (req.trades_storage_key.empty()) {
            wf->fail("prepare_ore_package: trades_storage_key is missing");
            return;
        }
        if (req.market_data_storage_key.empty()) {
            wf->fail("prepare_ore_package: market_data_storage_key is missing");
            return;
        }

        ores::service::service::cache::run_token_step_scope step_tokens(run_tokens_,
                                                                        req.report_instance_id);
        const ores::service::service::cache::run_token_key key{.grant_id = req.run_grant_id,
                                                               .run_id = req.report_instance_id};
        const auto run_token = run_tokens_.token_for(key, req.tenant_id);
        if (run_token.empty()) {
            wf->fail("prepare_ore_package: the run has no run token; its grant is missing or "
                     "IAM refused the exchange");
            return;
        }

        ores::storage::net::storage_transfer transfer(http_base_url_, run_token);

        // ── Create a staging directory ────────────────────────────────
        const auto stage_dir = std::filesystem::temp_directory_path() /
                               boost::uuids::to_string(boost::uuids::random_generator()());
        std::filesystem::create_directories(stage_dir);

        // ── The run's own data ────────────────────────────────────────
        // The engine reaches the run's market data through a file its run
        // document names, so the gathered body goes where that name points and
        // not under a name of the packaging step's own. The run document comes
        // from its owner through the owner's operation, carrying the run token.
        auto owners = service_nats_.with_delegation(run_token);
        const auto run_input =
            ores::ore::service::messaging::export_run(owners, req.definition_id);
        const auto places = ores::ore::store::declared_data_files(run_input);

        BOOST_LOG_SEV(lg(), debug)
            << "Downloading market data: " << req.market_data_storage_key;
        const auto market_data =
            transfer.download_blob(std::string(platform_bucket), req.market_data_storage_key);
        if (places.market_data.empty()) {
            BOOST_LOG_SEV(lg(), warn)
                << "The run document names no market data file, so the gathered market data "
                   "cannot be placed for run "
                << req.report_instance_id;
        } else {
            const auto target = stage_dir / places.market_data;
            std::filesystem::create_directories(target.parent_path());
            std::ofstream f(target, std::ios::binary | std::ios::trunc);
            f.write(market_data.data(), static_cast<std::streamsize>(market_data.size()));
        }

        BOOST_LOG_SEV(lg(), debug) << "Downloading fixings: " << req.fixings_storage_key;
        const auto fixings =
            transfer.download_blob(std::string(platform_bucket), req.fixings_storage_key);
        if (places.fixings.empty()) {
            BOOST_LOG_SEV(lg(), warn)
                << "The run document names no fixings file, so the gathered fixings cannot be "
                   "placed for run "
                << req.report_instance_id;
        } else {
            const auto target = stage_dir / places.fixings;
            std::filesystem::create_directories(target.parent_path());
            std::ofstream f(target, std::ios::binary | std::ios::trunc);
            f.write(fixings.data(), static_cast<std::streamsize>(fixings.size()));
        }

        BOOST_LOG_SEV(lg(), debug) << "Downloading trades: " << req.trades_storage_key;
        const auto trades =
            transfer.download_blob(std::string(platform_bucket), req.trades_storage_key);
        {
            std::ofstream f(stage_dir / "trades.msgpack", std::ios::binary | std::ios::trunc);
            f.write(trades.data(), static_cast<std::streamsize>(trades.size()));
        }

        // ── Pack into a tar.gz and upload ─────────────────────────────
        // The run document and the configuration it names, laid out where the
        // engine reads them. The documents come from their owners through the
        // owners' operations, carrying the run token, so ore never reads their
        // tables.
        const auto input = ores::ore::store::archive_layout(run_input);
        for (const auto& [path, content] : input) {
            const auto target = stage_dir / path;
            std::filesystem::create_directories(target.parent_path());
            std::ofstream f(target, std::ios::binary | std::ios::trunc);
            f << content;
        }

        const auto tarball_key = tarball_storage_key(req.report_instance_id);
        transfer.pack_and_upload(stage_dir, std::string(platform_bucket), tarball_key);

        // ── Clean up staging directory ────────────────────────────────
        std::filesystem::remove_all(stage_dir);

        // The downstream compute step hands this URI to a worker, which fetches
        // it over the storage HTTP interface, so it has to be a path and not
        // the bucket-and-key the store speaks. The compute dispatcher builds the
        // package and output URIs the same way.
        const auto tarball_uri =
            ores::storage::net::storage_paths::make_object_path(platform_bucket, tarball_key);

        prepare_ore_package_result result;
        result.success = true;
        result.tarball_uris = {tarball_uri};
        result.message =
            std::format("Packaged {} input files, {} bytes trades + {} bytes market data into {}",
                        input.size(),
                        trades.size(),
                        market_data.size(),
                        tarball_key);

        BOOST_LOG_SEV(lg(), info) << "prepare_ore_package complete | instance="
                                  << req.report_instance_id << " tarball=" << tarball_key;

        wf->complete(rfl::json::write(result));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "prepare_ore_package failed: " << e.what();
        wf->fail(e.what());
    }
}

}
