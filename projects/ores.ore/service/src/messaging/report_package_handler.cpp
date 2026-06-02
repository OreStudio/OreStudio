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

#include <fstream>
#include <format>
#include <filesystem>
#include <rfl/json.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.storage/net/storage_transfer.hpp"
#include "ores.reporting.api/messaging/report_execution_protocol.hpp"

namespace ores::ore::service::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;
using namespace ores::reporting::messaging;

namespace {

constexpr std::string_view report_data_bucket = "report-data";

std::string tarball_storage_key(const std::string& instance_id) {
    return instance_id + "/ore_package.tar.gz";
}

} // namespace

report_package_handler::report_package_handler(
    ores::nats::service::client& nats, std::string http_base_url)
    : nats_(nats), http_base_url_(std::move(http_base_url)) {}

void report_package_handler::prepare_package(ores::nats::message msg) {
    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf) return;

    const std::string_view sv(
        reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<prepare_ore_package_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode prepare_ore_package_request");
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "prepare_ore_package starting | instance="
                              << req.report_instance_id;

    try {
        if (req.trades_storage_key.empty()) {
            wf->fail("prepare_ore_package: trades_storage_key is missing");
            return;
        }
        if (req.market_data_storage_key.empty()) {
            wf->fail("prepare_ore_package: market_data_storage_key is missing");
            return;
        }

        ores::storage::net::storage_transfer transfer(http_base_url_);

        // ── Create a staging directory ────────────────────────────────
        const auto stage_dir = std::filesystem::temp_directory_path()
            / boost::uuids::to_string(boost::uuids::random_generator()());
        std::filesystem::create_directories(stage_dir);

        // ── Download trades blob ──────────────────────────────────────
        BOOST_LOG_SEV(lg(), debug)
            << "Downloading trades blob: " << req.trades_storage_key;
        const auto trades_blob = transfer.download_blob(
            std::string(report_data_bucket), req.trades_storage_key);
        {
            std::ofstream f(stage_dir / "trades.msgpack",
                std::ios::binary | std::ios::trunc);
            f.write(trades_blob.data(), static_cast<std::streamsize>(trades_blob.size()));
        }

        // ── Download market data blob ─────────────────────────────────
        BOOST_LOG_SEV(lg(), debug)
            << "Downloading market data blob: " << req.market_data_storage_key;
        const auto md_blob = transfer.download_blob(
            std::string(report_data_bucket), req.market_data_storage_key);
        {
            std::ofstream f(stage_dir / "market_data.msgpack",
                std::ios::binary | std::ios::trunc);
            f.write(md_blob.data(), static_cast<std::streamsize>(md_blob.size()));
        }

        // ── Pack into a tar.gz and upload ─────────────────────────────
        const auto tarball_key = tarball_storage_key(req.report_instance_id);
        transfer.pack_and_upload(stage_dir,
            std::string(report_data_bucket), tarball_key);

        // ── Clean up staging directory ────────────────────────────────
        std::filesystem::remove_all(stage_dir);

        const auto tarball_uri = std::string(report_data_bucket) + "/" + tarball_key;

        prepare_ore_package_result result;
        result.success = true;
        result.tarball_uris = {tarball_uri};
        result.message = std::format(
            "Packaged {} bytes trades + {} bytes market data into {}",
            trades_blob.size(), md_blob.size(), tarball_key);

        BOOST_LOG_SEV(lg(), info) << "prepare_ore_package complete | instance="
                                  << req.report_instance_id
                                  << " tarball=" << tarball_key;

        wf->complete(rfl::json::write(result));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "prepare_ore_package failed: " << e.what();
        wf->fail(e.what());
    }
}

}
