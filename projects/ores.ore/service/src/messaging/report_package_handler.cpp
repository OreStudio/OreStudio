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
#include "ores.ore.api/net/ore_storage.hpp"
#include "ores.ore.core/store/run_store.hpp"
#include "ores.ore.core/xml/exporter.hpp"
#include "ores.ore.service/messaging/run_configuration_operations.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.storage.api/net/object_keys.hpp"
#include "ores.storage.api/net/storage_paths.hpp"
#include "ores.storage.core/net/storage_transfer.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <filesystem>
#include <format>
#include <fstream>
#include <optional>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <string_view>

namespace ores::ore::service::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;
using namespace ores::reporting::messaging;

namespace {

constexpr std::string_view platform_bucket = ores::storage::api::object_keys::ores_bucket;

std::string tarball_storage_key(const std::string& instance_id) {
    return ores::ore::net::ore_storage::package_key(instance_id);
}

/**
 * @brief The latest date a market data body carries, as an ISO 8601 date.
 *
 * ORE's market data lines begin with the date written as YYYYMMDD, so a body
 * with no such line carries no date at all and no run can price from it.
 */
std::optional<std::string> latest_market_data_date(std::string_view body) {
    std::string latest;
    std::size_t first = 0;
    while (first < body.size()) {
        const auto end = body.find('\n', first);
        const auto line = body.substr(
            first, end == std::string_view::npos ? std::string_view::npos : end - first);
        if (line.size() >= 9 && line[8] == '\t') {
            const auto dated = line.substr(0, 8);
            const auto digits =
                std::ranges::all_of(dated, [](char c) { return c >= '0' && c <= '9'; });
            if (digits && std::string(dated) > latest)
                latest = std::string(dated);
        }
        if (end == std::string_view::npos)
            break;
        first = end + 1;
    }
    if (latest.empty())
        return std::nullopt;
    return latest.substr(0, 4) + "-" + latest.substr(4, 2) + "-" + latest.substr(6, 2);
}

/** The date a run document states as its as-of date, when it states one. */
std::optional<std::string> asof_date_of(const std::string& ore_xml) {
    constexpr std::string_view marker = "name=\"asofDate\">";
    const auto at = ore_xml.find(marker);
    if (at == std::string::npos)
        return std::nullopt;
    const auto from = at + marker.size();
    const auto to = ore_xml.find('<', from);
    if (to == std::string::npos || to == from)
        return std::nullopt;
    return ore_xml.substr(from, to - from);
}

/** The run document with its as-of date replaced. */
std::string with_asof_date(const std::string& ore_xml, const std::string& date) {
    constexpr std::string_view marker = "name=\"asofDate\">";
    const auto at = ore_xml.find(marker);
    if (at == std::string::npos)
        return ore_xml;
    const auto from = at + marker.size();
    const auto to = ore_xml.find('<', from);
    if (to == std::string::npos)
        return ore_xml;
    auto out = ore_xml;
    out.replace(from, to - from, date);
    return out;
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
    ores::nats::ack_guard ack(msg);

    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf) {
        ack.reject();
        return;
    }

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<prepare_ore_package_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode prepare_ore_package_request");
        ack.accept();
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "prepare_ore_package starting | instance="
                              << req.report_instance_id;

    try {
        if (req.definition_id.empty()) {
            wf->fail("prepare_ore_package: definition_id is missing");
            ack.accept();
            return;
        }
        if (req.trades_storage_key.empty()) {
            wf->fail("prepare_ore_package: trades_storage_key is missing");
            ack.accept();
            return;
        }
        if (req.market_data_storage_key.empty()) {
            wf->fail("prepare_ore_package: market_data_storage_key is missing");
            ack.accept();
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
            ack.accept();
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
        auto run_input = ores::ore::service::messaging::export_run(owners, req.definition_id);
        const auto places = ores::ore::store::declared_data_files(run_input);

        BOOST_LOG_SEV(lg(), debug) << "Downloading market data: " << req.market_data_storage_key;
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

        // A run document asks for a date, and the engine can only price a date
        // the gathered market data carries. When the data carries no line for
        // the requested date, the run prices at the last date it does carry,
        // and says so, rather than letting the engine refuse every analytic.
        const auto document = std::ranges::find_if(run_input, [](const auto& entry) {
            return entry.first.ends_with(std::string(ores::ore::store::run_document_file));
        });
        const auto requested_date = document != run_input.end() ?
                                        asof_date_of(document->second).value_or("") :
                                        std::string{};
        const auto market_date =
            latest_market_data_date(std::string_view{market_data.data(), market_data.size()});
        if (market_date && !requested_date.empty() && *market_date < requested_date &&
            document != run_input.end()) {
            BOOST_LOG_SEV(lg(), warn)
                << "The gathered market data ends on " << *market_date << " and the run asked for "
                << requested_date << ". The run prices at the data's date. Instance "
                << req.report_instance_id;
            document->second = with_asof_date(document->second, *market_date);
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

        // The gather step leaves the run's trades as the export response, which
        // the engine cannot read. The engine reads a portfolio, so the trades
        // and their instruments are rebuilt into one and written where the run
        // document names it, as the market data and the fixings above are. ORE
        // requires the name, so a run document without one has nowhere to put
        // the trades and is refused rather than packaged into a run that cannot
        // price.
        if (places.portfolio.empty()) {
            wf->fail("prepare_ore_package: the run document names no portfolio file, so the "
                     "gathered trades have nowhere to go for run " +
                     req.report_instance_id);
            ack.accept();
            return;
        }

        BOOST_LOG_SEV(lg(), debug) << "Downloading trades: " << req.trades_storage_key;
        const auto trades =
            transfer.download_blob(std::string(platform_bucket), req.trades_storage_key);

        const auto rows =
            rfl::msgpack::read<ores::trading::messaging::export_portfolio_response>(trades);
        if (!rows) {
            wf->fail("prepare_ore_package: the gathered trades are not a readable trade "
                     "export: " +
                     rows.error().what());
            ack.accept();
            return;
        }
        if (!rows->success) {
            wf->fail("prepare_ore_package: the gather step stored a failed trade export: " +
                     rows->message);
            ack.accept();
            return;
        }

        if (!market_date) {
            std::string named;
            std::size_t shown = 0;
            for (const auto& item : rows->items) {
                if (shown == 5) {
                    named += ", and " + std::to_string(rows->items.size() - shown) + " more";
                    break;
                }
                if (!named.empty())
                    named += ", ";
                named += item.ore_id;
                ++shown;
            }
            wf->fail(
                "prepare_ore_package: the gathered market data carries no dated line, so no run "
                "can price from it. The run asked for " +
                (requested_date.empty() ? std::string("no particular date") : requested_date) +
                ", the package carries " + std::to_string(rows->items.size()) +
                " trade(s): " + named);
            ack.accept();
            return;
        }

        const auto reference_curves =
            ores::ore::service::messaging::reference_curve_names_of(owners, req.definition_id);
        const auto portfolio = ores::ore::xml::exporter::export_portfolio(
            rows->items, rows->instruments, reference_curves);
        {
            const auto target = stage_dir / places.portfolio;
            std::filesystem::create_directories(target.parent_path());
            std::ofstream f(target, std::ios::binary | std::ios::trunc);
            f << portfolio;
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
        result.message = std::format(
            "Packaged {} input files, {} trades as {} portfolio bytes + {} market data bytes "
            "into {}",
            input.size(),
            rows->items.size(),
            portfolio.size(),
            market_data.size(),
            tarball_key);

        BOOST_LOG_SEV(lg(), info) << "prepare_ore_package complete | instance="
                                  << req.report_instance_id << " tarball=" << tarball_key;

        wf->complete(rfl::json::write(result));
        ack.accept();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "prepare_ore_package failed: " << e.what();
        wf->fail(e.what());
        ack.accept();
    }
}

}
