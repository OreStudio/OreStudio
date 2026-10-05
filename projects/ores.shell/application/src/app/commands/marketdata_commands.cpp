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
#include "ores.shell/app/commands/marketdata_commands.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/marketdata/feed_binding_commands.hpp"
#include "ores.shell/app/commands/marketdata/market_fixing_commands.hpp"
#include "ores.shell/app/commands/marketdata/market_observation_commands.hpp"
#include "ores.shell/app/commands/marketdata/market_series_asset_class_commands.hpp"
#include "ores.shell/app/commands/marketdata/market_series_commands.hpp"
#include "ores.shell/app/commands/marketdata/observation_lineage_commands.hpp"
#include "ores.shell/app/commands/marketdata/series_classification_rule_commands.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <chrono>
#include <cli/cli.h>
#include <fstream>
#include <functional>
#include <optional>
#include <ostream>
#include <sstream>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

// Import and export both carry a whole market.txt/fixings.txt file's worth of
// content in one request; mirror the bundles publish command's generous
// request timeout.
constexpr std::chrono::minutes bulk_transfer_timeout(5);

bool write_file(const std::string& path, const std::string& content) {
    std::ofstream file(path);
    if (!file.is_open())
        return false;
    file << content;
    return file.good();
}

std::optional<std::string> read_file(const std::string& path) {
    std::ifstream file(path);
    if (!file.is_open())
        return std::nullopt;

    std::ostringstream contents;
    contents << file.rdbuf();
    return contents.str();
}

}

void marketdata_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    // Generated per-entity command units. Each projects one market-data
    // entity onto the REPL as a top-level menu of its own, so the commands a
    // user can type cannot drift from the operations the service offers.
    feed_binding_commands::register_commands(root_menu, session);
    market_fixing_commands::register_commands(root_menu, session);
    market_observation_commands::register_commands(root_menu, session);
    market_series_commands::register_commands(root_menu, session);
    market_series_asset_class_commands::register_commands(root_menu, session);
    observation_lineage_commands::register_commands(root_menu, session);
    series_classification_rule_commands::register_commands(root_menu, session);

    // Import has no modelled operation family yet, so it stays hand-written.
    auto marketdata_menu = std::make_unique<cli::Menu>("marketdata");

    marketdata_menu->Insert(
        "import",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_import(std::ref(out), std::ref(session), args);
        },
        "Import ORE market.txt/fixings.txt content via import_market_data_request",
        {"[--file <path>] [--fixings <path>] [--source <tag>] [--duplicates-are-errors]"});

    marketdata_menu->Insert(
        "export",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_export(std::ref(out), std::ref(session), args);
        },
        "Write the tenant's market data back out as ORE market.txt/fixings.txt files",
        {"[--market-data <path>] [--fixings <path>]"});

    ores::shell::app::insert_menu(root_menu, std::move(marketdata_menu));
}

void marketdata_commands::process_import(std::ostream& out,
                                         nats_client& session,
                                         const std::vector<std::string>& args) {
    auto parsed = parse_args(args,
                             {{.name = "file", .requires_value = true, .default_value = ""},
                              {.name = "fixings", .requires_value = true, .default_value = ""},
                              {.name = "source", .requires_value = true, .default_value = ""},
                              {.name = "duplicates-are-errors"}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    const auto& file_path = parsed->flag("file");
    const auto& fixings_path = parsed->flag("fixings");
    if (file_path.empty() && fixings_path.empty()) {
        fail(out) << "Usage: marketdata import [--file <path>] [--fixings <path>] "
                     "[--source <tag>] [--duplicates-are-errors] "
                     "(at least one of --file/--fixings is required)"
                  << std::endl;
        return;
    }

    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    marketdata::messaging::import_market_data_request req;
    if (!file_path.empty()) {
        auto content = read_file(file_path);
        if (!content) {
            fail(out) << "Cannot open file: " << file_path << std::endl;
            return;
        }
        req.market_data_content = std::move(*content);
    }
    if (!fixings_path.empty()) {
        auto content = read_file(fixings_path);
        if (!content) {
            fail(out) << "Cannot open file: " << fixings_path << std::endl;
            return;
        }
        req.fixings_content = std::move(*content);
    }
    req.source = parsed->flag("source");
    req.duplicates_are_errors = parsed->flag_set("duplicates-are-errors");

    BOOST_LOG_SEV(lg(), info) << "Importing market data (file: " << file_path
                              << ", fixings: " << fixings_path << ", source: " << req.source << ")";
    out << "Importing market data..." << std::endl;

    auto result = do_auth_request<marketdata::messaging::import_market_data_response>(
        out, session, std::string(req.nats_subject), req, bulk_transfer_timeout);
    if (!result)
        return;

    if (!result->success) {
        // Sections without duplicate errors are still persisted (see
        // import_service::import) — report their counts too, so a partial
        // import doesn't read as "nothing happened".
        fail(out) << "Failed to import market data: " << result->message << std::endl;
        out << "  (series: " << result->series_count
            << ", observations: " << result->observation_count
            << ", fixings: " << result->fixing_count
            << " — counts reflect content that had no duplicate errors)" << std::endl;
        for (const auto& error : result->errors)
            out << "  ✗ " << error << std::endl;
        return;
    }

    out << "✓ Imported " << result->series_count << " series, " << result->observation_count
        << " observation(s), " << result->fixing_count << " fixing(s): " << result->message
        << std::endl;
    if (!result->warnings.empty()) {
        out << "⚠ " << result->warnings.size() << " warning(s):" << std::endl;
        for (const auto& warning : result->warnings)
            out << "  ⚠ " << warning << std::endl;
    }
    BOOST_LOG_SEV(lg(), info) << "Import succeeded: " << result->series_count << " series, "
                              << result->observation_count << " observations, "
                              << result->fixing_count << " fixings, " << result->warnings.size()
                              << " warning(s).";
}

void marketdata_commands::process_export(std::ostream& out,
                                         nats_client& session,
                                         const std::vector<std::string>& args) {
    auto parsed =
        parse_args(args,
                   {{.name = "market-data", .requires_value = true, .default_value = "market.txt"},
                    {.name = "fixings", .requires_value = true, .default_value = "fixings.txt"}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto& market_path = parsed->flag("market-data");
    const auto& fixings_path = parsed->flag("fixings");

    BOOST_LOG_SEV(lg(), info) << "Exporting market data (market-data: " << market_path
                              << ", fixings: " << fixings_path << ")";
    out << "Exporting market data..." << std::endl;

    marketdata::messaging::export_market_data_request req;
    auto result = do_auth_request<marketdata::messaging::export_market_data_response>(
        out, session, std::string(req.nats_subject), req, bulk_transfer_timeout);
    if (!result)
        return;

    if (!result->success) {
        fail(out) << "Failed to export market data: " << result->message << std::endl;
        return;
    }

    // Both files are written even when one is empty, because ORE reads both and
    // an absent fixings.txt fails the run -- which is exactly the state the
    // TA002 example ships in, with a fixings.txt of zero lines.
    if (!write_file(market_path, result->market_data_content)) {
        fail(out) << "Cannot write file: " << market_path << std::endl;
        return;
    }
    if (!write_file(fixings_path, result->fixings_content)) {
        fail(out) << "Cannot write file: " << fixings_path << std::endl;
        return;
    }

    out << "✓ Exported " << result->series_count << " series, " << result->observation_count
        << " observation(s), " << result->fixing_count << " fixing(s) to " << market_path << " and "
        << fixings_path << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Export succeeded: " << result->series_count << " series, "
                              << result->observation_count << " observations, "
                              << result->fixing_count << " fixings.";
}

}
