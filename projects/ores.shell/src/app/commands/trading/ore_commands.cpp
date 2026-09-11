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
#include "ores.shell/app/commands/trading/ore_commands.hpp"
#include "ores.ore.api/messaging/ore_import_protocol.hpp"
#include "ores.ore.api/net/ore_storage.hpp"
#include "ores.ore.core/planner/import_choices.hpp"
#include "ores.ore.core/xml/exporter.hpp"
#include "ores.platform/environment/environment.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/workflow_commands.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.storage/net/storage_transfer.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cli/cli.h>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <ostream>
#include <string>
#include <system_error>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;
using ores::platform::environment::environment;

namespace {

constexpr std::chrono::seconds default_import_timeout(600);
constexpr std::chrono::seconds import_request_timeout(60);
constexpr std::chrono::seconds export_request_timeout(300);
constexpr std::uint32_t default_export_limit = 10000;

std::string default_http_base_url() {
    return "http://localhost:" + environment::get_value_or_default("ORES_HTTP_PORT", "20600");
}

std::optional<boost::uuids::uuid> parse_uuid(std::ostream& out,
                                             const std::string& value,
                                             std::string_view what) {
    try {
        return boost::uuids::string_generator()(value);
    } catch (const std::exception&) {
        fail(out) << "Invalid " << what << ": " << value << std::endl;
        return std::nullopt;
    }
}

}

void ore_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto ore_menu = std::make_unique<cli::Menu>("ore");

    ore_menu->Insert(
        "upload",
        [](std::ostream& out, std::vector<std::string> args) {
            auto parsed = parse_args(args, {{.name = "request-id", .requires_value = true}});
            if (!parsed) {
                fail(out) << parsed.error() << std::endl;
                return;
            }
            if (parsed->positionals.size() != 1) {
                fail(out) << "Usage: ore upload <src_dir> [--request-id <uuid>]" << std::endl;
                return;
            }
            process_upload(std::ref(out), parsed->positionals.front(), parsed->flag("request-id"));
        },
        "Pack a directory of ORE documents and upload it to the ore-imports bucket",
        {"src_dir [--request-id <uuid>]"});

    ore_menu->Insert(
        "import",
        [&session](std::ostream& out, std::vector<std::string> args) {
            auto parsed = parse_args(args,
                                     {{.name = "party-id", .requires_value = true},
                                      {.name = "choices-file", .requires_value = true},
                                      {.name = "timeout",
                                       .requires_value = true,
                                       .default_value = std::to_string(default_import_timeout.count())}});
            if (!parsed) {
                fail(out) << parsed.error() << std::endl;
                return;
            }
            if (parsed->positionals.size() != 1) {
                fail(out) << "Usage: ore import <request_id> [--party-id <uuid>] "
                             "[--choices-file <path>] [--timeout <seconds>]"
                          << std::endl;
                return;
            }

            auto timeout = parse_positive_seconds(parsed->flag("timeout"));
            if (!timeout) {
                fail(out) << "Timeout must be a positive number of seconds: "
                          << parsed->flag("timeout") << std::endl;
                return;
            }

            process_import(std::ref(out),
                           std::ref(session),
                           parsed->positionals.front(),
                           parsed->flag("party-id"),
                           parsed->flag("choices-file"),
                           *timeout);
        },
        "Import an uploaded ORE tarball and wait for the workflow to finish",
        {"request_id [--party-id <uuid>] [--choices-file <path>] [--timeout <seconds>]"});

    ore_menu->Insert(
        "export",
        [&session](std::ostream& out, std::vector<std::string> args) {
            auto parsed = parse_args(
                args,
                {{.name = "node-id", .requires_value = true},
                 {.name = "limit",
                  .requires_value = true,
                  .default_value = std::to_string(default_export_limit)}});
            if (!parsed) {
                fail(out) << parsed.error() << std::endl;
                return;
            }
            if (parsed->positionals.size() != 1) {
                fail(out) << "Usage: ore export <output_file> [--node-id <uuid>] [--limit <n>]"
                          << std::endl;
                return;
            }

            auto limit = parse_uint32(parsed->flag("limit"));
            if (!limit) {
                fail(out) << "Flag --limit must be an unsigned integer: " << parsed->flag("limit")
                          << std::endl;
                return;
            }

            process_export(std::ref(out),
                           std::ref(session),
                           parsed->positionals.front(),
                           parsed->flag("node-id"),
                           *limit);
        },
        "Write a portfolio's trades back out as ORE portfolio XML",
        {"output_file [--node-id <uuid>] [--limit <n>]"});

    root_menu.Insert(std::move(ore_menu));
}

void ore_commands::process_upload(std::ostream& out,
                                  const std::string& src_dir,
                                  const std::string& request_id) {
    const std::filesystem::path source(src_dir);
    std::error_code ec;
    if (!std::filesystem::is_directory(source, ec)) {
        fail(out) << "Not a directory: " << src_dir << std::endl;
        return;
    }

    const auto effective_id =
        request_id.empty() ? boost::uuids::to_string(boost::uuids::random_generator()()) : request_id;
    const auto bucket = std::string(ores::ore::net::ore_storage::bucket);
    const auto key = ores::ore::net::ore_storage::import_key(effective_id);

    BOOST_LOG_SEV(lg(), info) << "Uploading " << src_dir << " to " << bucket << "/" << key;

    try {
        ores::storage::net::storage_transfer transfer(default_http_base_url());
        transfer.pack_and_upload(source, bucket, key);
    } catch (const std::exception& e) {
        fail(out) << "Upload failed: " << e.what() << std::endl;
        return;
    }

    out << "✓ Uploaded " << src_dir << " to " << bucket << "/" << key << std::endl;
    out << "request_id: " << effective_id << std::endl;
}

void ore_commands::process_import(std::ostream& out,
                                  nats_client& session,
                                  const std::string& request_id,
                                  const std::string& party_id,
                                  const std::string& choices_file,
                                  std::chrono::seconds timeout) {
    ores::ore::planner::import_choices choices;

    if (!choices_file.empty()) {
        std::ifstream in(choices_file);
        if (!in) {
            fail(out) << "Cannot read choices file: " << choices_file << std::endl;
            return;
        }
        const std::string json((std::istreambuf_iterator<char>(in)),
                               std::istreambuf_iterator<char>());
        auto parsed = rfl::json::read<ores::ore::planner::import_choices>(json);
        if (!parsed) {
            fail(out) << "Cannot parse choices file: " << parsed.error().what() << std::endl;
            return;
        }
        choices = std::move(*parsed);
    }

    const auto& party_ref = party_id.empty() ? session.auth().default_party_id : party_id;
    if (party_ref.empty()) {
        fail(out) << "No default party on this account and no --party-id given." << std::endl;
        return;
    }
    auto party_uuid = parse_uuid(out, party_ref, "party ID");
    if (!party_uuid)
        return;
    choices.party_id = *party_uuid;

    ores::ore::messaging::ore_import_request req;
    req.request_id = request_id;
    req.correlation_id = request_id;
    req.import_choices_json = rfl::json::write(choices);

    BOOST_LOG_SEV(lg(), info) << "Starting ORE import for request: " << request_id;

    auto result = do_auth_request<ores::ore::messaging::ore_import_response>(
        out, session, req.nats_subject, req, import_request_timeout);
    if (!result)
        return;
    if (!result->success) {
        fail(out) << "Import failed: " << result->message << std::endl;
        return;
    }

    for (const auto& item_error : result->item_errors)
        fail(out) << item_error.source_file << " / " << item_error.item_id << ": "
                  << item_error.message << std::endl;

    if (result->workflow_instance_id.empty()) {
        out << "✓ Import completed." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "ORE import workflow dispatched: "
                              << result->workflow_instance_id;
    out << "workflow_instance_id: " << result->workflow_instance_id << std::endl;

    if (!workflow_commands::wait_for_instance(out, session, result->workflow_instance_id, timeout, 1))
        fail(out) << "ORE import did not complete: see the steps above." << std::endl;
}

void ore_commands::process_export(std::ostream& out,
                                  nats_client& session,
                                  const std::string& output_file,
                                  const std::string& node_id,
                                  std::uint32_t limit) {
    ores::trading::messaging::export_portfolio_request req;
    req.node_id = node_id;
    req.limit = static_cast<int>(limit);

    BOOST_LOG_SEV(lg(), info) << "Requesting portfolio export (node: "
                              << (node_id.empty() ? "all" : node_id) << ")";

    auto result = do_auth_request<ores::trading::messaging::export_portfolio_response>(
        out, session, req.nats_subject, req, export_request_timeout);
    if (!result)
        return;
    if (!result->success) {
        fail(out) << "Export failed: " << result->message << std::endl;
        return;
    }

    const auto xml = ores::ore::xml::exporter::export_portfolio(result->items);

    std::ofstream out_file(output_file, std::ios::binary | std::ios::trunc);
    if (!out_file) {
        fail(out) << "Cannot write: " << output_file << std::endl;
        return;
    }
    out_file << xml;
    out_file.close();
    if (!out_file) {
        fail(out) << "Failed to write: " << output_file << std::endl;
        return;
    }

    out << "✓ Exported " << result->items.size() << " trade(s) to " << output_file << " ("
        << xml.size() << " bytes)." << std::endl;
}

}
