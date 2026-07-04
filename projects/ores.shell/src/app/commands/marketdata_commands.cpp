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
#include "ores.marketdata.api/messaging/import_protocol.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <chrono>
#include <cli/cli.h>
#include <fstream>
#include <functional>
#include <optional>
#include <ostream>
#include <rfl/json.hpp>
#include <sstream>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

// Imports can be large market.txt/fixings.txt files; mirror the
// bundles publish command's generous request timeout.
constexpr std::chrono::minutes import_timeout(5);

template <typename Response>
std::optional<Response>
do_auth_request(std::ostream& out,
                nats_client& session,
                const std::string& subject,
                const std::string& body,
                std::chrono::milliseconds timeout = std::chrono::seconds(30)) {
    try {
        auto reply = session.authenticated_request(subject, body, timeout);
        auto result = rfl::json::read<Response>(ores::nats::as_string_view(reply.data));
        if (!result) {
            fail(out) << "Failed to parse response: " << result.error().what() << std::endl;
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        fail(out) << "Request failed: " << e.what() << std::endl;
        return std::nullopt;
    }
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
    auto marketdata_menu = std::make_unique<cli::Menu>("marketdata");

    marketdata_menu->Insert(
        "import",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_import(std::ref(out), std::ref(session), args);
        },
        "Import ORE market.txt/fixings.txt content via import_market_data_request",
        {"[--file <path>] [--fixings <path>] [--source <tag>]"});

    root_menu.Insert(std::move(marketdata_menu));
}

void marketdata_commands::process_import(std::ostream& out,
                                         nats_client& session,
                                         const std::vector<std::string>& args) {
    auto parsed = parse_args(args,
                             {{.name = "file", .requires_value = true, .default_value = ""},
                              {.name = "fixings", .requires_value = true, .default_value = ""},
                              {.name = "source", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    const auto& file_path = parsed->flag("file");
    const auto& fixings_path = parsed->flag("fixings");
    if (file_path.empty() && fixings_path.empty()) {
        fail(out) << "Usage: marketdata import [--file <path>] [--fixings <path>] "
                     "[--source <tag>] (at least one of --file/--fixings is required)"
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

    BOOST_LOG_SEV(lg(), info) << "Importing market data (file: " << file_path
                              << ", fixings: " << fixings_path << ", source: " << req.source
                              << ")";
    out << "Importing market data..." << std::endl;

    auto result = do_auth_request<marketdata::messaging::import_market_data_response>(
        out, session, std::string(req.nats_subject), rfl::json::write(req), import_timeout);
    if (!result)
        return;

    if (!result->success) {
        fail(out) << "Failed to import market data: " << result->message << std::endl;
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

}
