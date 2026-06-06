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
#include "ores.shell/app/commands/parties_commands.hpp"
#include "ores.refdata.api/domain/party_table_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <algorithm>
#include <cli/cli.h>
#include <functional>
#include <ostream>
#include <rfl/json.hpp>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

// The wizards list parties with a generous page; mirror that.
constexpr int list_limit = 1000;

std::string to_lower_copy(std::string s) {
    std::transform(s.begin(), s.end(), s.begin(),
                   [](unsigned char c) { return std::tolower(c); });
    return s;
}

bool matches(const std::string& value, const std::string& filter_lower) {
    return filter_lower.empty() || to_lower_copy(value) == filter_lower;
}

}

void parties_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto parties_menu = std::make_unique<cli::Menu>("parties");

    parties_menu->Insert(
        "list",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_list(std::ref(out), std::ref(session), args);
        },
        "List parties, optionally filtered by category and status",
        {"[--category <c>] [--status <s>]"});

    root_menu.Insert(std::move(parties_menu));
}

void parties_commands::process_list(std::ostream& out,
                                    nats_client& session,
                                    const std::vector<std::string>& args) {
    auto parsed = parse_args(args, {
        {.name = "category", .requires_value = true, .default_value = ""},
        {.name = "status", .requires_value = true, .default_value = ""}
    });
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (!parsed->positionals.empty()) {
        fail(out) << "Usage: parties list [--category <c>] [--status <s>]" << std::endl;
        return;
    }

    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto category = to_lower_copy(parsed->flag("category"));
    const auto status = to_lower_copy(parsed->flag("status"));

    BOOST_LOG_SEV(lg(), debug) << "Listing parties (category: '" << category
                               << "', status: '" << status << "').";

    refdata::messaging::get_parties_request req;
    req.limit = list_limit;

    try {
        auto reply = session.authenticated_request(std::string(req.nats_subject),
                                                   rfl::json::write(req));
        auto result = rfl::json::read<refdata::messaging::get_parties_response>(
                ores::nats::as_string_view(reply.data));
        if (!result) {
            fail(out) << "Failed to parse response: " << result.error().what()
                      << std::endl;
            return;
        }

        std::vector<refdata::domain::party> filtered;
        for (const auto& party : result->parties) {
            if (matches(party.party_category, category) && matches(party.status, status))
                filtered.push_back(party);
        }

        BOOST_LOG_SEV(lg(), info) << "Retrieved " << result->parties.size()
                                  << " parties; showing " << filtered.size() << ".";
        out << filtered << std::endl;
        out << filtered.size() << " of " << result->total_available_count
            << " parties shown." << std::endl;
    } catch (const std::exception& e) {
        fail(out) << "Request failed: " << e.what() << std::endl;
    }
}

}
