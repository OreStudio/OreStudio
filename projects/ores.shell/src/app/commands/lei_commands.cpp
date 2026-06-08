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
#include "ores.shell/app/commands/lei_commands.hpp"
#include "ores.dq.api/messaging/lei_entity_summary_protocol.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include <algorithm>
#include <cli/cli.h>
#include <functional>
#include <iomanip>
#include <ostream>
#include <rfl/json.hpp>
#include <set>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

std::optional<dq::messaging::get_lei_entities_summary_response>
fetch_entities(std::ostream& out, nats_client& session, const std::string& country_filter) {
    dq::messaging::get_lei_entities_summary_request req;
    req.country_filter = country_filter;

    try {
        auto reply =
            session.authenticated_request(std::string(req.nats_subject), rfl::json::write(req));
        auto result = rfl::json::read<dq::messaging::get_lei_entities_summary_response>(
            ores::nats::as_string_view(reply.data));
        if (!result) {
            fail(out) << "Failed to parse response: " << result.error().what() << std::endl;
            return std::nullopt;
        }
        if (!result->success) {
            fail(out) << "Failed to fetch LEI entities: " << result->error_message << std::endl;
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        fail(out) << "Request failed: " << e.what() << std::endl;
        return std::nullopt;
    }
}

std::string to_lower_copy(std::string s) {
    std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c) { return std::tolower(c); });
    return s;
}

}

void lei_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto lei_menu = std::make_unique<cli::Menu>("lei");

    lei_menu->Insert(
        "countries",
        [&session](std::ostream& out) { process_countries(std::ref(out), std::ref(session)); },
        "List the countries that have LEI entities");

    lei_menu->Insert("entities",
                     [&session](std::ostream& out, std::vector<std::string> args) {
                         process_entities(std::ref(out), std::ref(session), args);
                     },
                     "List a country's LEI entities, optionally filtered by legal name",
                     {"country [--filter <text>]"});

    root_menu.Insert(std::move(lei_menu));
}

void lei_commands::process_countries(std::ostream& out, nats_client& session) {
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Fetching LEI entity countries.";

    auto result = fetch_entities(out, session, "");
    if (!result)
        return;

    std::set<std::string> countries;
    for (const auto& entity : result->entities)
        countries.insert(entity.country);

    BOOST_LOG_SEV(lg(), info) << "Found " << countries.size() << " countries.";
    for (const auto& country : countries)
        out << country << std::endl;
    out << countries.size() << " countr" << (countries.size() == 1 ? "y" : "ies")
        << " with LEI entities." << std::endl;
}

void lei_commands::process_entities(std::ostream& out,
                                    nats_client& session,
                                    const std::vector<std::string>& args) {
    auto parsed =
        parse_args(args, {{.name = "filter", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 1) {
        fail(out) << "Usage: lei entities <country> [--filter <text>]" << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto& country = parsed->positionals.front();
    const auto filter = to_lower_copy(parsed->flag("filter"));

    BOOST_LOG_SEV(lg(), debug) << "Fetching LEI entities for country: " << country << " (filter: '"
                               << filter << "')";

    auto result = fetch_entities(out, session, country);
    if (!result)
        return;

    std::size_t shown = 0;
    for (const auto& entity : result->entities) {
        if (!filter.empty() &&
            to_lower_copy(entity.entity_legal_name).find(filter) == std::string::npos)
            continue;
        out << std::left << std::setw(22) << entity.lei << std::setw(8) << entity.country
            << std::setw(18) << entity.entity_category << entity.entity_legal_name << std::endl;
        ++shown;
    }
    out << shown << " of " << result->entities.size() << " entit"
        << (result->entities.size() == 1 ? "y" : "ies") << " shown." << std::endl;
}

}
