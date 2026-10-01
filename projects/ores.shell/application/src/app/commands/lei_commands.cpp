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
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include <cli/cli.h>
#include <functional>
#include <ostream>
#include <set>
#include <string>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

std::optional<dq::messaging::get_lei_entities_summary_response>
fetch_entities(std::ostream& out, nats_client& session, const std::string& country_filter) {
    dq::messaging::get_lei_entities_summary_request req;
    req.country_filter = country_filter;

    try {
        auto result = do_auth_request<dq::messaging::get_lei_entities_summary_response>(
            out, session, std::string(req.nats_subject), req);
        if (!result)
            return std::nullopt;
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

}

void lei_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto lei_menu = std::make_unique<cli::Menu>("lei");

    // The single country browser. The two reads it used to carry are generated
    // units of their own now, under the lei_entity_summary menu; the country
    // list is a grouping of one response that no model declares.
    lei_menu->Insert(
        "countries",
        [&session](std::ostream& out) { process_countries(std::ref(out), std::ref(session)); },
        "List the countries that have LEI entities");

    ores::shell::app::insert_menu(root_menu, std::move(lei_menu));
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

}
