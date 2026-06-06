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
#include "ores.shell/app/commands/reports_commands.hpp"
#include "ores.dq.api/messaging/report_definition_template_protocol.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include <cli/cli.h>
#include <functional>
#include <iomanip>
#include <ostream>
#include <rfl/json.hpp>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

void reports_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto reports_menu = std::make_unique<cli::Menu>("reports");

    reports_menu->Insert(
        "templates",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_templates(std::ref(out), std::ref(session), args);
        },
        "List the report definition templates of a bundle",
        {"[--bundle <code>]"});

    root_menu.Insert(std::move(reports_menu));
}

void reports_commands::process_templates(std::ostream& out,
                                         nats_client& session,
                                         const std::vector<std::string>& args) {
    dq::messaging::list_dq_report_definition_templates_request req;
    auto parsed = parse_args(args, {
        {.name = "bundle", .requires_value = true, .default_value = req.bundle_code}
    });
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (!parsed->positionals.empty()) {
        fail(out) << "Usage: reports templates [--bundle <code>]" << std::endl;
        return;
    }

    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    req.bundle_code = parsed->flag("bundle");
    BOOST_LOG_SEV(lg(), debug) << "Listing report templates for bundle: "
                               << req.bundle_code;

    try {
        auto reply = session.authenticated_request(std::string(req.nats_subject),
                                                   rfl::json::write(req));
        std::string data_str(reply.data.begin(), reply.data.end());
        auto result =
            rfl::json::read<dq::messaging::list_dq_report_definition_templates_response>(
                data_str);
        if (!result) {
            fail(out) << "Failed to parse response" << std::endl;
            return;
        }
        if (!result->success) {
            fail(out) << "Failed to list report templates: " << result->message
                      << std::endl;
            return;
        }

        for (const auto& t : result->templates) {
            out << std::left << std::setw(34) << t.name << std::setw(16) << t.report_type
                << std::setw(20) << t.schedule_expression << t.description << std::endl;
        }
        out << result->templates.size() << " template"
            << (result->templates.size() == 1 ? "" : "s") << " in bundle '"
            << req.bundle_code << "'." << std::endl;
        BOOST_LOG_SEV(lg(), info) << "Listed " << result->templates.size()
                                  << " report templates.";
    } catch (const std::exception& e) {
        fail(out) << "Request failed: " << e.what() << std::endl;
    }
}

}
