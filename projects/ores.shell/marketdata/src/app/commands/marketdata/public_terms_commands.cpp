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
#include "ores.shell/app/commands/marketdata/public_terms_commands.hpp"
#include "ores.marketdata.api/messaging/provider_protocol.hpp"
#include "ores.marketdata.api/messaging/provider_terms_list_operations_protocol.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include <cli/cli.h>
#include <string>
#include <vector>

namespace ores::shell::app::commands {

using ores::nats::service::nats_client;

void public_terms_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    ores::shell::app::extend_menu(
        root_menu, "provider_terms_operations", [&session](cli::Menu& menu) {
            menu.Insert(
                "accept-all",
                [&session](std::ostream& out, std::vector<std::string>) {
                    accept_all(out, session);
                },
                "Accept the terms of every provider that states them, as they stand, "
                "without showing their text");
        });
}

bool public_terms_commands::accept_all(std::ostream& out, nats_client& session) {
    using namespace ores::marketdata::messaging;
    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to accept the terms." << std::endl;
        return false;
    }

    accept_many_provider_terms_request accept;
    list_providers_request list;
    for (;;) {
        const auto page =
            do_auth_request<list_providers_response>(out, session, list.nats_subject, list);
        if (!page)
            return false;
        for (const auto& provider : page->providers)
            if (!provider.terms_url.empty())
                accept.providers.push_back({.provider_code = provider.code, .provider_version = 0});
        if (page->providers.size() < list.limit)
            break;
        list.offset += list.limit;
    }
    if (accept.providers.empty()) {
        out << "  No provider states terms." << std::endl;
        return true;
    }

    const auto result = do_auth_request<accept_many_provider_terms_response>(
        out, session, accept.nats_subject, accept);
    if (!result)
        return false;
    for (const auto& outcome : result->outcomes)
        out << "  " << (outcome.success ? "accepted " : "refused  ") << outcome.provider_code
            << (outcome.success ? std::string() : ": " + outcome.message) << std::endl;
    out << "  " << result->message << std::endl;
    return result->success;
}

}
