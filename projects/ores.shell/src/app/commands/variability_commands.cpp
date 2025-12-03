/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.shell/app/commands/variability_commands.hpp"

#include <ostream>
#include <functional>
#include <cli/cli.h>
#include "ores.variability/messaging/protocol.hpp"
#include "ores.variability/domain/feature_flags_table_io.hpp"  // IWYU pragma: keep.

namespace ores::shell::app::commands {

using namespace ores::utility::log;
using comms::protocol::message_type;

void variability_commands::
register_commands(cli::Menu& root_menu, client_manager& client_manager) {
    auto variability_menu =
        std::make_unique<cli::Menu>("variability");

    variability_menu->Insert("list-flags", [&client_manager](std::ostream& out) {
        process_list_feature_flags(std::ref(out), std::ref(client_manager));
    }, "Retrieve all feature flags from the server");

    root_menu.Insert(std::move(variability_menu));
}

void variability_commands::
process_list_feature_flags(std::ostream& out, client_manager& client_manager) {
    try {
        BOOST_LOG_SEV(lg(), debug) << "Initiating list feature flags request.";

        using variability::messaging::list_feature_flags_request;
        using variability::messaging::list_feature_flags_response;
        client_manager.process_request<list_feature_flags_request,
                                       list_feature_flags_response,
                                       message_type::list_feature_flags_request>
            (list_feature_flags_request{})
            .and_then([&](const auto& response) {
                BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                                          << response.feature_flags.size() << " feature flags.";

                out << response.feature_flags << std::endl;
                return std::optional{response};
            });
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "List feature flags exception: " << e.what();
        out << "✗ Error: " << e.what() << std::endl;
    }
}

}
