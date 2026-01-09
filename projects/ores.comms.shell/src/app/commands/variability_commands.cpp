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
#include "ores.comms.shell/app/commands/variability_commands.hpp"

#include <ostream>
#include <functional>
#include <cli/cli.h>
#include "ores.variability/messaging/protocol.hpp"
#include "ores.variability/domain/feature_flags_table_io.hpp"  // IWYU pragma: keep.

namespace ores::comms::shell::app::commands {

using namespace ores::logging;
using comms::messaging::message_type;
using comms::net::client_session;

void variability_commands::
register_commands(cli::Menu& root_menu, client_session& session) {
    auto variability_menu =
        std::make_unique<cli::Menu>("variability");

    variability_menu->Insert("list-flags", [&session](std::ostream& out) {
        process_list_feature_flags(std::ref(out), std::ref(session));
    }, "Retrieve all feature flags from the server");

    root_menu.Insert(std::move(variability_menu));
}

void variability_commands::
process_list_feature_flags(std::ostream& out, client_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list feature flags request.";

    using variability::messaging::list_feature_flags_request;
    auto result = session.process_request(list_feature_flags_request{});

    if (!result) {
        out << "✗ " << to_string(result.error()) << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->feature_flags.size() << " feature flags.";
    out << result->feature_flags << std::endl;
}

}
