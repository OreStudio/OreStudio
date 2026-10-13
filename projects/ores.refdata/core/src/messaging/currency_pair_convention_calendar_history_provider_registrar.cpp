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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_history_provider_registrar.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.refdata.core/messaging/currency_pair_convention_calendar_history_provider_registrar.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.history.api/messaging/history_protocol.hpp"
#include "ores.history.api/service/version_builder.hpp"
#include "ores.history.core/service/dispatch_registry.hpp"
#include "ores.refdata.core/presentation/currency_pair_convention_calendar_history_field_mapper.hpp"
#include "ores.refdata.core/repository/currency_pair_convention_calendar_repository.hpp"
#include <algorithm>
#include <iterator>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

void register_currency_pair_convention_calendar_history_provider(
    ores::history::service::dispatch_registry& registry) {
    registry.register_history_provider(
        "ores.refdata.currency_pair_convention_calendar",
        "refdata::currency_pair_convention_calendars:read",
        [](const ores::database::context& scoped_ctx, const std::string& entity_id) {
            repository::currency_pair_convention_calendar_repository repo(scoped_ctx);
            const auto rows = repo.read_all_by_pair(entity_id);

            std::vector<ores::history::messaging::entity_history_version> versions;
            for (auto first = rows.begin(); first != rows.end();) {
                const auto last = std::find_if(first, rows.end(), [&](const auto& row) {
                    return row.link.calendar_code != first->link.calendar_code;
                });
                const std::vector<repository::currency_pair_convention_calendar_history_row> link(
                    first, last);
                auto own = ores::history::service::build_entity_history_versions(
                    link, presentation::render_currency_pair_convention_calendar_fields);
                versions.insert(versions.end(),
                                std::make_move_iterator(own.begin()),
                                std::make_move_iterator(own.end()));
                first = last;
            }
            return versions;
        });
}

}
