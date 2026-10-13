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
#include "ores.iam.core/messaging/role_grant_request_role_history_provider_registrar.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.history.api/messaging/history_protocol.hpp"
#include "ores.history.api/service/version_builder.hpp"
#include "ores.history.core/service/dispatch_registry.hpp"
#include "ores.iam.core/presentation/role_grant_request_role_history_field_mapper.hpp"
#include "ores.iam.core/repository/role_grant_request_role_repository.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <iterator>
#include <string>
#include <vector>

namespace ores::iam::messaging {

void register_role_grant_request_role_history_provider(
    ores::history::service::dispatch_registry& registry) {
    registry.register_history_provider(
        "ores.iam.role_grant_request_role",
        "iam::role_grant_request_roles:read",
        [](const ores::database::context& scoped_ctx, const std::string& entity_id) {
            repository::role_grant_request_role_repository repo(scoped_ctx);
            const auto rows =
                repo.read_all_by_request(boost::lexical_cast<boost::uuids::uuid>(entity_id));

            std::vector<ores::history::messaging::entity_history_version> versions;
            for (auto first = rows.begin(); first != rows.end();) {
                const auto last = std::find_if(first, rows.end(), [&](const auto& row) {
                    return row.link.role_id != first->link.role_id;
                });
                const std::vector<repository::role_grant_request_role_history_row> link(first,
                                                                                        last);
                auto own = ores::history::service::build_entity_history_versions(
                    link, presentation::render_role_grant_request_role_fields);
                versions.insert(versions.end(),
                                std::make_move_iterator(own.begin()),
                                std::make_move_iterator(own.end()));
                first = last;
            }
            return versions;
        });
}

}
