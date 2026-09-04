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
#include "ores.refdata.core/messaging/party_id_scheme_history_provider_registrar.hpp"
#include "ores.history.api/service/version_builder.hpp"
#include "ores.refdata.core/presentation/party_id_scheme_history_field_mapper.hpp"
#include "ores.refdata.core/service/party_id_scheme_service.hpp"

namespace ores::refdata::messaging {

void register_party_id_scheme_history_provider(
    ores::history::service::dispatch_registry& registry) {
    registry.register_history_provider(
        "ores.refdata.party_id_scheme",
        [](const ores::database::context& scoped_ctx, const std::string& entity_id) {
            service::party_id_scheme_service svc(scoped_ctx);
            auto versions = svc.get_scheme_history(entity_id);
            return ores::history::service::build_entity_history_versions(
                versions, presentation::render_party_id_scheme_fields);
        });
}

} // namespace ores::refdata::messaging
