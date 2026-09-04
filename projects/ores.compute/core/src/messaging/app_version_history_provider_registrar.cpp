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
#include "ores.compute.core/messaging/app_version_history_provider_registrar.hpp"
#include "ores.compute.core/presentation/app_version_history_field_mapper.hpp"
#include "ores.compute.core/service/app_version_service.hpp"
#include "ores.history.api/service/version_builder.hpp"

namespace ores::compute::messaging {

void register_app_version_history_provider(ores::history::service::dispatch_registry& registry) {
    registry.register_history_provider(
        "ores.compute.app_version",
        [](const ores::database::context& scoped_ctx, const std::string& entity_id) {
            service::app_version_service svc(scoped_ctx);
            auto versions = svc.get_app_version_history(entity_id);
            return ores::history::service::build_entity_history_versions(
                versions, presentation::render_app_version_fields);
        });
}

} // namespace ores::compute::messaging
