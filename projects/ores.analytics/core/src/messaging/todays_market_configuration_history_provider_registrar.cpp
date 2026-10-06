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
#include "ores.analytics.core/messaging/todays_market_configuration_history_provider_registrar.hpp"
#include "ores.analytics.core/presentation/todays_market_configuration_history_field_mapper.hpp"
#include "ores.analytics.core/service/todays_market_configuration_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.history.api/service/version_builder.hpp"
#include "ores.history.core/service/dispatch_registry.hpp"

namespace ores::analytics::messaging {

void register_todays_market_configuration_history_provider(
    ores::history::service::dispatch_registry& registry) {
    registry.register_history_provider(
        "ores.analytics.todays_market_configuration",
        "analytics::todays_market_configurations:read",
        [](const ores::database::context& scoped_ctx, const std::string& entity_id) {
            service::todays_market_configuration_service svc(scoped_ctx);
            auto versions = svc.get_configuration_history(entity_id);
            return ores::history::service::build_entity_history_versions(
                versions, presentation::render_todays_market_configuration_fields);
        });
}

}
