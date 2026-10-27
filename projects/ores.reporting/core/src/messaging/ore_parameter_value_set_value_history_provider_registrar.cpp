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
#include "ores.reporting.core/messaging/ore_parameter_value_set_value_history_provider_registrar.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.history.api/service/version_builder.hpp"
#include "ores.history.core/service/dispatch_registry.hpp"
#include "ores.reporting.core/presentation/ore_parameter_value_set_value_history_field_mapper.hpp"
#include "ores.reporting.core/service/ore_parameter_value_set_value_service.hpp"
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::reporting::messaging {

void register_ore_parameter_value_set_value_history_provider(
    ores::history::service::dispatch_registry& registry) {
    registry.register_history_provider(
        "ores.reporting.ore_parameter_value_set_value",
        "reporting::ore_parameter_value_set_values:read",
        [](const ores::database::context& scoped_ctx, const std::string& entity_id) {
            // The rows are the registry's, held by the system tenant, so the
            // read is scoped there. A read in the caller's tenant answers an
            // empty history for a row the caller may read.
            const auto sys_ctx = scoped_ctx.with_tenant(ores::utility::uuid::tenant_id::system(),
                                                        scoped_ctx.actor());
            service::ore_parameter_value_set_value_service svc(sys_ctx);
            auto versions = svc.get_value_history(entity_id);
            return ores::history::service::build_entity_history_versions(
                versions, presentation::render_ore_parameter_value_set_value_fields);
        });
}

}
