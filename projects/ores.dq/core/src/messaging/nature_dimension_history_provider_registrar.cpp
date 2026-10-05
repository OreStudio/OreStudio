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
#include "ores.dq.core/messaging/nature_dimension_history_provider_registrar.hpp"
#include "ores.dq.core/presentation/nature_dimension_history_field_mapper.hpp"
#include "ores.dq.core/service/nature_dimension_service.hpp"
#include "ores.history.api/service/version_builder.hpp"

namespace ores::dq::messaging {

void register_nature_dimension_history_provider(
    ores::history::service::dispatch_registry& registry) {
    registry.register_history_provider(
        "ores.dq.nature_dimension",
        "dq::nature_dimensions:read",
        [](const ores::database::context& scoped_ctx, const std::string& entity_id) {
            service::nature_dimension_service svc(scoped_ctx);
            auto versions = svc.get_dimension_history(entity_id);
            return ores::history::service::build_entity_history_versions(
                versions, presentation::render_nature_dimension_fields);
        });
}

}
