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
#include "ores.marketdata.core/messaging/market_fixing_history_provider_registrar.hpp"
#include "ores.history.api/service/version_builder.hpp"
#include "ores.marketdata.core/presentation/market_fixing_history_field_mapper.hpp"
#include "ores.marketdata.core/service/market_fixing_service.hpp"

namespace ores::marketdata::messaging {

void register_market_fixing_history_provider(ores::history::service::dispatch_registry& registry) {
    registry.register_history_provider(
        "ores.marketdata.market_fixing",
        [](const ores::database::context& scoped_ctx, const std::string& entity_id) {
            service::market_fixing_service svc(scoped_ctx);
            auto versions = svc.get_market_fixing_history(entity_id);
            return ores::history::service::build_entity_history_versions(
                versions, presentation::render_market_fixing_fields);
        });
}

} // namespace ores::marketdata::messaging
