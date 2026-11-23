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
#include "ores.marketdata.service/app/demand_catalogue_reader.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.marketdata.core/repository/feed_repository.hpp"
#include "ores.marketdata.core/repository/feed_scope_repository.hpp"
#include "ores.marketdata.core/repository/live_tenant_repository.hpp"
#include "ores.marketdata.core/repository/market_data_mapping_repository.hpp"
#include "ores.marketdata.core/repository/market_data_vendor_instrument_repository.hpp"
#include "ores.marketdata.core/repository/provider_repository.hpp"
#include "ores.marketdata.core/repository/route_repository.hpp"
#include "ores.marketdata.core/repository/subscription_repository.hpp"
#include "ores.marketdata.service/app/live_tenants.hpp"

namespace ores::marketdata::service::app {

demand_catalogue read_demand_catalogue(const ores::database::context& ctx) {
    const auto system_ctx = ores::database::service::tenant_context::with_system_tenant(ctx);
    demand_catalogue out;
    out.providers = repository::provider_repository{}.read_latest(system_ctx);
    out.feeds = repository::feed_repository{}.read_latest(system_ctx);
    out.scopes = repository::feed_scope_repository{}.read_latest(system_ctx);
    out.instruments =
        repository::market_data_vendor_instrument_repository{}.read_latest(system_ctx);
    out.mappings = repository::market_data_mapping_repository{}.read_latest(system_ctx);
    out.routes = repository::route_repository{}.read_latest_all_tenants(ctx);
    out.subscriptions = repository::subscription_repository{}.read_latest_all_tenants(ctx);
    const auto live = repository::live_tenant_repository{}.read(ctx);
    keep_live_tenants(out.routes, live);
    keep_live_tenants(out.subscriptions, live);
    return out;
}

}
