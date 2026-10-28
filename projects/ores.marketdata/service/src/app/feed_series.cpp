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
#include "ores.marketdata.service/app/feed_series.hpp"
#include "ores.marketdata.api/domain/market_series_asset_class.hpp"
#include "ores.marketdata.core/repository/market_series_asset_class_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include <utility>
#include <vector>

namespace ores::marketdata::service::app {

domain::market_series make_feed_series(const consumer& owner,
                                       const boost::uuids::uuid& id,
                                       const std::string& series_uri,
                                       const std::string& series_subclass,
                                       const std::string& service_account) {
    domain::market_series series;
    series.id = id;
    series.tenant_id = owner.tenant_id;
    series.party_id = owner.party_id;
    series.oresmd_uri = series_uri;
    series.series_subclass = series_subclass;
    series.provider_code = owner.provider_code;
    series.modified_by = service_account;
    series.performed_by = service_account;
    series.change_reason_code = "system.initial_load";
    series.change_commentary = "Created by the feed ingest loop for source " + owner.source_name;
    return series;
}

domain::market_series create_feed_series(const ores::database::context& tenant_ctx,
                                         const consumer& owner,
                                         const boost::uuids::uuid& id,
                                         const std::string& series_uri,
                                         const core::series_classification& classification,
                                         const std::string& service_account) {
    auto series =
        make_feed_series(owner, id, series_uri, classification.series_subclass, service_account);
    repository::market_series_repository{}.write(tenant_ctx, series);

    std::vector<domain::market_series_asset_class> classes;
    for (const auto& code : classification.asset_classes) {
        domain::market_series_asset_class row;
        row.tenant_id = series.tenant_id.to_string();
        row.market_series_id = series.id;
        row.asset_class_code = code;
        row.modified_by = series.modified_by;
        row.performed_by = series.performed_by;
        row.change_reason_code = series.change_reason_code;
        row.change_commentary = series.change_commentary;
        classes.push_back(std::move(row));
    }
    if (!classes.empty())
        repository::market_series_asset_class_repository(tenant_ctx).write(classes);
    return series;
}

}
