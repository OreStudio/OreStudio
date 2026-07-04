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
#include "ores.marketdata.api/generators/market_series_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <array>
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>

namespace ores::marketdata::generators {

using ores::utility::generation::generation_keys;

namespace {

constexpr std::array<domain::asset_class, 8> asset_classes{domain::asset_class::fx,
                                                           domain::asset_class::rates,
                                                           domain::asset_class::credit,
                                                           domain::asset_class::equity,
                                                           domain::asset_class::commodity,
                                                           domain::asset_class::inflation,
                                                           domain::asset_class::bond,
                                                           domain::asset_class::cross_asset};

constexpr std::array<domain::series_subclass, 15> series_subclasses{
    domain::series_subclass::spot,
    domain::series_subclass::forward,
    domain::series_subclass::volatility,
    domain::series_subclass::yield,
    domain::series_subclass::basis,
    domain::series_subclass::fra,
    domain::series_subclass::xccy,
    domain::series_subclass::spread,
    domain::series_subclass::index_credit,
    domain::series_subclass::recovery,
    domain::series_subclass::swap,
    domain::series_subclass::capfloor,
    domain::series_subclass::seasonality,
    domain::series_subclass::price,
    domain::series_subclass::correlation};

}

domain::market_series
generate_synthetic_market_series(utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::market_series r;
    r.version = 1;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    const auto idx = counter.fetch_add(1, std::memory_order_relaxed);
    r.party_id = ctx.generate_uuid();
    r.series_type = std::string(faker::word::noun()) + "-" + std::to_string(idx);
    r.metric = std::string(faker::word::noun()) + "-" + std::to_string(idx);
    r.qualifier = std::string(faker::word::noun()) + "-" + std::to_string(idx);
    r.asset_class = asset_classes[static_cast<std::size_t>(idx) % asset_classes.size()];
    r.series_subclass = series_subclasses[static_cast<std::size_t>(idx) % series_subclasses.size()];
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::market_series>
generate_synthetic_market_series(std::size_t n, utility::generation::generation_context& ctx) {
    std::vector<domain::market_series> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_market_series(ctx));
    return r;
}

}
