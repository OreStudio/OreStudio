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
#include "ores.database/service/tenant_context.hpp"
#include "ores.synthetic.api/domain/market_data_provider.hpp"
#include "ores.synthetic.api/generators/market_data_provider_generator.hpp"
#include "ores.synthetic.core/repository/market_data_provider_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

const std::string tags("[repository][system_visibility]");

using ores::database::service::tenant_context;
using ores::synthetic::repository::market_data_provider_repository;

}

// The installation's providers, system collections and their feeds ship under
// the system tenant, and every tenant reads them. The generated reads state
// that with the system tenant visible flag; the row level security policy
// still decides which system rows a tenant sees.
TEST_CASE("a_tenant_reads_a_provider_the_system_tenant_holds", tags) {
    ores::testing::scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto& tenant_ctx = h.context();
    const auto system_ctx = tenant_context::with_system_tenant(tenant_ctx);
    REQUIRE(tenant_ctx.tenant_id().to_string() != tenant_context::system_tenant_id);

    auto provider = ores::synthetic::generators::generate_synthetic_market_data_provider(gen);
    provider.code = "visibility_probe";
    provider.tenant_id =
        ores::utility::uuid::tenant_id::from_string(tenant_context::system_tenant_id).value();
    provider.change_reason_code = "system.test";

    market_data_provider_repository repo;
    repo.write(system_ctx, provider);
    // The probe leaves the system catalogue however the reads below end.
    struct probe_removal {
        market_data_provider_repository& repo;
        const ores::database::context& ctx;
        const std::string& code;
        ~probe_removal() {
            try {
                repo.remove(ctx, code);
            } catch (...) {
            }
        }
    } removal{repo, system_ctx, provider.code};

    const auto by_code = repo.read_latest(tenant_ctx, provider.code);
    const auto all = repo.read_latest(tenant_ctx);

    REQUIRE(by_code.size() == 1);
    CHECK(by_code.front().code == provider.code);
    CHECK(std::ranges::any_of(all, [&](const auto& row) { return row.code == provider.code; }));
}
