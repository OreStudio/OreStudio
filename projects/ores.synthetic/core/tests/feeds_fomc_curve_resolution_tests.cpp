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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/feeds/ir_curve_template_resolver.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/publish_helper.hpp"
#include "ores.testing/test_database_manager.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <utility>
#include <vector>

namespace {

const std::string tags("[feeds][fomc]");

/// A tenant of this test's own, with the base calendar data published to it.
///
/// A tenant gets its calendars, tenor schedules and calendar events from
/// datasets, after provisioning. The FOMC tenors resolve only once all of them
/// are present, so the test publishes the chain the base bundle publishes, in
/// its dependency order. The shared test tenant is left as it is.
struct fomc_tenant {
    ores::testing::database_helper base;
    ores::database::context ctx;

    fomc_tenant()
        : ctx(base.context()) {
        using ores::testing::test_database_manager;
        const auto code = test_database_manager::generate_test_tenant_code("synthetic.fomc");
        const auto tenant =
            test_database_manager::provision_test_tenant(ctx, code, "ores.synthetic FOMC curve");

        // The build publishes nothing, so the test publishes the chain it
        // needs. The coding schemes come first: the country publish resolves
        // ISO_3166_1_ALPHA_2 against them.
        REQUIRE(ores::testing::publish_dataset(
            ctx, tenant, "iso.coding_schemes", "ores_dq_coding_schemes_publish_fn"));
        for (const auto& [dataset, entity] :
             {std::pair{"iso.countries", "countries"},
              std::pair{"refdata.calendar_types", "calendar_types"},
              std::pair{"refdata.calendars", "calendars"},
              std::pair{"refdata.tenor_schedules", "tenor_schedules"},
              std::pair{"refdata.calendar_events", "calendar_events"}}) {
            REQUIRE(ores::testing::publish_refdata_dataset(ctx, tenant, dataset, entity));
        }
        ctx = ores::database::service::tenant_context::with_tenant(base.context(), tenant);
    }
};

ores::synthetic::domain::ir_curve_template_entry
entry(int sequence_index, std::string start, std::string end, std::string instrument) {
    ores::synthetic::domain::ir_curve_template_entry e;
    e.sequence_index = sequence_index;
    e.start_tenor_code = std::move(start);
    e.end_tenor_code = std::move(end);
    e.instrument_code = std::move(instrument);
    return e;
}

}

using namespace std::chrono;
using ores::synthetic::feed::build_ir_curve_refdata_context;
using ores::synthetic::feed::resolve;

TEST_CASE("fomc_tenors_resolve_to_meeting_dates_in_a_new_tenant", tags) {
    fomc_tenant t;

    auto refctx = build_ir_curve_refdata_context(t.ctx, "RATES_SPOT_FOMC");
    REQUIRE(refctx.has_value());
    refctx->horizon = 2026y / October / 4d;
    refctx->spot = refctx->horizon;

    const auto resolved = resolve(
        {entry(0, "SPOT", "1F", "DEPO"), entry(1, "1F", "2F", "FRA"), entry(7, "7F", "8F", "FRA")},
        *refctx,
        "Quarterly");

    REQUIRE(resolved.size() == 3);
    CHECK(resolved[0].start_date == 2026y / October / 4d);
    CHECK(resolved[0].end_date == 2026y / October / 28d);
    CHECK(resolved[1].start_date == 2026y / October / 28d);
    CHECK(resolved[1].end_date == 2026y / December / 9d);
    CHECK(resolved[2].start_date == 2027y / July / 28d);
    CHECK(resolved[2].end_date == 2027y / September / 15d);
}
