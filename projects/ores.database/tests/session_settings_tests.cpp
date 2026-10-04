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
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

const std::string test_suite("ores.database.tests");
const std::string tags("[session_settings]");

}

using ores::database::repository::execute_raw_multi_column_query;

TEST_CASE("a context without a party sees no party left on a pooled connection", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto party = boost::uuids::random_generator()();
    const auto party_ctx = h.context().with_party(h.tenant_id(), party, {party}, "party.user");
    const auto tenant_ctx = h.context().with_tenant(h.tenant_id(), "");

    const std::string sql = "select ores_iam_current_party_id_fn()::text, "
                            "ores_iam_visible_party_ids_fn()::text, ores_iam_current_actor_fn()";
    for (int i = 0; i < 50; ++i) {
        const auto as_party = execute_raw_multi_column_query(party_ctx, sql, lg, "party settings");
        REQUIRE(as_party.size() == 1);
        CHECK(as_party[0][0] == boost::uuids::to_string(party));

        const auto as_tenant =
            execute_raw_multi_column_query(tenant_ctx, sql, lg, "tenant settings");
        REQUIRE(as_tenant.size() == 1);
        INFO("round " << i);
        CHECK_FALSE(as_tenant[0][0].has_value());
        CHECK_FALSE(as_tenant[0][1].has_value());
        CHECK_FALSE(as_tenant[0][2].has_value());
    }
}
