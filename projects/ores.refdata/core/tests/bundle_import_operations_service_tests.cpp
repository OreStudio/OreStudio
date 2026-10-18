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
#include "ores.database/domain/context.hpp"
#include "ores.refdata.api/domain/convention_family_registry.hpp"
#include "ores.refdata.core/service/conventions_document_service.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <sqlgen/ConnectionPool.hpp>
#include <sqlgen/postgres.hpp>
#include <stdexcept>
#include <string>
#include <utility>

namespace {

const std::string tags("[bundle_import]");

/**
 * @brief A context with no connections, so a call that reaches storage fails.
 *
 * The staged dispatch decodes a payload before it writes, so these cases need
 * no database at all. The empty pool states that: a test that reaches storage
 * fails rather than opening a connection.
 */
ores::database::context database_less_context() {
    sqlgen::postgres::Credentials credentials;
    sqlgen::ConnectionPool<ores::database::context::connection_type> pool(
        sqlgen::ConnectionPoolConfig{.size = 0}, credentials);
    return ores::database::context(
        std::move(pool), std::move(credentials), ores::utility::uuid::tenant_id::system());
}

}

TEST_CASE("the staged dispatch returns false for a family the registry does not know", tags) {
    auto ctx = database_less_context();
    ores::refdata::service::conventions_save_result result;

    CHECK_FALSE(ores::refdata::service::apply_staged_family(ctx, "no_such_family", "[]", result));
}

TEST_CASE("the staged dispatch has a branch for every registry family", tags) {
    auto ctx = database_less_context();

    REQUIRE(ores::refdata::domain::convention_family_count > 0);
    for (std::size_t i = 0; i < ores::refdata::domain::convention_family_count; ++i) {
        const auto family = std::string(ores::refdata::domain::convention_families[i].key);
        ores::refdata::service::conventions_save_result result;
        auto reached_its_branch = false;
        try {
            // An array cannot decode into a family's value type, so a known
            // family throws here. An unknown family answers false instead, so
            // this distinguishes a branch that exists from a missing one.
            (void)ores::refdata::service::apply_staged_family(ctx, family, "[]", result);
        } catch (const std::exception&) {
            reached_its_branch = true;
        }
        INFO("family: " << family);
        CHECK(reached_its_branch);
    }
}
