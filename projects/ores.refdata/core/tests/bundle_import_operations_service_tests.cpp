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
#include "ores.refdata.core/service/conventions_bundle.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <stdexcept>
#include <string>
#include <utility>

namespace {

/**
 * @brief A context whose database is unreachable, so a call that reaches
 * storage fails.
 *
 * The staged dispatch decodes a payload before it writes, so these cases need
 * no database at all. The pool opens on first use against a port nothing
 * listens on, once: a test that reaches storage fails at once.
 */
ores::database::context database_less_context() {
    return ores::database::context(
        ores::database::pq::connection_options{.host = "127.0.0.1", .port = 1, .dbname = "none"},
        ores::utility::uuid::tenant_id::system(),
        "",
        "",
        ores::database::pq::open_policy{.attempts = 1});
}

}

TEST_CASE("the staged dispatch returns false for a family the registry does not know",
          "[bundle_import]") {
    auto ctx = database_less_context();
    ores::refdata::service::conventions_apply_result result;

    CHECK_FALSE(ores::refdata::service::apply_staged_family(ctx, "no_such_family", "[]", result));
}

TEST_CASE("the staged dispatch has a branch for every registry family", "[bundle_import]") {
    auto ctx = database_less_context();

    REQUIRE(ores::refdata::domain::convention_family_count > 0);
    for (std::size_t i = 0; i < ores::refdata::domain::convention_family_count; ++i) {
        const auto family = std::string(ores::refdata::domain::convention_families[i].key);
        ores::refdata::service::conventions_apply_result result;
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
