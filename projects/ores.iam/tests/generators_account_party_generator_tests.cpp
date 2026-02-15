/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.iam/generators/account_party_generator.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[generators]");

}

using namespace ores::iam::generators;
using namespace ores::logging;
using ores::utility::generation::generation_context;

TEST_CASE("account_party_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_account_party(ctx);

    BOOST_LOG_SEV(lg, info) << "Generated account_party";

    CHECK(sut.version == 1);
    CHECK(sut.tenant_id == "system");
    CHECK(!sut.account_id.is_nil());
    CHECK(!sut.party_id.is_nil());
    CHECK(!sut.modified_by.empty());
    CHECK(!sut.performed_by.empty());
    CHECK(sut.change_reason_code == "system.test");
    CHECK(sut.change_commentary == "Synthetic test data");
}

TEST_CASE("account_party_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_account_parties(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.account_id.is_nil());
        CHECK(!item.party_id.is_nil());
    }
}
