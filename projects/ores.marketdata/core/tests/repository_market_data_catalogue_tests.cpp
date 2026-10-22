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
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_data_mapping.hpp"
#include "ores.marketdata.api/domain/provider.hpp"
#include "ores.marketdata.api/domain/market_data_source.hpp"
#include "ores.marketdata.api/domain/market_data_vendor_instrument.hpp"
#include "ores.marketdata.api/generators/market_data_mapping_generator.hpp"
#include "ores.marketdata.api/generators/provider_generator.hpp"
#include "ores.marketdata.api/generators/market_data_source_generator.hpp"
#include "ores.marketdata.api/generators/market_data_vendor_instrument_generator.hpp"
#include "ores.marketdata.core/repository/market_data_mapping_repository.hpp"
#include "ores.marketdata.core/repository/provider_repository.hpp"
#include "ores.marketdata.core/repository/market_data_source_repository.hpp"
#include "ores.marketdata.core/repository/market_data_vendor_instrument_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <catch2/catch_test_macros.hpp>
#include <chrono>

namespace {

const std::string_view test_suite("ores.marketdata.core.tests");
const std::string tags("[repository][catalogue]");

}

using namespace ores::marketdata::generators;

using ores::testing::database_helper;
using ores::marketdata::repository::market_data_mapping_repository;
using ores::marketdata::repository::provider_repository;
using ores::marketdata::repository::market_data_source_repository;
using ores::marketdata::repository::market_data_vendor_instrument_repository;

TEST_CASE("write_and_read_a_provider", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    provider_repository repo;
    auto p = generate_synthetic_provider(ctx);
    repo.write(h.context(), p);

    const auto read = repo.read_latest(h.context(), p.code);
    REQUIRE(read.size() == 1);
    CHECK(read.front().code == p.code);
    CHECK(read.front().kind == p.kind);
}

TEST_CASE("a_provider_has_at_most_one_default_source", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_data_source_repository repo;
    auto first = generate_synthetic_market_data_source(ctx);
    first.provider_code = "bloomberg";
    first.kind = "live";
    first.is_default = true;
    repo.write(h.context(), first);

    auto second = generate_synthetic_market_data_source(ctx);
    second.provider_code = "bloomberg";
    second.kind = "simulated";
    second.is_default = false;
    CHECK_NOTHROW(repo.write(h.context(), second));

    auto rival = generate_synthetic_market_data_source(ctx);
    rival.provider_code = "bloomberg";
    rival.kind = "simulated";
    rival.is_default = true;
    CHECK_THROWS(repo.write(h.context(), rival));
}

TEST_CASE("a_source_kind_must_be_in_the_vocabulary", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_data_source_repository repo;
    auto s = generate_synthetic_market_data_source(ctx);
    s.kind = "not_a_kind";
    CHECK_THROWS(repo.write(h.context(), s));
}

TEST_CASE("a_vendor_instrument_identity_is_unique", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_data_vendor_instrument_repository repo;
    auto a = generate_synthetic_market_data_vendor_instrument(ctx);
    a.provider_code = "bloomberg";
    a.id_scheme = "bbg_ticker";
    a.external_id = "EURUSD Curncy";
    a.field = "PX_LAST";
    a.minted = false;
    repo.write(h.context(), a);

    auto other_field = a;
    other_field.id = ctx.generate_uuid();
    other_field.field = "PX_BID";
    CHECK_NOTHROW(repo.write(h.context(), other_field));

    auto duplicate = a;
    duplicate.id = ctx.generate_uuid();
    CHECK_THROWS(repo.write(h.context(), duplicate));
}

TEST_CASE("a_minted_vendor_instrument_needs_a_deviation_note", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_data_vendor_instrument_repository repo;
    auto v = generate_synthetic_market_data_vendor_instrument(ctx);
    v.minted = true;
    v.deviation_note = "";
    CHECK_THROWS(repo.write(h.context(), v));

    v.deviation_note = "The vendor publishes this pillar only as a chain.";
    CHECK_NOTHROW(repo.write(h.context(), v));
}

TEST_CASE("a_mapping_is_unique_for_an_instrument_and_a_start", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_data_mapping_repository repo;
    auto m = generate_synthetic_market_data_mapping(ctx);
    repo.write(h.context(), m);

    auto later = m;
    later.id = ctx.generate_uuid();
    later.effective_from = m.effective_from + std::chrono::hours(24);
    CHECK_NOTHROW(repo.write(h.context(), later));

    auto duplicate = m;
    duplicate.id = ctx.generate_uuid();
    CHECK_THROWS(repo.write(h.context(), duplicate));
}

TEST_CASE("a_mapping_cannot_end_before_it_starts", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_data_mapping_repository repo;
    auto m = generate_synthetic_market_data_mapping(ctx);
    m.effective_to = m.effective_from - std::chrono::hours(1);
    CHECK_THROWS(repo.write(h.context(), m));
}
