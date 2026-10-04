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
#include "ores.analytics.core/repository/pricing_model_config_repository.hpp"
#include "ores.analytics.core/repository/pricing_model_product_parameter_repository.hpp"
#include "ores.analytics.core/repository/pricing_model_product_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/party_scope.hpp"
#include "ores.ore.core/domain/pricing_engine_mapper.hpp"
#include "ores.ore.core/xml/roundtrip_harness.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "party_fixture.hpp"
#include <boost/uuid/random_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <string>
#include <string_view>
#include <vector>

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[pricingengines][database][roundtrip]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

}

using ores::analytics::domain::pricing_model_product;
using ores::analytics::domain::pricing_model_product_parameter;
using ores::analytics::repository::pricing_model_config_repository;
using ores::analytics::repository::pricing_model_product_parameter_repository;
using ores::analytics::repository::pricing_model_product_repository;
using ores::ore::domain::mapped_pricing_engines;
using ores::ore::domain::pricing_engine_mapper;
using ores::ore::domain::pricingengines;
using ores::ore::xml::parsed_text_difference;
using ores::platform::filesystem::file;
using ores::testing::scoped_database_helper;

/**
 * The whole path, through the database: a corpus document is parsed, mapped into
 * the analytics entities, written to PostgreSQL, read back, and only then turned
 * into a document again. The in-memory walk proves the mapper; this proves the
 * tables hold what the mapper produces.
 */
TEST_CASE("pricing_engines_roundtrip_through_the_database", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    scoped_database_helper h;

    // The largest document in the corpus, so the path carries eighty products,
    // a repeated engine type, repeated parameter names and nine global
    // parameters rather than a document that could pass by being trivial.
    const auto f = ore_path("examples/InitialMargin/Input/DimValidation/pricingengine.xml");
    const std::string content = file::read_content(f);

    pricingengines original;
    ores::ore::domain::load_data(content, original);

    mapped_pricing_engines mapped = pricing_engine_mapper::map(original);
    ores::ore::domain::assign_party(mapped, boost::uuids::random_generator()());
    REQUIRE(mapped.products.size() == original.Product.size());
    REQUIRE(!mapped.parameters.empty());

    pricing_model_config_repository config_repo;
    pricing_model_product_repository product_repo;
    pricing_model_product_parameter_repository parameter_repo;

    config_repo.write(h.context(), mapped.config);
    product_repo.write(h.context(), mapped.products);
    parameter_repo.write(h.context(), mapped.parameters);

    // Read back from the store, filtered to this configuration: the read is not
    // allowed to be the write's own memory.
    std::vector<pricing_model_product> products;
    for (const auto& row : product_repo.read_latest(h.context())) {
        if (row.pricing_model_config_id == mapped.config.id)
            products.push_back(row);
    }

    std::vector<pricing_model_product_parameter> parameters;
    for (const auto& row : parameter_repo.read_latest(h.context())) {
        if (row.pricing_model_config_id == mapped.config.id)
            parameters.push_back(row);
    }

    INFO("products read back: " << products.size() << " of " << mapped.products.size());
    INFO("parameters read back: " << parameters.size() << " of " << mapped.parameters.size());
    REQUIRE(products.size() == mapped.products.size());
    REQUIRE(parameters.size() == mapped.parameters.size());

    // Export from what the database returned, not from what was written.
    mapped_pricing_engines from_database;
    from_database.config = mapped.config;
    from_database.products = products;
    from_database.parameters = parameters;

    const pricingengines rebuilt = pricing_engine_mapper::reverse(from_database);

    pricingengines exported;
    ores::ore::domain::load_data(ores::ore::domain::save_data(rebuilt), exported);

    const std::string difference = parsed_text_difference(original, exported, f.string());
    INFO(difference);
    CHECK(difference.empty());
}

TEST_CASE("a party sees only its own pricing engine configuration", tags) {
    scoped_database_helper h;
    auto parties = ores::ore::tests::make_two_parties(h);
    pricingengines original;
    ores::ore::domain::load_data(
        file::read_content(
            ore_path("examples/InitialMargin/Input/DimValidation/pricingengine.xml")),
        original);
    auto mapped = pricing_engine_mapper::map(original);
    ores::ore::domain::assign_party(mapped, parties.a);

    ores::analytics::repository::pricing_model_config_repository repo;
    repo.write(parties.a_context, mapped.config);

    const auto owns = [&](const auto& rows) {
        return std::ranges::any_of(rows, [&](const auto& r) { return r.id == mapped.config.id; });
    };
    CHECK(owns(repo.read_latest(parties.a_context)));
    CHECK_FALSE(owns(repo.read_latest(parties.b_context)));
}
