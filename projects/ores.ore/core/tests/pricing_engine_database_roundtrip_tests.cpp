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
#include "ores.analytics.core/service/pricing_engines_document_service.hpp"
#include "ores.database/domain/party_scope.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/pricing_engine_projection.hpp"
#include "ores.ore.core/xml/generated/pricing_engines.hpp"
#include "ores.ore.core/xml/record_diff.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "party_fixture.hpp"
#include "test_suite.hpp"
#include <boost/uuid/random_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <string>
#include <string_view>
#include <vector>

namespace {

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

}

using ores::analytics::repository::pricing_model_config_repository;
using ores::ore::domain::pricing_engine_projection;
using ores::platform::filesystem::file;
using ores::testing::scoped_database_helper;

/**
 * The whole path, through the database: a corpus document is parsed, mapped into
 * the analytics entities, written to PostgreSQL, read back, and only then turned
 * into a document again. The in-memory walk proves the mapper; this proves the
 * tables hold what the mapper produces.
 */
TEST_CASE("pricing_engines_roundtrip_through_the_database",
          "[pricingengines][database][roundtrip]") {
    auto lg(ores::logging::make_logger(test_suite));
    scoped_database_helper h;

    // The largest document in the corpus, so the path carries eighty products,
    // a repeated engine type, repeated parameter names and nine global
    // parameters rather than a document that could pass by being trivial.
    const auto f = ore_path("examples/InitialMargin/Input/DimValidation/pricingengine.xml");
    const std::string content = file::read_content(f);

    const auto original = pricing_engine_projection::read(content);

    ores::analytics::messaging::pricing_engines_document mapped =
        pricing_engine_projection::map(original);
    ores::database::domain::assign_party(mapped, boost::uuids::random_generator()());
    REQUIRE(mapped.products.size() == original.product.size());
    REQUIRE(!mapped.parameters.empty());

    ores::analytics::service::pricing_engines_document_service(h.context()).save(mapped);

    // Read back from the store: the read is not allowed to be the write's own
    // memory.
    const auto from_database =
        ores::analytics::service::pricing_engines_document_service(h.context())
            .get(mapped.config.id);
    INFO("products read back: " << from_database.products.size() << " of "
                                << mapped.products.size());
    INFO("parameters read back: " << from_database.parameters.size() << " of "
                                  << mapped.parameters.size());
    REQUIRE(from_database.products.size() == mapped.products.size());
    REQUIRE(from_database.parameters.size() == mapped.parameters.size());

    const auto rebuilt = pricing_engine_projection::reverse(from_database);
    const auto exported =
        pricing_engine_projection::read(pricing_engine_projection::write(rebuilt));

    const auto difference = ores::ore::xml::compare_records(
        *ores::ore::xml::generated::pricing_engines::pricing_engines_document().root,
        &original,
        &exported);
    INFO(f.string());
    if (difference)
        INFO(difference->path << ": " << difference->detail);
    CHECK_FALSE(difference);
}

TEST_CASE("a party sees only its own pricing engine configuration",
          "[pricingengines][database][roundtrip]") {
    scoped_database_helper h;
    auto parties = ores::ore::tests::make_two_parties(h);
    const auto original = pricing_engine_projection::read(file::read_content(
        ore_path("examples/InitialMargin/Input/DimValidation/pricingengine.xml")));
    auto mapped = pricing_engine_projection::map(original);
    ores::database::domain::assign_party(mapped, parties.a);

    ores::analytics::repository::pricing_model_config_repository repo;
    repo.write(parties.a_context, mapped.config);

    const auto owns = [&](const auto& rows) {
        return std::ranges::any_of(rows, [&](const auto& r) { return r.id == mapped.config.id; });
    };
    CHECK(owns(repo.read_latest(parties.a_context)));
    CHECK_FALSE(owns(repo.read_latest(parties.b_context)));
}
