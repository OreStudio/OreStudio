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
#include "ores.analytics.core/repository/todays_market_collection_repository.hpp"
#include "ores.analytics.core/repository/todays_market_config_repository.hpp"
#include "ores.analytics.core/service/todays_market_document_service.hpp"
#include "ores.database/domain/party_scope.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/todays_market_projection.hpp"
#include "ores.ore.core/xml/record_diff.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "party_fixture.hpp"
#include "test_helpers.hpp"
#include "test_suite.hpp"
#include <boost/uuid/random_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <string>
#include <string_view>
#include <vector>


using ores::analytics::repository::todays_market_collection_repository;
using ores::analytics::repository::todays_market_config_repository;
using ores::ore::domain::todays_market_projection;
using ores::platform::filesystem::file;
using ores::testing::scoped_database_helper;

/**
 * The whole path, through the database: a corpus document is parsed, mapped into
 * the analytics entities, written to PostgreSQL, read back, and only then turned
 * into a document again. The in-memory walk proves the projection over every corpus
 * file; this proves the tables hold what the projection produces.
 */
TEST_CASE("todays_market_roundtrip_through_the_database", "[todaysmarket][database][roundtrip]") {
    auto lg(ores::logging::make_logger(test_suite));
    scoped_database_helper h;

    // The largest document of the kind in the corpus, so the path carries many
    // collections, duplicate keys and references rather than a trivial one.
    const auto f = ore_path("examples/Products/Input/todaysmarket.xml");
    const std::string content = file::read_content(f);

    const auto original = todays_market_projection::read(content);

    ores::analytics::messaging::todays_market_document mapped =
        todays_market_projection::map(original);
    ores::database::domain::assign_party(mapped, boost::uuids::random_generator()());
    REQUIRE(!mapped.collections.empty());
    REQUIRE(!mapped.entries.empty());
    REQUIRE(!mapped.configurations.empty());
    REQUIRE(!mapped.bindings.empty());

    ores::analytics::service::todays_market_document_service(h.context()).save(mapped);

    // Read back from the store: the read is not allowed to be the write's own
    // memory.
    const auto from_database =
        ores::analytics::service::todays_market_document_service(h.context()).get(mapped.config.id);
    const auto& entries = from_database.entries;
    INFO("collections read back: " << from_database.collections.size() << " of "
                                   << mapped.collections.size());
    INFO("entries read back: " << entries.size() << " of " << mapped.entries.size());
    REQUIRE(from_database.collections.size() == mapped.collections.size());
    REQUIRE(entries.size() == mapped.entries.size());
    REQUIRE(from_database.configurations.size() == mapped.configurations.size());
    REQUIRE(from_database.bindings.size() == mapped.bindings.size());

    // The document names its correlations with an ampersand. The database must
    // hold the character the document means, not the escape it was written in.
    bool holds_an_ampersand = false;
    for (const auto& e : entries) {
        INFO(e.target);
        CHECK(e.target.find("&amp;") == std::string::npos);
        if (e.target.find('&') != std::string::npos)
            holds_an_ampersand = true;
    }
    INFO("expected a correlation entry containing '&' in "
         << f.string() << "; if the fixture changed, pick one that has one");
    CHECK(holds_an_ampersand);

    // Export from what the database returned, not from what was written.
    const auto rebuilt = todays_market_projection::read(
        todays_market_projection::write(todays_market_projection::reverse(from_database)));
    const auto difference = ores::ore::xml::compare_records(
        *ores::ore::xml::generated::todays_market::todays_market_document().root,
        &original,
        &rebuilt);
    INFO(f.string());
    if (difference)
        INFO(difference->path << ": " << difference->detail);
    CHECK_FALSE(difference);
}

TEST_CASE("a collection naming a kind ORE does not have is refused",
          "[todaysmarket][database][roundtrip]") {
    scoped_database_helper h;

    const auto f = ore_path("examples/Products/Input/todaysmarket.xml");
    auto mapped =
        todays_market_projection::map(todays_market_projection::read(file::read_content(f)));
    REQUIRE(!mapped.collections.empty());
    auto collection = mapped.collections.front();
    collection.collection = "YieldCurve";
    // Document names are unique in a tenant, and the round trip case shares it.
    mapped.config.name = "unknown kind refusal";

    todays_market_config_repository().write(h.context(), mapped.config);
    CHECK_THROWS(todays_market_collection_repository().write(h.context(), collection));
}

TEST_CASE("a party sees only its own today's market configuration",
          "[todaysmarket][database][roundtrip]") {
    scoped_database_helper h;
    auto parties = ores::ore::tests::make_two_parties(h);
    auto mapped = todays_market_projection::map(todays_market_projection::read(
        file::read_content(ore_path("examples/Products/Input/todaysmarket.xml"))));
    ores::database::domain::assign_party(mapped, parties.a);

    ores::analytics::repository::todays_market_config_repository repo;
    repo.write(parties.a_context, mapped.config);

    const auto owns = [&](const auto& rows) {
        return std::ranges::any_of(rows, [&](const auto& r) { return r.id == mapped.config.id; });
    };
    CHECK(owns(repo.read_latest(parties.a_context)));
    CHECK_FALSE(owns(repo.read_latest(parties.b_context)));
}
