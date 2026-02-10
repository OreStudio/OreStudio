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
#include "ores.dq/repository/catalog_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/catalog_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/generators/catalog_generator.hpp"
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::dq::generators;

using ores::testing::database_helper;
using ores::dq::repository::catalog_repository;

TEST_CASE("write_single_catalog", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    catalog_repository repo(h.context());
    auto catalog = generate_synthetic_catalog();
    catalog.tenant_id = h.tenant_id().to_string();

    BOOST_LOG_SEV(lg, debug) << "Catalog: " << catalog;
    CHECK_NOTHROW(repo.write(catalog));
}

TEST_CASE("write_multiple_catalogs", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    catalog_repository repo(h.context());
    auto catalogs = generate_synthetic_catalogs(3);
    for (auto& c : catalogs)
        c.tenant_id = h.tenant_id().to_string();
    BOOST_LOG_SEV(lg, debug) << "Catalogs: " << catalogs;

    CHECK_NOTHROW(repo.write(catalogs));
}

TEST_CASE("read_latest_catalogs", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    catalog_repository repo(h.context());
    auto written_catalogs = generate_synthetic_catalogs(3);
    for (auto& c : written_catalogs)
        c.tenant_id = h.tenant_id().to_string();
    BOOST_LOG_SEV(lg, debug) << "Written catalogs: " << written_catalogs;

    repo.write(written_catalogs);

    auto read_catalogs = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read catalogs: " << read_catalogs;

    CHECK(!read_catalogs.empty());
    CHECK(read_catalogs.size() >= written_catalogs.size());
}

TEST_CASE("read_latest_catalog_by_name", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    catalog_repository repo(h.context());
    auto catalogs = generate_synthetic_catalogs(3);
    for (auto& c : catalogs)
        c.tenant_id = h.tenant_id().to_string();

    const auto target = catalogs.front();
    BOOST_LOG_SEV(lg, debug) << "Write catalogs: " << catalogs;
    repo.write(catalogs);

    BOOST_LOG_SEV(lg, debug) << "Target catalog: " << target;

    auto read_catalogs = repo.read_latest(target.name);
    BOOST_LOG_SEV(lg, debug) << "Read catalogs: " << read_catalogs;

    REQUIRE(read_catalogs.size() == 1);
    CHECK(read_catalogs[0].name == target.name);
    CHECK(read_catalogs[0].description == target.description);
}

TEST_CASE("read_all_catalog_versions", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    catalog_repository repo(h.context());

    auto cat1 = generate_synthetic_catalog();
    cat1.tenant_id = h.tenant_id().to_string();
    const auto test_name = cat1.name;
    BOOST_LOG_SEV(lg, debug) << "Catalog v1: " << cat1;

    auto cat2 = cat1;
    cat2.version = 1;
    cat2.description = "Updated description for version test";
    BOOST_LOG_SEV(lg, debug) << "Catalog v2: " << cat2;

    repo.write(cat1);
    repo.write(cat2);

    auto read_catalogs = repo.read_all(test_name);
    BOOST_LOG_SEV(lg, debug) << "Read catalogs: " << read_catalogs;

    CHECK(read_catalogs.size() >= 2);
}

TEST_CASE("read_nonexistent_catalog", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    catalog_repository repo(h.context());

    const std::string nonexistent_name = "nonexistent.catalog.12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent name: " << nonexistent_name;

    auto read_catalogs = repo.read_latest(nonexistent_name);
    BOOST_LOG_SEV(lg, debug) << "Read catalogs: " << read_catalogs;

    CHECK(read_catalogs.size() == 0);
}

TEST_CASE("write_and_read_catalog_roundtrip", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    catalog_repository repo(h.context());

    auto catalog = generate_synthetic_catalog();
    catalog.tenant_id = h.tenant_id().to_string();
    BOOST_LOG_SEV(lg, debug) << "Catalog: " << catalog;

    const auto catalog_name = catalog.name;
    repo.write(catalog);

    auto read_catalogs = repo.read_latest(catalog_name);
    BOOST_LOG_SEV(lg, debug) << "Read catalogs: " << read_catalogs;

    REQUIRE(read_catalogs.size() == 1);
    CHECK(read_catalogs[0].name == catalog.name);
    CHECK(read_catalogs[0].description == catalog.description);
}
