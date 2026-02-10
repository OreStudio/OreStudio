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
#include "ores.iam/repository/tenant_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.iam/domain/tenant.hpp"
#include "ores.iam/domain/tenant_json_io.hpp" // IWYU pragma: keep.
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[repository]");

using ores::iam::domain::tenant;

tenant make_tenant(ores::testing::database_helper& /*h*/) {
    tenant t;
    t.version = 1;
    t.id = boost::uuids::random_generator()();
    t.code = std::string(faker::string::alphanumeric(10));
    t.name = std::string(faker::word::noun());
    t.type = "organisation";
    t.description = std::string(faker::lorem::sentence());
    t.hostname = std::string(faker::string::alphanumeric(8)) + ".example.com";
    t.status = "active";
    t.recorded_by = std::string(faker::internet::username());
    t.change_reason_code = "system.new";
    t.change_commentary = "Synthetic test data";
    t.performed_by = std::string(faker::internet::username());
    return t;
}

}

using namespace ores::logging;

using ores::testing::database_helper;
using ores::iam::repository::tenant_repository;

TEST_CASE("write_single_tenant", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    tenant_repository repo(h.context());
    auto t = make_tenant(h);

    BOOST_LOG_SEV(lg, debug) << "Tenant: " << t;
    CHECK_NOTHROW(repo.write(t));
}

TEST_CASE("write_multiple_tenants", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    tenant_repository repo(h.context());
    std::vector<tenant> tenants;
    for (int i = 0; i < 3; ++i)
        tenants.push_back(make_tenant(h));

    BOOST_LOG_SEV(lg, debug) << "Tenants: " << tenants;
    CHECK_NOTHROW(repo.write(tenants));
}

TEST_CASE("read_latest_tenants", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    tenant_repository repo(h.context());
    std::vector<tenant> written;
    for (int i = 0; i < 3; ++i)
        written.push_back(make_tenant(h));

    BOOST_LOG_SEV(lg, debug) << "Written tenants: " << written;
    repo.write(written);

    auto read_tenants = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read tenants: " << read_tenants;

    CHECK(!read_tenants.empty());
    CHECK(read_tenants.size() >= written.size());
}

TEST_CASE("read_latest_tenant_by_id", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    tenant_repository repo(h.context());
    auto t = make_tenant(h);
    const auto target_id = t.id;

    BOOST_LOG_SEV(lg, debug) << "Tenant: " << t;
    repo.write(t);

    BOOST_LOG_SEV(lg, debug) << "Target ID: " << target_id;

    auto read_tenants = repo.read_latest(target_id);
    BOOST_LOG_SEV(lg, debug) << "Read tenants: " << read_tenants;

    REQUIRE(read_tenants.size() == 1);
    CHECK(read_tenants[0].id == target_id);
    CHECK(read_tenants[0].code == t.code);
    CHECK(read_tenants[0].name == t.name);
}

TEST_CASE("read_latest_tenant_by_code", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    tenant_repository repo(h.context());
    auto t = make_tenant(h);
    const auto target_code = t.code;

    BOOST_LOG_SEV(lg, debug) << "Tenant: " << t;
    repo.write(t);

    BOOST_LOG_SEV(lg, debug) << "Target code: " << target_code;

    auto read_tenants = repo.read_latest_by_code(target_code);
    BOOST_LOG_SEV(lg, debug) << "Read tenants: " << read_tenants;

    REQUIRE(read_tenants.size() == 1);
    CHECK(read_tenants[0].code == target_code);
    CHECK(read_tenants[0].id == t.id);
}

TEST_CASE("read_nonexistent_tenant", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    tenant_repository repo(h.context());

    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent ID: " << nonexistent_id;

    auto read_tenants = repo.read_latest(nonexistent_id);
    BOOST_LOG_SEV(lg, debug) << "Read tenants: " << read_tenants;

    CHECK(read_tenants.size() == 0);
}
