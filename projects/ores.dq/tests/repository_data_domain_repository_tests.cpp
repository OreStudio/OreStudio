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
#include "ores.dq/repository/data_domain_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/data_domain_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/generators/data_domain_generator.hpp"
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::dq::generators;

using ores::testing::database_helper;
using ores::dq::repository::data_domain_repository;

TEST_CASE("write_single_data_domain", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    data_domain_repository repo(h.context());
    auto data_domain = generate_synthetic_data_domain();

    BOOST_LOG_SEV(lg, debug) << "Data domain: " << data_domain;
    CHECK_NOTHROW(repo.write(data_domain));
}

TEST_CASE("write_multiple_data_domains", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    data_domain_repository repo(h.context());
    auto data_domains = generate_synthetic_data_domains(3);
    BOOST_LOG_SEV(lg, debug) << "Data domains: " << data_domains;

    CHECK_NOTHROW(repo.write(data_domains));
}

TEST_CASE("read_latest_data_domains", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    data_domain_repository repo(h.context());
    auto written_data_domains = generate_synthetic_data_domains(3);
    BOOST_LOG_SEV(lg, debug) << "Written data domains: " << written_data_domains;

    repo.write(written_data_domains);

    auto read_data_domains = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read data domains: " << read_data_domains;

    CHECK(!read_data_domains.empty());
    CHECK(read_data_domains.size() >= written_data_domains.size());
}

TEST_CASE("read_latest_data_domain_by_name", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    data_domain_repository repo(h.context());
    auto data_domains = generate_synthetic_data_domains(3);

    const auto target = data_domains.front();
    BOOST_LOG_SEV(lg, debug) << "Write data domains: " << data_domains;
    repo.write(data_domains);

    BOOST_LOG_SEV(lg, debug) << "Target data domain: " << target;

    auto read_data_domains = repo.read_latest(target.name);
    BOOST_LOG_SEV(lg, debug) << "Read data domains: " << read_data_domains;

    REQUIRE(read_data_domains.size() == 1);
    CHECK(read_data_domains[0].name == target.name);
    CHECK(read_data_domains[0].description == target.description);
}

TEST_CASE("read_nonexistent_data_domain", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    data_domain_repository repo(h.context());

    const std::string nonexistent_name = "nonexistent.data_domain.12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent name: " << nonexistent_name;

    auto read_data_domains = repo.read_latest(nonexistent_name);
    BOOST_LOG_SEV(lg, debug) << "Read data domains: " << read_data_domains;

    CHECK(read_data_domains.size() == 0);
}
