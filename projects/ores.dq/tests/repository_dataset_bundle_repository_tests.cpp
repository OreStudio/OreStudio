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
#include "ores.dq/repository/dataset_bundle_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/dataset_bundle_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/generators/dataset_bundle_generator.hpp"
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::dq::generators;

using ores::testing::database_helper;
using ores::dq::repository::dataset_bundle_repository;

TEST_CASE("write_single_dataset_bundle", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    dataset_bundle_repository repo(h.context());
    auto bundle = generate_synthetic_dataset_bundle();
    bundle.tenant_id = h.tenant_id().to_string();
    bundle.change_reason_code = "system.test";
    bundle.code = bundle.code + "_" + std::string(faker::string::alphanumeric(8));

    BOOST_LOG_SEV(lg, debug) << "Dataset bundle: " << bundle;
    CHECK_NOTHROW(repo.write(bundle));
}

TEST_CASE("write_multiple_dataset_bundles", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    dataset_bundle_repository repo(h.context());
    auto bundles = generate_synthetic_dataset_bundles(3);
    for (auto& b : bundles) {
        b.tenant_id = h.tenant_id().to_string();
        b.change_reason_code = "system.test";
        b.code = b.code + "_" + std::string(faker::string::alphanumeric(8));
    }
    BOOST_LOG_SEV(lg, debug) << "Dataset bundles: " << bundles;

    CHECK_NOTHROW(repo.write(bundles));
}

TEST_CASE("read_latest_dataset_bundles", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    dataset_bundle_repository repo(h.context());
    auto written_bundles = generate_synthetic_dataset_bundles(3);
    for (auto& b : written_bundles) {
        b.tenant_id = h.tenant_id().to_string();
        b.change_reason_code = "system.test";
        b.code = b.code + "_" + std::string(faker::string::alphanumeric(8));
    }
    BOOST_LOG_SEV(lg, debug) << "Written bundles: " << written_bundles;

    repo.write(written_bundles);

    auto read_bundles = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read bundles: " << read_bundles;

    CHECK(!read_bundles.empty());
    CHECK(read_bundles.size() >= written_bundles.size());
}

TEST_CASE("read_latest_dataset_bundle_by_id", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    dataset_bundle_repository repo(h.context());
    auto bundles = generate_synthetic_dataset_bundles(3);
    for (auto& b : bundles) {
        b.tenant_id = h.tenant_id().to_string();
        b.change_reason_code = "system.test";
        b.code = b.code + "_" + std::string(faker::string::alphanumeric(8));
    }

    const auto target = bundles.front();
    BOOST_LOG_SEV(lg, debug) << "Write bundles: " << bundles;
    repo.write(bundles);

    BOOST_LOG_SEV(lg, debug) << "Target bundle: " << target;

    auto read_bundles = repo.read_latest(target.id);
    BOOST_LOG_SEV(lg, debug) << "Read bundles: " << read_bundles;

    REQUIRE(read_bundles.size() == 1);
    CHECK(read_bundles[0].id == target.id);
    CHECK(read_bundles[0].code == target.code);
    CHECK(read_bundles[0].name == target.name);
}
