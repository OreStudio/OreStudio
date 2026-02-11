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
#include "ores.dq/repository/dataset_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/dataset_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/generators/dataset_generator.hpp"
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::dq::generators;

using ores::testing::database_helper;
using ores::dq::repository::dataset_repository;

TEST_CASE("write_single_dataset", tags) {

    auto lg(make_logger(test_suite));

    database_helper h;

    dataset_repository repo(h.context());
    auto dataset = generate_synthetic_dataset();
    dataset.tenant_id = h.tenant_id();

    BOOST_LOG_SEV(lg, debug) << "Dataset: " << dataset;
    CHECK_NOTHROW(repo.write(dataset));
}

TEST_CASE("write_multiple_datasets", tags) {

    auto lg(make_logger(test_suite));

    database_helper h;

    dataset_repository repo(h.context());
    auto datasets = generate_synthetic_datasets(3);
    for (auto& d : datasets)
        d.tenant_id = h.tenant_id();
    BOOST_LOG_SEV(lg, debug) << "Datasets: " << datasets;

    CHECK_NOTHROW(repo.write(datasets));
}

TEST_CASE("read_latest_datasets", tags) {

    auto lg(make_logger(test_suite));

    database_helper h;

    dataset_repository repo(h.context());
    auto written_datasets = generate_synthetic_datasets(3);
    for (auto& d : written_datasets)
        d.tenant_id = h.tenant_id();
    BOOST_LOG_SEV(lg, debug) << "Written datasets: " << written_datasets;

    repo.write(written_datasets);

    auto read_datasets = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read datasets: " << read_datasets;

    CHECK(!read_datasets.empty());
    CHECK(read_datasets.size() >= written_datasets.size());
}

TEST_CASE("read_latest_dataset_by_id", tags) {

    auto lg(make_logger(test_suite));

    database_helper h;

    dataset_repository repo(h.context());
    auto datasets = generate_synthetic_datasets(3);
    for (auto& d : datasets)
        d.tenant_id = h.tenant_id();

    const auto target = datasets.front();
    BOOST_LOG_SEV(lg, debug) << "Write datasets: " << datasets;
    repo.write(datasets);

    BOOST_LOG_SEV(lg, debug) << "Target dataset: " << target;

    auto read_datasets = repo.read_latest(target.id);
    BOOST_LOG_SEV(lg, debug) << "Read datasets: " << read_datasets;

    REQUIRE(read_datasets.size() == 1);
    CHECK(read_datasets[0].id == target.id);
    CHECK(read_datasets[0].name == target.name);
    CHECK(read_datasets[0].code == target.code);
}

TEST_CASE("read_nonexistent_dataset_by_id", tags) {

    auto lg(make_logger(test_suite));

    database_helper h;

    dataset_repository repo(h.context());

    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent ID: " << nonexistent_id;

    auto read_datasets = repo.read_latest(nonexistent_id);
    BOOST_LOG_SEV(lg, debug) << "Read datasets: " << read_datasets;

    CHECK(read_datasets.size() == 0);
}
