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
#include "ores.synthetic.api/domain/generation_options.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.synthetic.tests");
const std::string tags("[domain][generation_options]");

}

using ores::synthetic::domain::generation_options;
using namespace ores::logging;

TEST_CASE("default_constructed_generation_options_has_expected_counts", tags) {
    auto lg(make_logger(test_suite));

    const generation_options sut;
    BOOST_LOG_SEV(lg, info) << "Default generation_options account_count: "
                            << sut.account_count;

    CHECK(!sut.seed.has_value());
    CHECK(sut.account_count == 5);
    CHECK(sut.catalog_count == 3);
    CHECK(sut.data_domain_count == 4);
    CHECK(sut.subject_areas_per_domain == 3);
    CHECK(sut.origin_dimension_count == 5);
    CHECK(sut.nature_dimension_count == 4);
    CHECK(sut.treatment_dimension_count == 4);
    CHECK(sut.dataset_count == 20);
    CHECK(!sut.methodology_id.has_value());
    CHECK(sut.dependencies.empty());
}

TEST_CASE("generation_options_seed_can_be_set", tags) {
    auto lg(make_logger(test_suite));

    generation_options sut;
    sut.seed = 42ULL;
    BOOST_LOG_SEV(lg, info) << "generation_options seed: " << *sut.seed;

    CHECK(sut.seed.has_value());
    CHECK(*sut.seed == 42ULL);
}

TEST_CASE("generation_options_counts_can_be_overridden", tags) {
    auto lg(make_logger(test_suite));

    generation_options sut;
    sut.account_count = 10;
    sut.catalog_count = 6;
    sut.data_domain_count = 8;
    sut.dataset_count = 50;
    BOOST_LOG_SEV(lg, info) << "generation_options account_count: "
                            << sut.account_count;

    CHECK(sut.account_count == 10);
    CHECK(sut.catalog_count == 6);
    CHECK(sut.data_domain_count == 8);
    CHECK(sut.dataset_count == 50);
}

TEST_CASE("generation_options_dependencies_can_be_set", tags) {
    auto lg(make_logger(test_suite));

    generation_options sut;
    sut.dependencies.push_back("ISO Reference Data");
    sut.dependencies.push_back("Core DQ Dimensions");
    BOOST_LOG_SEV(lg, info) << "generation_options dependency count: "
                            << sut.dependencies.size();

    CHECK(sut.dependencies.size() == 2);
    CHECK(sut.dependencies[0] == "ISO Reference Data");
    CHECK(sut.dependencies[1] == "Core DQ Dimensions");
}
