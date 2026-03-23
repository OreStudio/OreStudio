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
#include "ores.synthetic.api/domain/synthetic_catalog.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.synthetic.tests");
const std::string tags("[domain][synthetic_catalog]");

}

using ores::synthetic::domain::synthetic_catalog;
using namespace ores::logging;

TEST_CASE("default_constructed_synthetic_catalog_has_zero_seed_and_empty_vectors", tags) {
    auto lg(make_logger(test_suite));

    const synthetic_catalog sut;
    BOOST_LOG_SEV(lg, info) << "Default synthetic_catalog seed: " << sut.seed;

    CHECK(sut.seed == 0);
    CHECK(sut.accounts.empty());
    CHECK(sut.catalogs.empty());
    CHECK(sut.data_domains.empty());
    CHECK(sut.subject_areas.empty());
    CHECK(sut.origin_dimensions.empty());
    CHECK(sut.nature_dimensions.empty());
    CHECK(sut.treatment_dimensions.empty());
    CHECK(sut.datasets.empty());
    CHECK(sut.dependencies.empty());
}

TEST_CASE("synthetic_catalog_seed_can_be_set", tags) {
    auto lg(make_logger(test_suite));

    synthetic_catalog sut;
    sut.seed = 777ULL;
    BOOST_LOG_SEV(lg, info) << "synthetic_catalog seed: " << sut.seed;

    CHECK(sut.seed == 777ULL);
}

TEST_CASE("synthetic_catalog_dependency_can_be_added", tags) {
    auto lg(make_logger(test_suite));

    synthetic_catalog sut;
    sut.dependencies.push_back("ISO Reference Data");
    sut.dependencies.push_back("Core DQ Dimensions");
    BOOST_LOG_SEV(lg, info) << "synthetic_catalog dependency count: "
                            << sut.dependencies.size();

    CHECK(sut.dependencies.size() == 2);
    CHECK(sut.dependencies[0] == "ISO Reference Data");
    CHECK(sut.dependencies[1] == "Core DQ Dimensions");
}
