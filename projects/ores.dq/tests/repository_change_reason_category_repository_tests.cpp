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
#include "ores.dq/repository/change_reason_category_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/change_reason_category_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/generators/change_reason_category_generator.hpp"
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::dq::generators;

using ores::testing::database_helper;
using ores::dq::repository::change_reason_category_repository;

TEST_CASE("write_single_change_reason_category", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    change_reason_category_repository repo(h.context());
    auto category = generate_synthetic_change_reason_category();

    BOOST_LOG_SEV(lg, debug) << "Change reason category: " << category;
    CHECK_NOTHROW(repo.write(category));
}

TEST_CASE("write_multiple_change_reason_categories", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    change_reason_category_repository repo(h.context());
    auto categories = generate_synthetic_change_reason_categories(3);
    BOOST_LOG_SEV(lg, debug) << "Change reason categories: " << categories;

    CHECK_NOTHROW(repo.write(categories));
}

TEST_CASE("read_latest_change_reason_categories", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    change_reason_category_repository repo(h.context());
    auto written_categories = generate_synthetic_change_reason_categories(3);
    BOOST_LOG_SEV(lg, debug) << "Written categories: " << written_categories;

    repo.write(written_categories);

    auto read_categories = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read categories: " << read_categories;

    CHECK(!read_categories.empty());
    CHECK(read_categories.size() >= written_categories.size());
}

TEST_CASE("read_latest_change_reason_category_by_code", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    change_reason_category_repository repo(h.context());
    auto categories = generate_synthetic_change_reason_categories(3);

    const auto target = categories.front();
    BOOST_LOG_SEV(lg, debug) << "Write categories: " << categories;
    repo.write(categories);

    BOOST_LOG_SEV(lg, debug) << "Target category: " << target;

    auto read_categories = repo.read_latest(target.code);
    BOOST_LOG_SEV(lg, debug) << "Read categories: " << read_categories;

    REQUIRE(read_categories.size() == 1);
    CHECK(read_categories[0].code == target.code);
    CHECK(read_categories[0].description == target.description);
}
