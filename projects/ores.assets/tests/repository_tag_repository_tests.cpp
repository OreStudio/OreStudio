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
#include "ores.assets/repository/tag_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/log/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.assets/domain/tag.hpp" // IWYU pragma: keep.
#include "ores.assets/domain/tag_json_io.hpp" // IWYU pragma: keep.
#include "ores.assets/generators/tag_generator.hpp"

namespace {

const std::string_view test_suite("ores.assets.tests");
const std::string database_table("oresdb.tags");
const std::string tags("[repository]");

}

using namespace ores::assets::generators;
using ores::assets::domain::tag;
using ores::assets::repository::tag_repository;
using ores::testing::scoped_database_helper;
using namespace ores::utility::log;

TEST_CASE("write_single_tag", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto t = generate_synthetic_tag();
    BOOST_LOG_SEV(lg, debug) << "Tag: " << t;

    tag_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), t));
}

TEST_CASE("write_multiple_tags", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto tag_list = generate_unique_synthetic_tags(3);
    BOOST_LOG_SEV(lg, debug) << "Tags: " << tag_list;

    tag_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), tag_list));
}

TEST_CASE("read_latest_tags", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto written_tags = generate_unique_synthetic_tags(3);
    BOOST_LOG_SEV(lg, debug) << "Written tags: " << written_tags;

    tag_repository repo;
    repo.write(h.context(), written_tags);

    auto read_tags = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read tags: " << read_tags;

    CHECK(!read_tags.empty());
    CHECK(read_tags.size() == written_tags.size());
}

TEST_CASE("read_latest_tag_by_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto t = generate_synthetic_tag();
    const auto original_description = t.description;
    BOOST_LOG_SEV(lg, debug) << "Tag: " << t;

    tag_repository repo;
    repo.write(h.context(), t);

    t.description = original_description + " v2";
    repo.write(h.context(), t);

    auto read_tags = repo.read_latest_by_id(h.context(), t.tag_id);
    BOOST_LOG_SEV(lg, debug) << "Read tags: " << read_tags;

    REQUIRE(read_tags.size() == 1);
    CHECK(read_tags[0].tag_id == t.tag_id);
    CHECK(read_tags[0].description == original_description + " v2");
}

TEST_CASE("read_latest_tag_by_name", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto t = generate_synthetic_tag();
    BOOST_LOG_SEV(lg, debug) << "Tag: " << t;

    tag_repository repo;
    repo.write(h.context(), t);

    auto read_tags = repo.read_latest_by_name(h.context(), t.name);
    BOOST_LOG_SEV(lg, debug) << "Read tags: " << read_tags;

    REQUIRE(read_tags.size() == 1);
    CHECK(read_tags[0].name == t.name);
}

TEST_CASE("read_all_tags", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto written_tags = generate_unique_synthetic_tags(3);
    BOOST_LOG_SEV(lg, debug) << "Written tags: " << written_tags;

    tag_repository repo;
    repo.write(h.context(), written_tags);

    auto read_tags = repo.read_all(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read tags: " << read_tags;

    CHECK(!read_tags.empty());
    CHECK(read_tags.size() >= written_tags.size());
}

TEST_CASE("read_nonexistent_tag_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    tag_repository repo;

    const std::string nonexistent_id = "00000000-0000-0000-0000-000000000000";
    BOOST_LOG_SEV(lg, debug) << "Non-existent ID: " << nonexistent_id;

    auto read_tags = repo.read_latest_by_id(h.context(), nonexistent_id);
    BOOST_LOG_SEV(lg, debug) << "Read tags: " << read_tags;

    CHECK(read_tags.size() == 0);
}
