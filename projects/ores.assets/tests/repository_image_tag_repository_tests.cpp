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
#include "ores.assets/repository/image_tag_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/log/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.assets/domain/image_tag.hpp" // IWYU pragma: keep.
#include "ores.assets/domain/image_tag_json_io.hpp" // IWYU pragma: keep.
#include "ores.assets/generators/image_tag_generator.hpp"

namespace {

const std::string_view test_suite("ores.assets.tests");
const std::string database_table("oresdb.image_tags");
const std::string tags("[repository]");

}

using namespace ores::assets::generators;
using ores::assets::domain::image_tag;
using ores::assets::repository::image_tag_repository;
using ores::testing::scoped_database_helper;
using namespace ores::utility::log;

TEST_CASE("write_single_image_tag", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto it = generate_synthetic_image_tag();
    BOOST_LOG_SEV(lg, debug) << "Image-tag: " << it;

    image_tag_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), it));
}

TEST_CASE("write_multiple_image_tags", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto image_tags = generate_synthetic_image_tags(3);
    BOOST_LOG_SEV(lg, debug) << "Image-tags: " << image_tags;

    image_tag_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), image_tags));
}

TEST_CASE("read_latest_image_tags", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto written_image_tags = generate_synthetic_image_tags(3);
    BOOST_LOG_SEV(lg, debug) << "Written image-tags: " << written_image_tags;

    image_tag_repository repo;
    repo.write(h.context(), written_image_tags);

    auto read_image_tags = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read image-tags: " << read_image_tags;

    CHECK(!read_image_tags.empty());
    CHECK(read_image_tags.size() == written_image_tags.size());
}

TEST_CASE("read_latest_image_tags_by_image", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto it = generate_synthetic_image_tag();
    BOOST_LOG_SEV(lg, debug) << "Image-tag: " << it;

    image_tag_repository repo;
    repo.write(h.context(), it);

    auto read_image_tags = repo.read_latest_by_image(h.context(), it.image_id);
    BOOST_LOG_SEV(lg, debug) << "Read image-tags: " << read_image_tags;

    REQUIRE(read_image_tags.size() == 1);
    CHECK(read_image_tags[0].image_id == it.image_id);
    CHECK(read_image_tags[0].tag_id == it.tag_id);
}

TEST_CASE("read_latest_image_tags_by_tag", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto it = generate_synthetic_image_tag();
    BOOST_LOG_SEV(lg, debug) << "Image-tag: " << it;

    image_tag_repository repo;
    repo.write(h.context(), it);

    auto read_image_tags = repo.read_latest_by_tag(h.context(), it.tag_id);
    BOOST_LOG_SEV(lg, debug) << "Read image-tags: " << read_image_tags;

    REQUIRE(read_image_tags.size() == 1);
    CHECK(read_image_tags[0].tag_id == it.tag_id);
    CHECK(read_image_tags[0].image_id == it.image_id);
}

TEST_CASE("read_nonexistent_image_tag", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    image_tag_repository repo;

    const std::string nonexistent_id = "00000000-0000-0000-0000-000000000000";
    BOOST_LOG_SEV(lg, debug) << "Non-existent ID: " << nonexistent_id;

    auto read_image_tags = repo.read_latest_by_image(h.context(), nonexistent_id);
    BOOST_LOG_SEV(lg, debug) << "Read image-tags: " << read_image_tags;

    CHECK(read_image_tags.size() == 0);
}
