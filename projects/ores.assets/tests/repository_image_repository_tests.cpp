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
#include "ores.assets/repository/image_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.assets/domain/image.hpp" // IWYU pragma: keep.
#include "ores.assets/domain/image_json_io.hpp" // IWYU pragma: keep.
#include "ores.assets/generators/image_generator.hpp"

namespace {

const std::string_view test_suite("ores.assets.tests");
const std::string database_table("production.assets_images_tbl");
const std::string tags("[repository]");

}

using namespace ores::assets::generators;
using ores::assets::domain::image;
using ores::assets::repository::image_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_image", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto img = generate_synthetic_image();
    BOOST_LOG_SEV(lg, debug) << "Image: " << img;

    image_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), img));
}

TEST_CASE("write_multiple_images", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto images = generate_unique_synthetic_images(3);
    BOOST_LOG_SEV(lg, debug) << "Images: " << images;

    image_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), images));
}

TEST_CASE("read_latest_images", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto written_images = generate_unique_synthetic_images(3);
    BOOST_LOG_SEV(lg, debug) << "Written images: " << written_images;

    image_repository repo;
    repo.write(h.context(), written_images);

    auto read_images = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read images: " << read_images;

    CHECK(!read_images.empty());
    CHECK(read_images.size() == written_images.size());
}

TEST_CASE("read_latest_image_by_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto img = generate_synthetic_image();
    const auto original_description = img.description;
    BOOST_LOG_SEV(lg, debug) << "Image: " << img;

    image_repository repo;
    repo.write(h.context(), img);

    img.description = original_description + " v2";
    repo.write(h.context(), img);

    auto read_images = repo.read_latest_by_id(h.context(), boost::uuids::to_string(img.image_id));
    BOOST_LOG_SEV(lg, debug) << "Read images: " << read_images;

    REQUIRE(read_images.size() == 1);
    CHECK(read_images[0].image_id == img.image_id);
    CHECK(read_images[0].description == original_description + " v2");
}

TEST_CASE("read_latest_image_by_key", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto img = generate_synthetic_image();
    BOOST_LOG_SEV(lg, debug) << "Image: " << img;

    image_repository repo;
    repo.write(h.context(), img);

    auto read_images = repo.read_latest_by_key(h.context(), img.key);
    BOOST_LOG_SEV(lg, debug) << "Read images: " << read_images;

    REQUIRE(read_images.size() == 1);
    CHECK(read_images[0].key == img.key);
}

TEST_CASE("read_all_images", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto written_images = generate_unique_synthetic_images(3);
    BOOST_LOG_SEV(lg, debug) << "Written images: " << written_images;

    image_repository repo;
    repo.write(h.context(), written_images);

    auto read_images = repo.read_all(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read images: " << read_images;

    CHECK(!read_images.empty());
    CHECK(read_images.size() >= written_images.size());
}

TEST_CASE("read_nonexistent_image_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    image_repository repo;

    const std::string nonexistent_id = "00000000-0000-0000-0000-000000000000";
    BOOST_LOG_SEV(lg, debug) << "Non-existent ID: " << nonexistent_id;

    auto read_images = repo.read_latest_by_id(h.context(), nonexistent_id);
    BOOST_LOG_SEV(lg, debug) << "Read images: " << read_images;

    CHECK(read_images.size() == 0);
}
