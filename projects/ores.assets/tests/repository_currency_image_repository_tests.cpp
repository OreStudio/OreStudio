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
#include "ores.assets/repository/currency_image_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/log/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.assets/domain/currency_image.hpp" // IWYU pragma: keep.
#include "ores.assets/domain/currency_image_json_io.hpp" // IWYU pragma: keep.
#include "ores.assets/generators/currency_image_generator.hpp"

namespace {

const std::string_view test_suite("ores.assets.tests");
const std::string database_table("oresdb.currency_images");
const std::string tags("[repository]");

}

using namespace ores::assets::generators;
using ores::assets::domain::currency_image;
using ores::assets::repository::currency_image_repository;
using ores::testing::scoped_database_helper;
using namespace ores::utility::log;

TEST_CASE("write_single_currency_image", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto ci = generate_synthetic_currency_image();
    BOOST_LOG_SEV(lg, debug) << "Currency-image: " << ci;

    currency_image_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), ci));
}

TEST_CASE("write_multiple_currency_images", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto currency_images = generate_unique_synthetic_currency_images(3);
    BOOST_LOG_SEV(lg, debug) << "Currency-images: " << currency_images;

    currency_image_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), currency_images));
}

TEST_CASE("read_latest_currency_images", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto written_currency_images = generate_unique_synthetic_currency_images(3);
    BOOST_LOG_SEV(lg, debug) << "Written currency-images: " << written_currency_images;

    currency_image_repository repo;
    repo.write(h.context(), written_currency_images);

    auto read_currency_images = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read currency-images: " << read_currency_images;

    CHECK(!read_currency_images.empty());
    CHECK(read_currency_images.size() == written_currency_images.size());
}

TEST_CASE("read_latest_currency_images_by_currency", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto ci = generate_synthetic_currency_image();
    BOOST_LOG_SEV(lg, debug) << "Currency-image: " << ci;

    currency_image_repository repo;
    repo.write(h.context(), ci);

    auto read_currency_images = repo.read_latest_by_currency(h.context(), ci.iso_code);
    BOOST_LOG_SEV(lg, debug) << "Read currency-images: " << read_currency_images;

    REQUIRE(read_currency_images.size() == 1);
    CHECK(read_currency_images[0].iso_code == ci.iso_code);
    CHECK(read_currency_images[0].image_id == ci.image_id);
}

TEST_CASE("read_latest_currency_images_by_image", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto ci = generate_synthetic_currency_image();
    BOOST_LOG_SEV(lg, debug) << "Currency-image: " << ci;

    currency_image_repository repo;
    repo.write(h.context(), ci);

    auto read_currency_images = repo.read_latest_by_image(h.context(), ci.image_id);
    BOOST_LOG_SEV(lg, debug) << "Read currency-images: " << read_currency_images;

    REQUIRE(read_currency_images.size() == 1);
    CHECK(read_currency_images[0].image_id == ci.image_id);
    CHECK(read_currency_images[0].iso_code == ci.iso_code);
}

TEST_CASE("read_nonexistent_currency_image", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    currency_image_repository repo;

    const std::string nonexistent_iso = "XXX";
    BOOST_LOG_SEV(lg, debug) << "Non-existent ISO code: " << nonexistent_iso;

    auto read_currency_images = repo.read_latest_by_currency(h.context(), nonexistent_iso);
    BOOST_LOG_SEV(lg, debug) << "Read currency-images: " << read_currency_images;

    CHECK(read_currency_images.size() == 0);
}
