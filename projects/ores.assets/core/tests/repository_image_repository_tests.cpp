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
#include "ores.assets.api/domain/image.hpp"         // IWYU pragma: keep.
#include "ores.assets.api/domain/image_json_io.hpp" // IWYU pragma: keep.
#include "ores.assets.api/generators/image_generator.hpp"
#include "ores.assets.core/repository/image_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.

namespace {

const std::string_view test_suite("ores.assets.tests");
const std::string tags("[repository]");

}

using namespace ores::assets::generators;
using ores::assets::domain::image;
using ores::assets::repository::image_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_image", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto img = generate_synthetic_image(ctx);
    BOOST_LOG_SEV(lg, debug) << "Image: " << img;

    image_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), img));
}

TEST_CASE("write_multiple_images", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto images = generate_synthetic_images(3, ctx);
    BOOST_LOG_SEV(lg, debug) << "Images: " << images;

    image_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), images));
}

TEST_CASE("read_latest_images", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto written_images = generate_synthetic_images(3, ctx);
    BOOST_LOG_SEV(lg, debug) << "Written images: " << written_images;

    image_repository repo;
    repo.write(h.context(), written_images);

    auto read_images = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read images: " << read_images;

    // Every written image is found by the primary key it was written under.
    CHECK(read_images.size() >= written_images.size());
    for (const auto& written : written_images) {
        auto it = std::ranges::find_if(read_images,
                                       [&written](const image& i) { return i.id == written.id; });
        CHECK(it != read_images.end());
    }
}

TEST_CASE("read_latest_image_by_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto img = generate_synthetic_image(ctx);
    const auto original_description = img.description;
    BOOST_LOG_SEV(lg, debug) << "Image: " << img;

    image_repository repo;
    repo.write(h.context(), img);

    img.description = original_description + " v2";
    repo.write(h.context(), img);

    auto read_images = repo.read_latest(h.context(), boost::uuids::to_string(img.id));
    BOOST_LOG_SEV(lg, debug) << "Read images: " << read_images;

    REQUIRE(read_images.size() == 1);
    CHECK(read_images[0].id == img.id);
    CHECK(read_images[0].description == original_description + " v2");
    CHECK(read_images[0].mime_type == img.mime_type);
    CHECK(read_images[0].data == img.data);
}

TEST_CASE("write_and_read_image_preserves_every_byte", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto img = generate_synthetic_image(ctx);
    // Bytes chosen so the base64 hop must carry a zero byte, a high byte and a
    // length whose encoding needs padding. A text-only or lossy mapper
    // corrupts one of them, and the read below catches it: the bytes that come
    // back are equal to the bytes that went in.
    img.data = std::vector<std::uint8_t>{0x00, 0xFF, 0x10, 0x80, 0x01, 0xFE, 0x7F};
    BOOST_LOG_SEV(lg, debug) << "Image: " << img;

    image_repository repo;
    repo.write(h.context(), img);

    auto read_images = repo.read_latest(h.context(), boost::uuids::to_string(img.id));
    BOOST_LOG_SEV(lg, debug) << "Read images: " << read_images;

    REQUIRE(read_images.size() == 1);
    CHECK(read_images[0].data == img.data);
}

TEST_CASE("read_latest_image_by_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto img = generate_synthetic_image(ctx);
    BOOST_LOG_SEV(lg, debug) << "Image: " << img;

    image_repository repo;
    repo.write(h.context(), img);

    auto read_images = repo.read_latest_by_code(h.context(), img.code);
    BOOST_LOG_SEV(lg, debug) << "Read images: " << read_images;

    REQUIRE(read_images.size() == 1);
    CHECK(read_images[0].code == img.code);
    CHECK(read_images[0].id == img.id);
}

TEST_CASE("read_all_image_versions", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto img = generate_synthetic_image(ctx);
    const auto original_description = img.description;
    BOOST_LOG_SEV(lg, debug) << "Image: " << img;

    image_repository repo;
    repo.write(h.context(), img);

    img.description = original_description + " v2";
    repo.write(h.context(), img);

    // read_all addresses one row by its primary key and returns every version,
    // newest first, including the version the second write closed.
    auto read_images = repo.read_all(h.context(), boost::uuids::to_string(img.id));
    BOOST_LOG_SEV(lg, debug) << "Read images: " << read_images;

    REQUIRE(read_images.size() == 2);
    CHECK(read_images[0].version == 2);
    CHECK(read_images[0].description == original_description + " v2");
    CHECK(read_images[1].version == 1);
    CHECK(read_images[1].description == original_description);
}

TEST_CASE("read_nonexistent_image_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    image_repository repo;

    const std::string nonexistent_id = "00000000-0000-0000-0000-000000000000";
    BOOST_LOG_SEV(lg, debug) << "Non-existent ID: " << nonexistent_id;

    auto read_images = repo.read_latest(h.context(), nonexistent_id);
    BOOST_LOG_SEV(lg, debug) << "Read images: " << read_images;

    CHECK(read_images.size() == 0);
}
