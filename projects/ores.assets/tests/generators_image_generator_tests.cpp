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
#include <set>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.assets/domain/image.hpp" // IWYU pragma: keep.
#include "ores.assets/domain/image_json_io.hpp" // IWYU pragma: keep.
#include "ores.assets/generators/image_generator.hpp"

namespace {

const std::string_view test_suite("ores.assets.tests");
const std::string tags("[generators]");

}

using namespace ores::assets::generators;
using namespace ores::logging;

TEST_CASE("generate_single_image", tags) {
    auto lg(make_logger(test_suite));

    auto image = generate_synthetic_image();
    BOOST_LOG_SEV(lg, debug) << "Generated image: " << image;

    CHECK(!image.image_id.is_nil());
    CHECK(!image.key.empty());
    CHECK(!image.description.empty());
    CHECK(!image.svg_data.empty());
    CHECK(!image.recorded_by.empty());
    CHECK(image.recorded_at != std::chrono::system_clock::time_point{});
}

TEST_CASE("generate_multiple_images", tags) {
    auto lg(make_logger(test_suite));

    auto images = generate_synthetic_images(3);
    BOOST_LOG_SEV(lg, debug) << "Generated images: " << images;

    CHECK(images.size() == 3);
}

TEST_CASE("generate_unique_images", tags) {
    auto lg(make_logger(test_suite));

    auto images = generate_unique_synthetic_images(3);
    BOOST_LOG_SEV(lg, debug) << "Generated unique images: " << images;

    CHECK(images.size() == 3);

    // Verify all keys are unique
    std::set<std::string> keys;
    for (const auto& img : images)
        keys.insert(img.key);

    CHECK(keys.size() == 3);
}
