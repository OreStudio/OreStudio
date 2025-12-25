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
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.assets/domain/currency_image.hpp" // IWYU pragma: keep.
#include "ores.assets/domain/currency_image_json_io.hpp" // IWYU pragma: keep.
#include "ores.assets/generators/currency_image_generator.hpp"

namespace {

const std::string_view test_suite("ores.assets.tests");
const std::string tags("[generators]");

}

using namespace ores::assets::generators;
using namespace ores::telemetry::log;

TEST_CASE("generate_single_currency_image", tags) {
    auto lg(make_logger(test_suite));

    auto ci = generate_synthetic_currency_image();
    BOOST_LOG_SEV(lg, debug) << "Generated currency_image: " << ci;

    CHECK(!ci.iso_code.empty());
    CHECK(!ci.image_id.empty());
    CHECK(!ci.assigned_by.empty());
    CHECK(!ci.assigned_at.empty());
}

TEST_CASE("generate_currency_image_with_params", tags) {
    auto lg(make_logger(test_suite));

    auto ci = generate_synthetic_currency_image("USD", "test-image-id");
    BOOST_LOG_SEV(lg, debug) << "Generated currency_image with params: " << ci;

    CHECK(ci.iso_code == "USD");
    CHECK(ci.image_id == "test-image-id");
    CHECK(!ci.assigned_by.empty());
    CHECK(!ci.assigned_at.empty());
}

TEST_CASE("generate_multiple_currency_images", tags) {
    auto lg(make_logger(test_suite));

    auto cis = generate_synthetic_currency_images(3);
    BOOST_LOG_SEV(lg, debug) << "Generated currency_images: " << cis;

    CHECK(cis.size() == 3);
}

TEST_CASE("generate_unique_currency_images", tags) {
    auto lg(make_logger(test_suite));

    auto cis = generate_unique_synthetic_currency_images(3);
    BOOST_LOG_SEV(lg, debug) << "Generated unique currency_images: " << cis;

    CHECK(cis.size() == 3);

    // Verify all iso_codes are unique
    std::set<std::string> codes;
    for (const auto& ci : cis)
        codes.insert(ci.iso_code);

    CHECK(codes.size() == 3);
}
