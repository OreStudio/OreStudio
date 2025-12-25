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
#include "ores.assets/domain/image_tag.hpp" // IWYU pragma: keep.
#include "ores.assets/domain/image_tag_json_io.hpp" // IWYU pragma: keep.
#include "ores.assets/generators/image_tag_generator.hpp"

namespace {

const std::string_view test_suite("ores.assets.tests");
const std::string tags("[generators]");

}

using namespace ores::assets::generators;
using namespace ores::telemetry::log;

TEST_CASE("generate_single_image_tag", tags) {
    auto lg(make_logger(test_suite));

    auto image_tag = generate_synthetic_image_tag();
    BOOST_LOG_SEV(lg, debug) << "Generated image_tag: " << image_tag;

    CHECK(!image_tag.image_id.empty());
    CHECK(!image_tag.tag_id.empty());
    CHECK(!image_tag.assigned_by.empty());
    CHECK(!image_tag.assigned_at.empty());
}

TEST_CASE("generate_image_tag_with_params", tags) {
    auto lg(make_logger(test_suite));

    auto image_tag = generate_synthetic_image_tag("test-image-id", "test-tag-id");
    BOOST_LOG_SEV(lg, debug) << "Generated image_tag with params: " << image_tag;

    CHECK(image_tag.image_id == "test-image-id");
    CHECK(image_tag.tag_id == "test-tag-id");
    CHECK(!image_tag.assigned_by.empty());
    CHECK(!image_tag.assigned_at.empty());
}

TEST_CASE("generate_multiple_image_tags", tags) {
    auto lg(make_logger(test_suite));

    auto image_tags = generate_synthetic_image_tags(3);
    BOOST_LOG_SEV(lg, debug) << "Generated image_tags: " << image_tags;

    CHECK(image_tags.size() == 3);
}

