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
#include "ores.assets.api/domain/image_tag.hpp"         // IWYU pragma: keep.
#include "ores.assets.api/domain/image_tag_json_io.hpp" // IWYU pragma: keep.
#include "ores.assets.api/generators/image_tag_generator.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <catch2/catch_test_macros.hpp>
#include <set>
#include <utility>

namespace {

const std::string_view test_suite("ores.assets.tests");
const std::string tags("[generators]");

}

using namespace ores::assets::generators;
using namespace ores::logging;
using ores::utility::generation::generation_context;

TEST_CASE("generate_single_image_tag", tags) {
    auto lg(make_logger(test_suite));

    generation_context ctx;
    auto image_tag = generate_synthetic_image_tag(ctx);
    BOOST_LOG_SEV(lg, debug) << "Generated image_tag: " << image_tag;

    CHECK(!image_tag.image_id.is_nil());
    CHECK(!image_tag.tag_id.is_nil());
    CHECK(!image_tag.assigned_by.empty());
    CHECK(image_tag.assigned_at != std::chrono::system_clock::time_point{});
}

TEST_CASE("generate_multiple_image_tags", tags) {
    auto lg(make_logger(test_suite));

    generation_context ctx;
    auto image_tags = generate_synthetic_image_tags(3, ctx);
    BOOST_LOG_SEV(lg, debug) << "Generated image_tags: " << image_tags;

    CHECK(image_tags.size() == 3);

    // Each row names a distinct (image, tag) pair: that pair is the junction's
    // key, so a batch that repeated a pair would be a batch of duplicates.
    std::set<std::pair<boost::uuids::uuid, boost::uuids::uuid>> pairs;
    for (const auto& it : image_tags)
        pairs.insert({it.image_id, it.tag_id});

    CHECK(pairs.size() == 3);
}
