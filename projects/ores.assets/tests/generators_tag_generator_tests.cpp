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
#include "ores.assets/domain/tag.hpp" // IWYU pragma: keep.
#include "ores.assets/domain/tag_json_io.hpp" // IWYU pragma: keep.
#include "ores.assets/generators/tag_generator.hpp"

namespace {

const std::string_view test_suite("ores.assets.tests");
const std::string tags("[generators]");
const std::string test_tenant_id("00000000-0000-0000-0000-000000000001");

}

using namespace ores::assets::generators;
using namespace ores::logging;

TEST_CASE("generate_single_tag", tags) {
    auto lg(make_logger(test_suite));

    auto tag = generate_synthetic_tag(test_tenant_id);
    BOOST_LOG_SEV(lg, debug) << "Generated tag: " << tag;

    CHECK(!tag.tag_id.empty());
    CHECK(!tag.name.empty());
    CHECK(!tag.description.empty());
    CHECK(!tag.modified_by.empty());
    CHECK(!tag.recorded_at.empty());
    CHECK(tag.tenant_id == test_tenant_id);
}

TEST_CASE("generate_multiple_tags", tags) {
    auto lg(make_logger(test_suite));

    auto tags_list = generate_synthetic_tags(3, test_tenant_id);
    BOOST_LOG_SEV(lg, debug) << "Generated tags: " << tags_list;

    CHECK(tags_list.size() == 3);
    for (const auto& t : tags_list)
        CHECK(t.tenant_id == test_tenant_id);
}

TEST_CASE("generate_unique_tags", tags) {
    auto lg(make_logger(test_suite));

    auto tags_list = generate_unique_synthetic_tags(3, test_tenant_id);
    BOOST_LOG_SEV(lg, debug) << "Generated unique tags: " << tags_list;

    CHECK(tags_list.size() == 3);

    // Verify all names are unique
    std::set<std::string> names;
    for (const auto& t : tags_list) {
        names.insert(t.name);
        CHECK(t.tenant_id == test_tenant_id);
    }

    CHECK(names.size() == 3);
}
