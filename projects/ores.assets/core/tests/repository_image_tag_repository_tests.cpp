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
#include "ores.assets.api/domain/image.hpp"             // IWYU pragma: keep.
#include "ores.assets.api/domain/image_json_io.hpp"     // IWYU pragma: keep.
#include "ores.assets.api/domain/image_tag.hpp"         // IWYU pragma: keep.
#include "ores.assets.api/domain/image_tag_json_io.hpp" // IWYU pragma: keep.
#include "ores.assets.api/domain/tag.hpp"               // IWYU pragma: keep.
#include "ores.assets.api/domain/tag_json_io.hpp"       // IWYU pragma: keep.
#include "ores.assets.api/generators/image_generator.hpp"
#include "ores.assets.api/generators/image_tag_generator.hpp"
#include "ores.assets.api/generators/tag_generator.hpp"
#include "ores.assets.core/repository/image_repository.hpp"
#include "ores.assets.core/repository/image_tag_repository.hpp"
#include "ores.assets.core/repository/tag_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.

namespace {

const std::string_view test_suite("ores.assets.tests");
const std::string tags("[repository]");

}

using namespace ores::assets::generators;
using ores::assets::domain::image_tag;
using ores::assets::repository::image_repository;
using ores::assets::repository::image_tag_repository;
using ores::assets::repository::tag_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_image_tag", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    image_repository image_repo;
    tag_repository tag_repo;
    image_tag_repository repo(h.context());

    // A junction row names two live rows: the insert trigger refuses a pair
    // whose halves do not exist, so the image and the tag are written first.
    auto img = generate_synthetic_image(ctx);
    auto t = generate_synthetic_tag(ctx);
    image_repo.write(h.context(), img);
    tag_repo.write(h.context(), t);

    auto it = generate_synthetic_image_tag(ctx);
    it.image_id = img.id;
    it.tag_id = t.id;
    BOOST_LOG_SEV(lg, debug) << "Image-tag: " << it;

    CHECK_NOTHROW(repo.write(it));
}

TEST_CASE("write_multiple_image_tags", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    image_repository image_repo;
    tag_repository tag_repo;
    image_tag_repository repo(h.context());

    auto img = generate_synthetic_image(ctx);
    auto t1 = generate_synthetic_tag(ctx);
    auto t2 = generate_synthetic_tag(ctx);
    image_repo.write(h.context(), img);
    tag_repo.write(h.context(), t1);
    tag_repo.write(h.context(), t2);

    auto it1 = generate_synthetic_image_tag(ctx);
    it1.image_id = img.id;
    it1.tag_id = t1.id;
    auto it2 = generate_synthetic_image_tag(ctx);
    it2.image_id = img.id;
    it2.tag_id = t2.id;
    BOOST_LOG_SEV(lg, debug) << "Image-tags: " << std::vector<image_tag>{it1, it2};

    CHECK_NOTHROW(repo.write(std::vector<image_tag>{it1, it2}));
}

TEST_CASE("read_latest_image_tags", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    image_repository image_repo;
    tag_repository tag_repo;
    image_tag_repository repo(h.context());

    auto img = generate_synthetic_image(ctx);
    auto t = generate_synthetic_tag(ctx);
    image_repo.write(h.context(), img);
    tag_repo.write(h.context(), t);

    auto it = generate_synthetic_image_tag(ctx);
    it.image_id = img.id;
    it.tag_id = t.id;
    repo.write(it);

    auto read_image_tags = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read image-tags: " << read_image_tags;

    auto found = std::ranges::find_if(read_image_tags, [&it](const image_tag& r) {
        return r.image_id == it.image_id && r.tag_id == it.tag_id;
    });
    REQUIRE(found != read_image_tags.end());
    CHECK(found->version == 1);
    CHECK(found->assigned_by == it.assigned_by);
}

TEST_CASE("read_latest_image_tags_by_image", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    image_repository image_repo;
    tag_repository tag_repo;
    image_tag_repository repo(h.context());

    auto img = generate_synthetic_image(ctx);
    auto t = generate_synthetic_tag(ctx);
    image_repo.write(h.context(), img);
    tag_repo.write(h.context(), t);

    auto it = generate_synthetic_image_tag(ctx);
    it.image_id = img.id;
    it.tag_id = t.id;
    repo.write(it);

    auto read_image_tags = repo.read_latest_by_image(it.image_id);
    BOOST_LOG_SEV(lg, debug) << "Read image-tags: " << read_image_tags;

    REQUIRE(read_image_tags.size() == 1);
    CHECK(read_image_tags[0].image_id == it.image_id);
    CHECK(read_image_tags[0].tag_id == it.tag_id);
}

TEST_CASE("read_latest_image_tags_by_tag", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    image_repository image_repo;
    tag_repository tag_repo;
    image_tag_repository repo(h.context());

    auto img = generate_synthetic_image(ctx);
    auto t = generate_synthetic_tag(ctx);
    image_repo.write(h.context(), img);
    tag_repo.write(h.context(), t);

    auto it = generate_synthetic_image_tag(ctx);
    it.image_id = img.id;
    it.tag_id = t.id;
    repo.write(it);

    auto read_image_tags = repo.read_latest_by_tag(it.tag_id);
    BOOST_LOG_SEV(lg, debug) << "Read image-tags: " << read_image_tags;

    REQUIRE(read_image_tags.size() == 1);
    CHECK(read_image_tags[0].tag_id == it.tag_id);
    CHECK(read_image_tags[0].image_id == it.image_id);
}

TEST_CASE("read_nonexistent_image_tag", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    image_tag_repository repo(h.context());

    boost::uuids::random_generator gen;
    const auto nonexistent_image_id = gen();
    BOOST_LOG_SEV(lg, debug) << "Non-existent image ID: " << nonexistent_image_id;

    auto read_image_tags = repo.read_latest_by_image(nonexistent_image_id);
    BOOST_LOG_SEV(lg, debug) << "Read image-tags: " << read_image_tags;

    CHECK(read_image_tags.empty());
}

TEST_CASE("update_image_tag", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    image_repository image_repo;
    tag_repository tag_repo;
    image_tag_repository repo(h.context());

    auto img = generate_synthetic_image(ctx);
    auto t = generate_synthetic_tag(ctx);
    image_repo.write(h.context(), img);
    tag_repo.write(h.context(), t);

    auto it = generate_synthetic_image_tag(ctx);
    it.image_id = img.id;
    it.tag_id = t.id;
    repo.write(it);

    // Re-writing the same association replaces it, so the version advances and
    // the new row carries the actor the second write stated.
    auto updated_it = it;
    updated_it.assigned_by = "new_user";
    BOOST_LOG_SEV(lg, debug) << "Updated image-tag: " << updated_it;

    CHECK_NOTHROW(repo.write(updated_it));

    auto read_image_tags = repo.read_latest(it.image_id, it.tag_id);
    BOOST_LOG_SEV(lg, debug) << "Read image-tags: " << read_image_tags;

    REQUIRE(read_image_tags.size() == 1);
    CHECK(read_image_tags[0].version == 2);
    CHECK(read_image_tags[0].assigned_by == "new_user");
}
