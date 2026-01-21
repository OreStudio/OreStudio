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
#include "ores.connections/repository/tag_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.connections/domain/tag_json_io.hpp" // IWYU pragma: keep.
#include "ores.connections/generators/tag_generator.hpp"

namespace {

const std::string test_suite("ores.connections.tests");
const std::string tags("[repository]");

class scoped_sqlite_context {
public:
    scoped_sqlite_context()
        : db_path_(std::filesystem::temp_directory_path() /
                   ("ores_test_" + std::to_string(std::rand()) + ".db")),
          ctx_(db_path_) {
        ctx_.initialize_schema();
    }

    ~scoped_sqlite_context() {
        std::filesystem::remove(db_path_);
    }

    ores::connections::repository::sqlite_context& context() { return ctx_; }

private:
    std::filesystem::path db_path_;
    ores::connections::repository::sqlite_context ctx_;
};

}

using namespace ores::connections::generators;
using namespace ores::connections::repository;
using namespace ores::logging;

TEST_CASE("write_single_tag", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    tag_repository repo(h.context());

    auto tag = generate_synthetic_tag();
    BOOST_LOG_SEV(lg, debug) << "Tag: " << tag;

    CHECK_NOTHROW(repo.write(tag));
}

TEST_CASE("write_multiple_tags", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    tag_repository repo(h.context());

    auto tags = generate_unique_synthetic_tags(3);
    BOOST_LOG_SEV(lg, debug) << "Tags: " << tags;

    CHECK_NOTHROW(repo.write(tags));
}

TEST_CASE("read_all_tags", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    tag_repository repo(h.context());

    auto written = generate_unique_synthetic_tags(3);
    repo.write(written);

    auto read = repo.read_all();
    BOOST_LOG_SEV(lg, debug) << "Read tags: " << read;

    CHECK(read.size() == written.size());
}

TEST_CASE("read_tag_by_id", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    tag_repository repo(h.context());

    auto tag = generate_synthetic_tag();
    repo.write(tag);

    auto read = repo.read_by_id(tag.id);
    BOOST_LOG_SEV(lg, debug) << "Read tag: " << (read ? "found" : "not found");

    REQUIRE(read.has_value());
    CHECK(read->id == tag.id);
    CHECK(read->name == tag.name);
}

TEST_CASE("read_tag_by_id_not_found", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    tag_repository repo(h.context());

    auto tag = generate_synthetic_tag();
    auto read = repo.read_by_id(tag.id);

    CHECK_FALSE(read.has_value());
}

TEST_CASE("read_tag_by_name", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    tag_repository repo(h.context());

    auto tag = generate_synthetic_tag();
    tag.name = "UniqueTestTag";
    repo.write(tag);

    auto read = repo.read_by_name("UniqueTestTag");
    BOOST_LOG_SEV(lg, debug) << "Read tag by name: " << (read ? "found" : "not found");

    REQUIRE(read.has_value());
    CHECK(read->id == tag.id);
    CHECK(read->name == "UniqueTestTag");
}

TEST_CASE("read_tag_by_name_not_found", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    tag_repository repo(h.context());

    auto read = repo.read_by_name("NonExistentTag");

    CHECK_FALSE(read.has_value());
}

TEST_CASE("remove_tag", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    tag_repository repo(h.context());

    auto tag = generate_synthetic_tag();
    repo.write(tag);

    CHECK(repo.read_by_id(tag.id).has_value());

    repo.remove(tag.id);

    CHECK_FALSE(repo.read_by_id(tag.id).has_value());
}

TEST_CASE("update_tag", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    tag_repository repo(h.context());

    auto tag = generate_synthetic_tag();
    repo.write(tag);

    tag.name = "UpdatedTagName";
    repo.write(tag);

    auto read = repo.read_by_id(tag.id);
    REQUIRE(read.has_value());
    CHECK(read->name == "UpdatedTagName");
}
