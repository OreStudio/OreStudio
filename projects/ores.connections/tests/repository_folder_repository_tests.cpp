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
#include "ores.connections/repository/folder_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.utility/generation/generation_context.hpp"
#include "ores.connections/domain/folder_json_io.hpp" // IWYU pragma: keep.
#include "ores.connections/generators/folder_generator.hpp"

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
using ores::utility::generation::generation_context;

TEST_CASE("write_single_folder", tags) {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    folder_repository repo(h.context());
    generation_context ctx;

    auto folder = generate_synthetic_folder(ctx);
    BOOST_LOG_SEV(lg, debug) << "Folder: " << folder;

    CHECK_NOTHROW(repo.write(folder));
}

TEST_CASE("write_multiple_folders", tags) {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    folder_repository repo(h.context());
    generation_context ctx;

    auto folders = generate_synthetic_folders(3, ctx);
    BOOST_LOG_SEV(lg, debug) << "Folders: " << folders;

    CHECK_NOTHROW(repo.write(folders));
}

TEST_CASE("read_all_folders", tags) {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    folder_repository repo(h.context());
    generation_context ctx;

    auto written = generate_synthetic_folders(3, ctx);
    repo.write(written);

    auto read = repo.read_all();
    BOOST_LOG_SEV(lg, debug) << "Read folders: " << read;

    CHECK(read.size() == written.size());
}

TEST_CASE("read_folder_by_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    folder_repository repo(h.context());
    generation_context ctx;

    auto folder = generate_synthetic_folder(ctx);
    repo.write(folder);

    auto read = repo.read_by_id(folder.id);
    BOOST_LOG_SEV(lg, debug) << "Read folder: " << (read ? "found" : "not found");

    REQUIRE(read.has_value());
    CHECK(read->id == folder.id);
    CHECK(read->name == folder.name);
}

TEST_CASE("read_folder_by_id_not_found", tags) {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    folder_repository repo(h.context());
    generation_context ctx;

    auto folder = generate_synthetic_folder(ctx);
    auto read = repo.read_by_id(folder.id);

    CHECK_FALSE(read.has_value());
}

TEST_CASE("read_root_folders", tags) {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    folder_repository repo(h.context());
    generation_context ctx;

    // Create 2 root folders and 1 child folder
    auto root1 = generate_synthetic_folder(ctx);
    auto root2 = generate_synthetic_folder(ctx);
    auto child = generate_synthetic_folder(ctx);
    child.parent_id = root1.id;

    repo.write({root1, root2, child});

    auto roots = repo.read_by_parent(std::nullopt);
    BOOST_LOG_SEV(lg, debug) << "Root folders: " << roots;

    CHECK(roots.size() == 2);
}

TEST_CASE("read_child_folders", tags) {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    folder_repository repo(h.context());
    generation_context ctx;

    auto parent = generate_synthetic_folder(ctx);
    auto child1 = generate_synthetic_folder(ctx);
    child1.parent_id = parent.id;
    auto child2 = generate_synthetic_folder(ctx);
    child2.parent_id = parent.id;

    repo.write({parent, child1, child2});

    auto children = repo.read_by_parent(parent.id);
    BOOST_LOG_SEV(lg, debug) << "Child folders: " << children;

    CHECK(children.size() == 2);
}

TEST_CASE("remove_folder", tags) {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    folder_repository repo(h.context());
    generation_context ctx;

    auto folder = generate_synthetic_folder(ctx);
    repo.write(folder);

    CHECK(repo.read_by_id(folder.id).has_value());

    repo.remove(folder.id);

    CHECK_FALSE(repo.read_by_id(folder.id).has_value());
}

TEST_CASE("update_folder", tags) {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    folder_repository repo(h.context());
    generation_context ctx;

    auto folder = generate_synthetic_folder(ctx);
    repo.write(folder);

    folder.name = "Updated Name";
    repo.write(folder);

    auto read = repo.read_by_id(folder.id);
    REQUIRE(read.has_value());
    CHECK(read->name == "Updated Name");
}
