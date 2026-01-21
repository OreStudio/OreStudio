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
#include "ores.connections/repository/server_environment_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.connections/domain/server_environment_json_io.hpp" // IWYU pragma: keep.
#include "ores.connections/generators/server_environment_generator.hpp"
#include "ores.connections/generators/folder_generator.hpp"
#include "ores.connections/repository/folder_repository.hpp"

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

TEST_CASE("write_single_server_environment", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    server_environment_repository repo(h.context());

    auto env = generate_synthetic_server_environment();
    BOOST_LOG_SEV(lg, debug) << "Server environment: " << env;

    CHECK_NOTHROW(repo.write(env));
}

TEST_CASE("write_multiple_server_environments", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    server_environment_repository repo(h.context());

    auto envs = generate_synthetic_server_environments(3);
    BOOST_LOG_SEV(lg, debug) << "Server environments: " << envs;

    CHECK_NOTHROW(repo.write(envs));
}

TEST_CASE("read_all_server_environments", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    server_environment_repository repo(h.context());

    auto written = generate_synthetic_server_environments(3);
    repo.write(written);

    auto read = repo.read_all();
    BOOST_LOG_SEV(lg, debug) << "Read server environments: " << read;

    CHECK(read.size() == written.size());
}

TEST_CASE("read_server_environment_by_id", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    server_environment_repository repo(h.context());

    auto env = generate_synthetic_server_environment();
    repo.write(env);

    auto read = repo.read_by_id(env.id);
    BOOST_LOG_SEV(lg, debug) << "Read environment: " << (read ? "found" : "not found");

    REQUIRE(read.has_value());
    CHECK(read->id == env.id);
    CHECK(read->name == env.name);
    CHECK(read->host == env.host);
}

TEST_CASE("read_server_environment_by_id_not_found", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    server_environment_repository repo(h.context());

    auto env = generate_synthetic_server_environment();
    auto read = repo.read_by_id(env.id);

    CHECK_FALSE(read.has_value());
}

TEST_CASE("read_root_server_environments", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    folder_repository folder_repo(h.context());
    server_environment_repository repo(h.context());

    // Create a folder
    auto folder = generate_synthetic_folder();
    folder_repo.write(folder);

    // Create 2 root environments and 1 in folder
    auto root1 = generate_synthetic_server_environment();
    auto root2 = generate_synthetic_server_environment();
    auto in_folder = generate_synthetic_server_environment();
    in_folder.folder_id = folder.id;

    repo.write({root1, root2, in_folder});

    auto roots = repo.read_by_folder(std::nullopt);
    BOOST_LOG_SEV(lg, debug) << "Root environments: " << roots;

    CHECK(roots.size() == 2);
}

TEST_CASE("read_server_environments_in_folder", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    folder_repository folder_repo(h.context());
    server_environment_repository repo(h.context());

    auto folder = generate_synthetic_folder();
    folder_repo.write(folder);

    auto env1 = generate_synthetic_server_environment();
    env1.folder_id = folder.id;
    auto env2 = generate_synthetic_server_environment();
    env2.folder_id = folder.id;

    repo.write({env1, env2});

    auto in_folder = repo.read_by_folder(folder.id);
    BOOST_LOG_SEV(lg, debug) << "Environments in folder: " << in_folder;

    CHECK(in_folder.size() == 2);
}

TEST_CASE("remove_server_environment", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    server_environment_repository repo(h.context());

    auto env = generate_synthetic_server_environment();
    repo.write(env);

    CHECK(repo.read_by_id(env.id).has_value());

    repo.remove(env.id);

    CHECK_FALSE(repo.read_by_id(env.id).has_value());
}

TEST_CASE("update_server_environment", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    server_environment_repository repo(h.context());

    auto env = generate_synthetic_server_environment();
    repo.write(env);

    env.name = "Updated Name";
    env.host = "updated.host.com";
    env.port = 9999;
    repo.write(env);

    auto read = repo.read_by_id(env.id);
    REQUIRE(read.has_value());
    CHECK(read->name == "Updated Name");
    CHECK(read->host == "updated.host.com");
    CHECK(read->port == 9999);
}

TEST_CASE("server_environment_with_encrypted_password", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    server_environment_repository repo(h.context());

    auto env = generate_synthetic_server_environment();
    env.encrypted_password = "base64encodedencryptedpassword==";
    repo.write(env);

    auto read = repo.read_by_id(env.id);
    REQUIRE(read.has_value());
    CHECK(read->encrypted_password == "base64encodedencryptedpassword==");
}
