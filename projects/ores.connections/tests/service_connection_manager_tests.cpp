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
#include "ores.connections/service/connection_manager.hpp"

#include <filesystem>
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.connections/domain/folder_json_io.hpp" // IWYU pragma: keep.
#include "ores.connections/domain/tag_json_io.hpp" // IWYU pragma: keep.
#include "ores.connections/domain/server_environment_json_io.hpp" // IWYU pragma: keep.
#include "ores.connections/generators/folder_generator.hpp"
#include "ores.connections/generators/tag_generator.hpp"
#include "ores.connections/generators/server_environment_generator.hpp"

namespace {

const std::string test_suite("ores.connections.tests");
const std::string tags("[service]");

class scoped_connection_manager {
public:
    scoped_connection_manager(const std::string& master_password = "test_master_password")
        : db_path_(std::filesystem::temp_directory_path() /
                   ("ores_test_" + std::to_string(std::rand()) + ".db")),
          manager_(db_path_, master_password),
          master_password_(master_password) {}

    ~scoped_connection_manager() {
        std::filesystem::remove(db_path_);
    }

    ores::connections::service::connection_manager& manager() { return manager_; }
    const std::string& master_password() const { return master_password_; }
    const std::filesystem::path& db_path() const { return db_path_; }

private:
    std::filesystem::path db_path_;
    ores::connections::service::connection_manager manager_;
    std::string master_password_;
};

}

using namespace ores::connections::generators;
using namespace ores::connections::service;
using namespace ores::logging;

TEST_CASE("create_and_get_folder", "[service]") {
    auto lg(make_logger(test_suite));

    scoped_connection_manager h;
    auto& mgr = h.manager();

    auto folder = generate_synthetic_folder();
    BOOST_LOG_SEV(lg, debug) << "Creating folder: " << folder;

    mgr.create_folder(folder);

    auto read = mgr.get_folder(folder.id);
    REQUIRE(read.has_value());
    CHECK(read->id == folder.id);
    CHECK(read->name == folder.name);
}

TEST_CASE("get_root_folders", "[service]") {
    auto lg(make_logger(test_suite));

    scoped_connection_manager h;
    auto& mgr = h.manager();

    auto root1 = generate_synthetic_folder();
    auto root2 = generate_synthetic_folder();
    auto child = generate_synthetic_folder();
    child.parent_id = root1.id;

    mgr.create_folder(root1);
    mgr.create_folder(root2);
    mgr.create_folder(child);

    auto roots = mgr.get_root_folders();
    BOOST_LOG_SEV(lg, debug) << "Root folders: " << roots;

    CHECK(roots.size() == 2);
}

TEST_CASE("create_and_get_tag", "[service]") {
    auto lg(make_logger(test_suite));

    scoped_connection_manager h;
    auto& mgr = h.manager();

    auto tag = generate_synthetic_tag();
    BOOST_LOG_SEV(lg, debug) << "Creating tag: " << tag;

    mgr.create_tag(tag);

    auto read = mgr.get_tag(tag.id);
    REQUIRE(read.has_value());
    CHECK(read->id == tag.id);
    CHECK(read->name == tag.name);
}

TEST_CASE("get_tag_by_name", "[service]") {
    auto lg(make_logger(test_suite));

    scoped_connection_manager h;
    auto& mgr = h.manager();

    auto tag = generate_synthetic_tag();
    tag.name = "ProductionTag";
    mgr.create_tag(tag);

    auto read = mgr.get_tag_by_name("ProductionTag");
    REQUIRE(read.has_value());
    CHECK(read->id == tag.id);
}

TEST_CASE("create_environment_with_encrypted_password", "[service]") {
    auto lg(make_logger(test_suite));

    scoped_connection_manager h;
    auto& mgr = h.manager();

    auto env = generate_synthetic_server_environment();
    const std::string password = "my_secret_password";

    BOOST_LOG_SEV(lg, debug) << "Creating environment: " << env;

    mgr.create_environment(env, password);

    auto read = mgr.get_environment(env.id);
    REQUIRE(read.has_value());
    CHECK(read->id == env.id);
    CHECK(read->name == env.name);

    // Password should be encrypted (not stored in plain text)
    CHECK(!read->encrypted_password.empty());
    CHECK(read->encrypted_password != password);
}

TEST_CASE("get_decrypted_password", "[service]") {
    auto lg(make_logger(test_suite));

    scoped_connection_manager h;
    auto& mgr = h.manager();

    auto env = generate_synthetic_server_environment();
    const std::string password = "my_secret_password_123";

    mgr.create_environment(env, password);

    auto decrypted = mgr.get_password(env.id);
    BOOST_LOG_SEV(lg, debug) << "Decrypted password: " << decrypted;

    CHECK(decrypted == password);
}

TEST_CASE("update_environment_without_changing_password", "[service]") {
    auto lg(make_logger(test_suite));

    scoped_connection_manager h;
    auto& mgr = h.manager();

    auto env = generate_synthetic_server_environment();
    const std::string password = "original_password";

    mgr.create_environment(env, password);

    env.name = "Updated Name";
    mgr.update_environment(env, std::nullopt);

    auto read = mgr.get_environment(env.id);
    REQUIRE(read.has_value());
    CHECK(read->name == "Updated Name");

    // Password should still be decryptable
    CHECK(mgr.get_password(env.id) == password);
}

TEST_CASE("update_environment_with_new_password", "[service]") {
    auto lg(make_logger(test_suite));

    scoped_connection_manager h;
    auto& mgr = h.manager();

    auto env = generate_synthetic_server_environment();
    const std::string old_password = "old_password";
    const std::string new_password = "new_password";

    mgr.create_environment(env, old_password);
    mgr.update_environment(env, new_password);

    CHECK(mgr.get_password(env.id) == new_password);
}

TEST_CASE("add_and_remove_tag_from_environment", "[service]") {
    auto lg(make_logger(test_suite));

    scoped_connection_manager h;
    auto& mgr = h.manager();

    auto env = generate_synthetic_server_environment();
    auto tag = generate_synthetic_tag();

    mgr.create_tag(tag);
    mgr.create_environment(env, "password");

    mgr.add_tag_to_environment(env.id, tag.id);

    auto env_tags = mgr.get_tags_for_environment(env.id);
    BOOST_LOG_SEV(lg, debug) << "Environment tags: " << env_tags;

    REQUIRE(env_tags.size() == 1);
    CHECK(env_tags[0].id == tag.id);

    mgr.remove_tag_from_environment(env.id, tag.id);

    env_tags = mgr.get_tags_for_environment(env.id);
    CHECK(env_tags.empty());
}

TEST_CASE("get_environments_with_tag", "[service]") {
    auto lg(make_logger(test_suite));

    scoped_connection_manager h;
    auto& mgr = h.manager();

    auto env1 = generate_synthetic_server_environment();
    auto env2 = generate_synthetic_server_environment();
    auto tag = generate_synthetic_tag();

    mgr.create_tag(tag);
    mgr.create_environment(env1, "password1");
    mgr.create_environment(env2, "password2");

    mgr.add_tag_to_environment(env1.id, tag.id);
    mgr.add_tag_to_environment(env2.id, tag.id);

    auto tagged_envs = mgr.get_environments_with_tag(tag.id);
    BOOST_LOG_SEV(lg, debug) << "Tagged environments: " << tagged_envs;

    CHECK(tagged_envs.size() == 2);
}

TEST_CASE("verify_master_password", "[service]") {
    auto lg(make_logger(test_suite));

    scoped_connection_manager h;
    auto& mgr = h.manager();

    // Empty database should verify
    CHECK(mgr.verify_master_password());

    // Create an environment with a password
    auto env = generate_synthetic_server_environment();
    mgr.create_environment(env, "test_password");

    // Should still verify with correct password
    CHECK(mgr.verify_master_password());
}

TEST_CASE("change_master_password", "[service]") {
    auto lg(make_logger(test_suite));

    const std::string old_password = "old_master";
    const std::string new_password = "new_master";
    const std::string env_password = "environment_password";

    std::filesystem::path db_path = std::filesystem::temp_directory_path() /
        ("ores_test_" + std::to_string(std::rand()) + ".db");

    {
        // Create manager with old password
        connection_manager mgr(db_path, old_password);

        auto env = generate_synthetic_server_environment();
        mgr.create_environment(env, env_password);

        // Change to new password
        mgr.change_master_password(new_password);

        // Should still be able to get password with new master
        CHECK(mgr.get_password(env.id) == env_password);
    }

    {
        // Reopen with new password
        connection_manager mgr(db_path, new_password);

        auto envs = mgr.get_all_environments();
        REQUIRE(!envs.empty());

        // Should be able to decrypt with new password
        CHECK(mgr.get_password(envs[0].id) == env_password);
    }

    std::filesystem::remove(db_path);
}

TEST_CASE("environments_in_folder", "[service]") {
    auto lg(make_logger(test_suite));

    scoped_connection_manager h;
    auto& mgr = h.manager();

    auto folder = generate_synthetic_folder();
    mgr.create_folder(folder);

    auto env1 = generate_synthetic_server_environment();
    env1.folder_id = folder.id;
    auto env2 = generate_synthetic_server_environment();
    env2.folder_id = folder.id;
    auto root_env = generate_synthetic_server_environment();

    mgr.create_environment(env1, "pass1");
    mgr.create_environment(env2, "pass2");
    mgr.create_environment(root_env, "pass3");

    auto in_folder = mgr.get_environments_in_folder(folder.id);
    auto at_root = mgr.get_environments_in_folder(std::nullopt);

    BOOST_LOG_SEV(lg, debug) << "In folder: " << in_folder;
    BOOST_LOG_SEV(lg, debug) << "At root: " << at_root;

    CHECK(in_folder.size() == 2);
    CHECK(at_root.size() == 1);
}

TEST_CASE("delete_folder", "[service]") {
    auto lg(make_logger(test_suite));

    scoped_connection_manager h;
    auto& mgr = h.manager();

    auto folder = generate_synthetic_folder();
    mgr.create_folder(folder);

    CHECK(mgr.get_folder(folder.id).has_value());

    mgr.delete_folder(folder.id);

    CHECK_FALSE(mgr.get_folder(folder.id).has_value());
}

TEST_CASE("delete_tag", "[service]") {
    auto lg(make_logger(test_suite));

    scoped_connection_manager h;
    auto& mgr = h.manager();

    auto tag = generate_synthetic_tag();
    mgr.create_tag(tag);

    CHECK(mgr.get_tag(tag.id).has_value());

    mgr.delete_tag(tag.id);

    CHECK_FALSE(mgr.get_tag(tag.id).has_value());
}

TEST_CASE("delete_environment", "[service]") {
    auto lg(make_logger(test_suite));

    scoped_connection_manager h;
    auto& mgr = h.manager();

    auto env = generate_synthetic_server_environment();
    mgr.create_environment(env, "password");

    CHECK(mgr.get_environment(env.id).has_value());

    mgr.delete_environment(env.id);

    CHECK_FALSE(mgr.get_environment(env.id).has_value());
}
