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
#include "ores.connections/repository/environment_tag_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.utility/generation/generation_context.hpp"
#include "ores.connections/domain/environment_tag_json_io.hpp" // IWYU pragma: keep.
#include "ores.connections/generators/environment_tag_generator.hpp"
#include "ores.connections/generators/server_environment_generator.hpp"
#include "ores.connections/generators/tag_generator.hpp"
#include "ores.connections/repository/server_environment_repository.hpp"
#include "ores.connections/repository/tag_repository.hpp"

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

TEST_CASE("write_single_environment_tag", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    server_environment_repository env_repo(h.context());
    tag_repository tag_repo(h.context());
    environment_tag_repository repo(h.context());
    generation_context ctx;

    // Create environment and tag first
    auto env = generate_synthetic_server_environment(ctx);
    auto tag = generate_synthetic_tag(ctx);
    env_repo.write(env);
    tag_repo.write(tag);

    auto env_tag = generate_synthetic_environment_tag(ctx, env.id, tag.id);
    BOOST_LOG_SEV(lg, debug) << "Environment tag: " << env_tag;

    CHECK_NOTHROW(repo.write(env_tag));
}

TEST_CASE("write_multiple_environment_tags", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    server_environment_repository env_repo(h.context());
    tag_repository tag_repo(h.context());
    environment_tag_repository repo(h.context());
    generation_context ctx;

    auto env = generate_synthetic_server_environment(ctx);
    auto tags = generate_unique_synthetic_tags(3, ctx);
    env_repo.write(env);
    tag_repo.write(tags);

    std::vector<ores::connections::domain::environment_tag> env_tags;
    for (const auto& tag : tags) {
        env_tags.push_back(generate_synthetic_environment_tag(ctx, env.id, tag.id));
    }

    BOOST_LOG_SEV(lg, debug) << "Environment tags: " << env_tags;

    CHECK_NOTHROW(repo.write(env_tags));
}

TEST_CASE("read_environment_tags_by_environment", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    server_environment_repository env_repo(h.context());
    tag_repository tag_repo(h.context());
    environment_tag_repository repo(h.context());
    generation_context ctx;

    auto env = generate_synthetic_server_environment(ctx);
    auto tags = generate_unique_synthetic_tags(3, ctx);
    env_repo.write(env);
    tag_repo.write(tags);

    std::vector<ores::connections::domain::environment_tag> written;
    for (const auto& tag : tags) {
        written.push_back(generate_synthetic_environment_tag(ctx, env.id, tag.id));
    }
    repo.write(written);

    auto read = repo.read_by_environment(env.id);
    BOOST_LOG_SEV(lg, debug) << "Read environment tags: " << read;

    CHECK(read.size() == written.size());
}

TEST_CASE("read_tags_by_environment", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    server_environment_repository env_repo(h.context());
    tag_repository tag_repo(h.context());
    environment_tag_repository repo(h.context());
    generation_context ctx;

    auto env1 = generate_synthetic_server_environment(ctx);
    auto env2 = generate_synthetic_server_environment(ctx);
    auto tags = generate_unique_synthetic_tags(3, ctx);
    auto tag1 = tags[0];
    auto tag2 = tags[1];
    auto tag3 = tags[2];

    env_repo.write({env1, env2});
    tag_repo.write(tags);

    // env1 has tag1 and tag2
    repo.write(generate_synthetic_environment_tag(ctx, env1.id, tag1.id));
    repo.write(generate_synthetic_environment_tag(ctx, env1.id, tag2.id));
    // env2 has tag3
    repo.write(generate_synthetic_environment_tag(ctx, env2.id, tag3.id));

    auto env1_tags = repo.read_by_environment(env1.id);
    auto env2_tags = repo.read_by_environment(env2.id);

    BOOST_LOG_SEV(lg, debug) << "env1 tags: " << env1_tags;
    BOOST_LOG_SEV(lg, debug) << "env2 tags: " << env2_tags;

    CHECK(env1_tags.size() == 2);
    CHECK(env2_tags.size() == 1);
}

TEST_CASE("read_environments_by_tag", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    server_environment_repository env_repo(h.context());
    tag_repository tag_repo(h.context());
    environment_tag_repository repo(h.context());
    generation_context ctx;

    auto env1 = generate_synthetic_server_environment(ctx);
    auto env2 = generate_synthetic_server_environment(ctx);
    auto env3 = generate_synthetic_server_environment(ctx);
    auto tags = generate_unique_synthetic_tags(2, ctx);
    auto tag1 = tags[0];
    auto tag2 = tags[1];

    env_repo.write({env1, env2, env3});
    tag_repo.write(tags);

    // tag1 has env1 and env2
    repo.write(generate_synthetic_environment_tag(ctx, env1.id, tag1.id));
    repo.write(generate_synthetic_environment_tag(ctx, env2.id, tag1.id));
    // tag2 has env3
    repo.write(generate_synthetic_environment_tag(ctx, env3.id, tag2.id));

    auto tag1_envs = repo.read_by_tag(tag1.id);
    auto tag2_envs = repo.read_by_tag(tag2.id);

    BOOST_LOG_SEV(lg, debug) << "tag1 environments: " << tag1_envs;
    BOOST_LOG_SEV(lg, debug) << "tag2 environments: " << tag2_envs;

    CHECK(tag1_envs.size() == 2);
    CHECK(tag2_envs.size() == 1);
}

TEST_CASE("remove_environment_tag", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    server_environment_repository env_repo(h.context());
    tag_repository tag_repo(h.context());
    environment_tag_repository repo(h.context());
    generation_context ctx;

    auto env = generate_synthetic_server_environment(ctx);
    auto tag = generate_synthetic_tag(ctx);
    env_repo.write(env);
    tag_repo.write(tag);

    auto env_tag = generate_synthetic_environment_tag(ctx, env.id, tag.id);
    repo.write(env_tag);

    CHECK(repo.read_by_environment(env.id).size() == 1);

    repo.remove(env.id, tag.id);

    CHECK(repo.read_by_environment(env.id).empty());
}

TEST_CASE("remove_all_tags_from_environment", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_sqlite_context h;
    server_environment_repository env_repo(h.context());
    tag_repository tag_repo(h.context());
    environment_tag_repository repo(h.context());
    generation_context ctx;

    auto env = generate_synthetic_server_environment(ctx);
    auto tags = generate_unique_synthetic_tags(3, ctx);
    env_repo.write(env);
    tag_repo.write(tags);

    for (const auto& tag : tags) {
        repo.write(generate_synthetic_environment_tag(ctx, env.id, tag.id));
    }

    CHECK(repo.read_by_environment(env.id).size() == 3);

    repo.remove_by_environment(env.id);

    CHECK(repo.read_by_environment(env.id).empty());
}
