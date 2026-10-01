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
#include "ores.iam.api/domain/session.hpp"
#include "ores.iam.api/domain/session_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.api/generators/session_generator.hpp"
#include "ores.iam.core/repository/session_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::iam::generators;

using ores::testing::database_helper;
using ores::iam::repository::session_repository;

TEST_CASE("create_single_session", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);

    session_repository repo;
    auto s = generate_synthetic_session(gen_ctx);

    BOOST_LOG_SEV(lg, debug) << "Session: " << s;
    CHECK_NOTHROW(repo.write(h.context(), s));
}

TEST_CASE("read_active_returns_the_sessions_that_have_not_ended", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);

    session_repository repo;
    auto open = generate_synthetic_session(gen_ctx);
    open.end_time.clear();
    repo.write(h.context(), open);

    auto ended = generate_synthetic_session(gen_ctx);
    ended.end_time = "2026-09-30 09:00:00Z";
    repo.write(h.context(), ended);

    const auto active = repo.read_active(h.context());

    // Other cases in this suite write sessions into the same tenant, so what is
    // asserted is what the read includes and leaves out, not how many rows it
    // answers with.
    bool saw_open = false;
    bool saw_ended = false;
    for (const auto& s : active) {
        saw_open = saw_open || s.id == open.id;
        saw_ended = saw_ended || s.id == ended.id;
    }
    CHECK(saw_open);
    CHECK_FALSE(saw_ended);
}

TEST_CASE("read_session_by_id", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);

    session_repository repo;
    auto s = generate_synthetic_session(gen_ctx);
    const auto target_id = s.id;

    BOOST_LOG_SEV(lg, debug) << "Session: " << s;
    repo.write(h.context(), s);

    BOOST_LOG_SEV(lg, debug) << "Target ID: " << target_id;

    auto read_session = repo.read(h.context(), target_id);
    BOOST_LOG_SEV(lg, debug) << "Read session has value: " << read_session.has_value();

    REQUIRE(read_session.has_value());
    CHECK(read_session->id == target_id);
    CHECK(read_session->account_id == s.account_id);
}

TEST_CASE("read_nonexistent_session", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    session_repository repo;

    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent ID: " << nonexistent_id;

    auto read_session = repo.read(h.context(), nonexistent_id);
    BOOST_LOG_SEV(lg, debug) << "Read session has value: " << read_session.has_value();

    CHECK(!read_session.has_value());
}
