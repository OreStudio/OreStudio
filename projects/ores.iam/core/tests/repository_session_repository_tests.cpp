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
#include "ores.iam.api/messaging/session_protocol.hpp"
#include "ores.iam.core/repository/session_repository.hpp"
#include "ores.iam.core/service/session_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>

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

/*
 * An account's page reads that account's sessions and no other, newest first:
 * the list filters on the account and is ordered by the start time.
 */
TEST_CASE("list_sessions_reads_one_accounts_sessions_newest_first", tags) {
    database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);
    session_repository repo;

    const auto now = std::chrono::system_clock::now();
    auto older = generate_synthetic_session(gen_ctx);
    older.start_time = now - std::chrono::hours(2);
    auto newer = generate_synthetic_session(gen_ctx);
    newer.account_id = older.account_id;
    newer.start_time = now - std::chrono::hours(1);
    auto other = generate_synthetic_session(gen_ctx);
    other.account_id = boost::uuids::random_generator()();
    other.start_time = now;
    repo.write(h.context(), older);
    repo.write(h.context(), newer);
    repo.write(h.context(), other);

    ores::iam::messaging::list_sessions_request request;
    request.offset = 0;
    request.limit = 10;
    request.order = {.field = "start_time", .descending = true};
    request.filter = ores::iam::messaging::sessions_filter{.account_id = older.account_id};

    ores::iam::service::session_service svc(h.context());
    const auto answer = svc.list_sessions(request);

    REQUIRE(answer.sessions.size() == 2);
    CHECK(answer.total == 2);
    CHECK(answer.sessions[0].id == newer.id);
    CHECK(answer.sessions[1].id == older.id);
}

TEST_CASE("list_sessions_reads_the_sessions_of_any_account_named", tags) {
    database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);
    session_repository repo;

    auto first = generate_synthetic_session(gen_ctx);
    auto second = generate_synthetic_session(gen_ctx);
    second.account_id = boost::uuids::random_generator()();
    auto unnamed = generate_synthetic_session(gen_ctx);
    unnamed.account_id = boost::uuids::random_generator()();
    repo.write(h.context(), first);
    repo.write(h.context(), second);
    repo.write(h.context(), unnamed);

    ores::iam::messaging::list_sessions_request request;
    request.limit = 10;
    request.filter = ores::iam::messaging::sessions_filter{
        .account_id_one_of = std::vector{first.account_id, second.account_id}};

    ores::iam::service::session_service svc(h.context());
    const auto answer = svc.list_sessions(request);

    REQUIRE(answer.sessions.size() == 2);
    for (const auto& row : answer.sessions)
        CHECK(row.account_id != unnamed.account_id);
}

TEST_CASE("list_sessions_refuses_a_filter_naming_more_than_1000_accounts", tags) {
    database_helper h;

    ores::iam::messaging::list_sessions_request request;
    request.filter = ores::iam::messaging::sessions_filter{
        .account_id_one_of = std::vector<boost::uuids::uuid>(1001)};

    ores::iam::service::session_service svc(h.context());
    const auto answer = svc.list_sessions(request);

    CHECK(answer.result.outcome == ores::utility::domain::outcome::invalid);
    CHECK(answer.result.code == "filter_too_large");
    CHECK(answer.sessions.empty());
}
