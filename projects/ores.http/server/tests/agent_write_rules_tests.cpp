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
#include "ores.http.server/app/agent_write_rules.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>

using namespace ores::http::server::app;
using ores::http::domain::http_response;

TEST_CASE("a_token_of_reads_alone_can_write_nothing", "[agent_writes]") {
    CHECK_FALSE(agent_token_can_write({"refdata::currencies:read", "iam::accounts:lookup"}));
    CHECK_FALSE(agent_token_can_write({}));
    CHECK(agent_token_can_write({"refdata::currencies:read", "refdata::currencies:write"}));
}

TEST_CASE("a_replay_applied_only_when_its_answer_says_so", "[agent_writes]") {
    std::string why;
    CHECK(agent_write_applied(http_response::json(R"({"result":{"outcome":"ok"}})"), why));
    CHECK(agent_write_applied(http_response::json(R"({"success":true})"), why));
    CHECK(agent_write_applied(http_response::json("not json"), why));

    CHECK_FALSE(agent_write_applied(
        http_response::json(R"({"result":{"outcome":"conflict","message":"stale"}})"), why));
    CHECK(why == "stale");
    CHECK_FALSE(agent_write_applied(http_response::json(R"({"success":false,"message":"no"})"), why));
    CHECK(why == "no");
    CHECK_FALSE(agent_write_applied(http_response::forbidden("refused"), why));
}
