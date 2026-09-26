/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.shell/app/commands/scheduler_commands.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cli/cli.h>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

using ores::nats::service::nats_client;
using ores::shell::app::commands::scheduler_commands;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.shell.application.tests");
const std::string tags("[commands][scheduler]");

const std::string valid_party_id("3f2504e0-4f89-11d3-9a0c-0305e82c3301");

void log_in(nats_client& session) {
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    info.default_party_id = valid_party_id;
    session.set_auth(std::move(info));
}

/**
 * @brief Every case below refuses before the transport is touched.
 *
 * These cases never reach a server: each one states an argument or a
 * session the command rejects first, which is what makes them runnable in
 * the unit suite. The commands' happy paths are exercised against a live
 * fleet instead, because that is the only place a job can actually fire.
 */
} // anonymous namespace

TEST_CASE("scheduler_commands registers its six verbs", tags) {
    auto lg(make_logger(test_suite));

    cli::Menu root_menu("root");
    nats_client session;

    scheduler_commands::register_commands(root_menu, session);

    const auto completions = root_menu.GetCompletions("scheduler ");
    for (const auto& verb : {std::string{"scheduler jobs"},
                             std::string{"scheduler schedule"},
                             std::string{"scheduler remove"},
                             std::string{"scheduler instances"},
                             std::string{"scheduler status"},
                             std::string{"scheduler watch"}})
        CHECK(std::find(completions.begin(), completions.end(), verb) != completions.end());
}

TEST_CASE("scheduler lists jobs only for a session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    scheduler_commands::process_jobs(out, session);

    CHECK(out.str().find("must be logged in") != std::string::npos);
}

TEST_CASE("scheduler refuses a malformed cron before it sends anything", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    log_in(session);
    std::ostringstream out;

    scheduler_commands::process_schedule(out, session, {"nightly", "not a cron"});

    CHECK(out.str().find("is not a cron expression") != std::string::npos);
}

TEST_CASE("scheduler schedule states its usage when the name is missing", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    log_in(session);
    std::ostringstream out;

    scheduler_commands::process_schedule(out, session, {"only-a-name"});

    CHECK(out.str().find("Usage: scheduler schedule") != std::string::npos);
}

TEST_CASE("scheduler watch refuses a duration outside its range", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    scheduler_commands::process_watch(out, session, {"--seconds", "0"});
    CHECK(out.str().find("must be between 1 and") != std::string::npos);

    std::ostringstream too_long;
    scheduler_commands::process_watch(too_long, session, {"--seconds", "100000"});
    CHECK(too_long.str().find("must be between 1 and") != std::string::npos);
}

TEST_CASE("scheduler instances refuses a zero limit", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    log_in(session);
    std::ostringstream out;

    scheduler_commands::process_instances(out, session, {"--limit", "0"});

    CHECK(out.str().find("must be a positive integer") != std::string::npos);
}

TEST_CASE("scheduler status needs a session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    scheduler_commands::process_status(out, session);

    CHECK(out.str().find("must be logged in") != std::string::npos);
}
