/*
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
 */
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/reporting/pricing_engines_commands.hpp"
#include "ores.testing/party_fixture.hpp"
#include "test_suite.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cli/cli.h>
#include <filesystem>
#include <fstream>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

using ores::testing::valid_party_id;
using ores::nats::service::nats_client;
using ores::shell::app::command_feedback;
using ores::shell::app::commands::pricing_engines_commands;
using namespace ores::logging;

namespace {

const std::string configuration_id("55555555-5555-4555-8555-555555555555");

void log_in(nats_client& session) {
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    info.default_party_id = valid_party_id;
    session.set_auth(std::move(info));
}

}

TEST_CASE("pricing_engines_commands_registers_the_export", "[commands]") {
    auto lg(make_logger(test_suite));

    cli::Menu root_menu("root");
    nats_client session;

    pricing_engines_commands::register_commands(root_menu, session);

    const auto completions = root_menu.GetCompletions("pricing_engines ");
    CHECK(std::find(completions.begin(),
                    completions.end(),
                    std::string{"pricing_engines export"}) != completions.end());

    BOOST_LOG_SEV(lg, debug) << "Registered 1 command(s).";
}

TEST_CASE("pricing_engines_commands_process_export_requires_a_session", "[commands]") {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    pricing_engines_commands::process_export(out, session, {configuration_id});

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("pricing_engines_commands_process_export_reports_the_expected_count", "[commands]") {
    auto lg(make_logger(test_suite));

    nats_client session;
    log_in(session);
    std::ostringstream out;

    command_feedback::reset();
    pricing_engines_commands::process_export(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 1 argument, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("pricing_engines_commands_process_export_refuses_an_id_that_is_not_one", "[commands]") {
    auto lg(make_logger(test_suite));

    nats_client session;
    log_in(session);
    std::ostringstream out;

    command_feedback::reset();
    pricing_engines_commands::process_export(out, session, {"not-an-identifier"});

    BOOST_LOG_SEV(lg, debug) << "Output for a bad id: " << out.str();
    CHECK(out.str().find("is not one") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("pricing_engines_commands_process_export_refuses_an_unknown_flag", "[commands]") {
    auto lg(make_logger(test_suite));

    nats_client session;
    log_in(session);
    std::ostringstream out;

    command_feedback::reset();
    pricing_engines_commands::process_export(out, session, {configuration_id, "--nonsense"});

    BOOST_LOG_SEV(lg, debug) << "Output for an unknown flag: " << out.str();
    CHECK(out.str().find("--nonsense") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("pricing_engines_commands_writes_the_document_where_it_was_asked", "[commands]") {
    auto lg(make_logger(test_suite));

    const ores::ore::messaging::ore_document file{
        .name = "pricingengine.xml",
        .content = "<?xml version=\"1.0\"?>\n<PricingEngines>\n  <GlobalParameters />\n"
                   "</PricingEngines>\n"};

    std::ostringstream printed;
    CHECK(pricing_engines_commands::write_document(printed, "", file));
    CHECK(printed.str() == file.content);

    const auto path = std::filesystem::temp_directory_path() / "pricing_engines_export.xml";
    std::ostringstream told;
    CHECK(pricing_engines_commands::write_document(told, path.string(), file));
    CHECK(told.str().find(path.string()) != std::string::npos);
    CHECK(told.str().find(file.name) != std::string::npos);

    std::ifstream read(path);
    const std::string written((std::istreambuf_iterator<char>(read)),
                              std::istreambuf_iterator<char>());
    BOOST_LOG_SEV(lg, debug) << "Written file: " << written;
    CHECK(written == file.content);
    read.close();
    std::filesystem::remove(path);
}

TEST_CASE("pricing_engines_commands_states_a_path_it_cannot_write", "[commands]") {
    auto lg(make_logger(test_suite));

    std::ostringstream out;
    command_feedback::reset();
    const bool written = pricing_engines_commands::write_document(
        out,
        "/nonexistent-directory/pricingengine.xml",
        {.name = "pricingengine.xml", .content = "<PricingEngines />"});

    BOOST_LOG_SEV(lg, debug) << "Output: " << out.str();
    CHECK_FALSE(written);
    CHECK(out.str().find("Cannot write") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("pricing_engines_commands_registers_the_import", "[commands]") {
    auto lg(make_logger(test_suite));

    cli::Menu root_menu("root");
    nats_client session;

    pricing_engines_commands::register_commands(root_menu, session);

    const auto completions = root_menu.GetCompletions("pricing_engines ");
    BOOST_LOG_SEV(lg, debug) << "Completions: " << completions.size();
    CHECK(std::find(completions.begin(),
                    completions.end(),
                    std::string{"pricing_engines import"}) != completions.end());
}

TEST_CASE("pricing_engines_commands_process_import_requires_a_session", "[commands]") {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    pricing_engines_commands::process_import(out, session, {configuration_id});

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("Name the file with --file") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("pricing_engines_commands_process_import_needs_a_file", "[commands]") {
    auto lg(make_logger(test_suite));

    nats_client session;
    log_in(session);
    std::ostringstream out;

    command_feedback::reset();
    pricing_engines_commands::process_import(out, session, {"--new-name", "From ORE"});

    BOOST_LOG_SEV(lg, debug) << "Output with no file: " << out.str();
    CHECK(out.str().find("--file <path>") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("pricing_engines_commands_process_import_names_one_configuration", "[commands]") {
    auto lg(make_logger(test_suite));

    const auto path = std::filesystem::temp_directory_path() / "pricing_engines_no_target.xml";
    {
        std::ofstream write(path);
        write << "<?xml version=\"1.0\"?>\n<PricingEngines>\n</PricingEngines>\n";
    }

    nats_client session;
    log_in(session);
    std::ostringstream out;

    command_feedback::reset();
    pricing_engines_commands::process_import(out, session, {"--file", path.string()});

    BOOST_LOG_SEV(lg, debug) << "Output with no target: " << out.str();
    CHECK(out.str().find("Name one configuration") != std::string::npos);
    CHECK(command_feedback::failed());
    std::filesystem::remove(path);
}

TEST_CASE("pricing_engines_commands_process_import_refuses_a_file_it_cannot_read",
          "[commands]") {
    auto lg(make_logger(test_suite));

    nats_client session;
    log_in(session);
    std::ostringstream out;

    command_feedback::reset();
    pricing_engines_commands::process_import(
        out, session, {configuration_id, "--file", "/nonexistent-directory/pricingengine.xml"});

    BOOST_LOG_SEV(lg, debug) << "Output: " << out.str();
    CHECK(out.str().find("Cannot read") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("pricing_engines_commands_process_import_refuses_two_configurations", "[commands]") {
    auto lg(make_logger(test_suite));

    const auto path = std::filesystem::temp_directory_path() / "pricing_engines_import.xml";
    {
        std::ofstream write(path);
        write << "<?xml version=\"1.0\"?>\n<PricingEngines>\n</PricingEngines>\n";
    }

    nats_client session;
    log_in(session);
    std::ostringstream out;

    command_feedback::reset();
    pricing_engines_commands::process_import(
        out,
        session,
        {configuration_id, "--new-name", "From ORE", "--file", path.string()});

    BOOST_LOG_SEV(lg, debug) << "Output: " << out.str();
    CHECK(out.str().find("Name one configuration") != std::string::npos);
    CHECK(command_feedback::failed());
    std::filesystem::remove(path);
}
