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
#include "ores.comms/config/ores.comms.config.hpp"

#include <vector>
#include <catch2/catch_test_macros.hpp>
#include "ores.comms/messaging/handshake_protocol.hpp"

namespace {

const std::string tags("[config]");

namespace po = boost::program_options;
using namespace ores::comms::config;
using namespace ores::comms::messaging;

po::variables_map parse_args(const po::options_description& desc,
                             const std::vector<const char*>& args) {
    po::variables_map vm;
    po::store(po::parse_command_line(
        static_cast<int>(args.size()), args.data(), desc), vm);
    po::notify(vm);
    return vm;
}

}

// server_configuration tests

TEST_CASE("server_make_options_description_without_ssl", tags) {
    auto desc = server_configuration::make_options_description(
        8080, 100, "test-server", false);

    REQUIRE(desc.find_nothrow("port", false) != nullptr);
    REQUIRE(desc.find_nothrow("max-connections", false) != nullptr);
    REQUIRE(desc.find_nothrow("identifier", false) != nullptr);
    REQUIRE(desc.find_nothrow("certificate", false) == nullptr);
    REQUIRE(desc.find_nothrow("private-key", false) == nullptr);
}

TEST_CASE("server_make_options_description_with_ssl", tags) {
    auto desc = server_configuration::make_options_description(
        8080, 100, "test-server", true);

    REQUIRE(desc.find_nothrow("port", false) != nullptr);
    REQUIRE(desc.find_nothrow("certificate", false) != nullptr);
    REQUIRE(desc.find_nothrow("private-key", false) != nullptr);
}

TEST_CASE("server_read_options_uses_defaults", tags) {
    auto desc = server_configuration::make_options_description(
        8080, 100, "test-server", true);
    std::vector<const char*> args = {"test"};
    auto vm = parse_args(desc, args);

    auto opts = server_configuration::read_options(vm);

    REQUIRE(opts.port == 8080);
    REQUIRE(opts.max_connections == 100);
    REQUIRE(opts.server_identifier == "test-server");
}

TEST_CASE("server_read_options_parses_port", tags) {
    auto desc = server_configuration::make_options_description(
        8080, 100, "test-server", false);
    std::vector<const char*> args = {"test", "--port", "9999"};
    auto vm = parse_args(desc, args);

    auto opts = server_configuration::read_options(vm);

    REQUIRE(opts.port == 9999);
}

TEST_CASE("server_read_options_parses_short_port", tags) {
    auto desc = server_configuration::make_options_description(
        8080, 100, "test-server", false);
    std::vector<const char*> args = {"test", "-p", "7777"};
    auto vm = parse_args(desc, args);

    auto opts = server_configuration::read_options(vm);

    REQUIRE(opts.port == 7777);
}

TEST_CASE("server_read_options_parses_max_connections", tags) {
    auto desc = server_configuration::make_options_description(
        8080, 100, "test-server", false);
    std::vector<const char*> args = {"test", "--max-connections", "500"};
    auto vm = parse_args(desc, args);

    auto opts = server_configuration::read_options(vm);

    REQUIRE(opts.max_connections == 500);
}

TEST_CASE("server_read_options_parses_identifier", tags) {
    auto desc = server_configuration::make_options_description(
        8080, 100, "test-server", false);
    std::vector<const char*> args = {"test", "--identifier", "my-server"};
    auto vm = parse_args(desc, args);

    auto opts = server_configuration::read_options(vm);

    REQUIRE(opts.server_identifier == "my-server");
}

TEST_CASE("server_read_options_parses_ssl_options", tags) {
    auto desc = server_configuration::make_options_description(
        8080, 100, "test-server", true);
    std::vector<const char*> args = {"test", "--certificate", "my.crt", "--private-key", "my.key"};
    auto vm = parse_args(desc, args);

    auto opts = server_configuration::read_options(vm);

    REQUIRE(opts.certificate_file == "my.crt");
    REQUIRE(opts.private_key_file == "my.key");
}

// client_configuration tests

TEST_CASE("client_make_options_description_without_auth", tags) {
    auto desc = client_configuration::make_options_description(
        55555, "test-client", false);

    REQUIRE(desc.find_nothrow("connect-host", false) != nullptr);
    REQUIRE(desc.find_nothrow("connect-port", false) != nullptr);
    REQUIRE(desc.find_nothrow("connect-identifier", false) != nullptr);
    REQUIRE(desc.find_nothrow("login-username", false) == nullptr);
    REQUIRE(desc.find_nothrow("login-password", false) == nullptr);
}

TEST_CASE("client_make_options_description_with_auth", tags) {
    auto desc = client_configuration::make_options_description(
        55555, "test-client", true);

    REQUIRE(desc.find_nothrow("connect-host", false) != nullptr);
    REQUIRE(desc.find_nothrow("login-username", false) != nullptr);
    REQUIRE(desc.find_nothrow("login-password", false) != nullptr);
}

TEST_CASE("client_read_options_uses_defaults", tags) {
    auto desc = client_configuration::make_options_description(
        55555, "test-client", false);
    std::vector<const char*> args = {"test"};
    auto vm = parse_args(desc, args);

    auto opts = client_configuration::read_options(vm);

    REQUIRE(opts.host == "localhost");
    REQUIRE(opts.port == 55555);
    REQUIRE(opts.client_identifier == "test-client");
}

TEST_CASE("client_read_options_parses_host", tags) {
    auto desc = client_configuration::make_options_description(
        55555, "test-client", false);
    std::vector<const char*> args = {"test", "--connect-host", "192.168.1.1"};
    auto vm = parse_args(desc, args);

    auto opts = client_configuration::read_options(vm);

    REQUIRE(opts.host == "192.168.1.1");
}

TEST_CASE("client_read_options_parses_port", tags) {
    auto desc = client_configuration::make_options_description(
        55555, "test-client", false);
    std::vector<const char*> args = {"test", "--connect-port", "12345"};
    auto vm = parse_args(desc, args);

    auto opts = client_configuration::read_options(vm);

    REQUIRE(opts.port == 12345);
}

TEST_CASE("client_read_options_parses_identifier", tags) {
    auto desc = client_configuration::make_options_description(
        55555, "test-client", false);
    std::vector<const char*> args = {"test", "--connect-identifier", "my-client"};
    auto vm = parse_args(desc, args);

    auto opts = client_configuration::read_options(vm);

    REQUIRE(opts.client_identifier == "my-client");
}

TEST_CASE("client_read_login_options_returns_nullopt_when_no_options", tags) {
    auto desc = client_configuration::make_options_description(
        55555, "test-client", true);
    std::vector<const char*> args = {"test"};
    auto vm = parse_args(desc, args);

    auto opts = client_configuration::read_login_options(vm);

    REQUIRE_FALSE(opts.has_value());
}

TEST_CASE("client_read_login_options_parses_credentials", tags) {
    auto desc = client_configuration::make_options_description(
        55555, "test-client", true);
    std::vector<const char*> args = {"test", "--login-username", "user1", "--login-password", "pass1"};
    auto vm = parse_args(desc, args);

    auto opts = client_configuration::read_login_options(vm);

    REQUIRE(opts.has_value());
    REQUIRE(opts->username == "user1");
    REQUIRE(opts->password == "pass1");
}

TEST_CASE("client_read_login_options_throws_on_missing_password", tags) {
    auto desc = client_configuration::make_options_description(
        55555, "test-client", true);
    std::vector<const char*> args = {"test", "--login-username", "user1"};
    auto vm = parse_args(desc, args);

    REQUIRE_THROWS_AS(client_configuration::read_login_options(vm), std::runtime_error);
}

TEST_CASE("client_read_login_options_throws_on_missing_username", tags) {
    auto desc = client_configuration::make_options_description(
        55555, "test-client", true);
    std::vector<const char*> args = {"test", "--login-password", "pass1"};
    auto vm = parse_args(desc, args);

    REQUIRE_THROWS_AS(client_configuration::read_login_options(vm), std::runtime_error);
}

// compression_configuration tests

TEST_CASE("compression_make_options_description_has_options", tags) {
    auto desc = compression_configuration::make_options_description();

    REQUIRE(desc.find_nothrow("compression-enabled", false) != nullptr);
    REQUIRE(desc.find_nothrow("compression-algorithm", false) != nullptr);
}

TEST_CASE("compression_read_options_disabled_by_default", tags) {
    auto desc = compression_configuration::make_options_description();
    std::vector<const char*> args = {"test"};
    auto vm = parse_args(desc, args);

    auto opts = compression_configuration::read_options(vm);

    REQUIRE_FALSE(opts.enabled);
    REQUIRE(opts.algorithm == "all");
}

TEST_CASE("compression_read_options_enabled_with_flag", tags) {
    auto desc = compression_configuration::make_options_description();
    std::vector<const char*> args = {"test", "--compression-enabled"};
    auto vm = parse_args(desc, args);

    auto opts = compression_configuration::read_options(vm);

    REQUIRE(opts.enabled);
}

TEST_CASE("compression_read_options_enabled_with_short_flag", tags) {
    auto desc = compression_configuration::make_options_description();
    std::vector<const char*> args = {"test", "-c"};
    auto vm = parse_args(desc, args);

    auto opts = compression_configuration::read_options(vm);

    REQUIRE(opts.enabled);
}

TEST_CASE("compression_read_options_parses_algorithm", tags) {
    auto desc = compression_configuration::make_options_description();
    std::vector<const char*> args = {"test", "--compression-algorithm", "zlib"};
    auto vm = parse_args(desc, args);

    auto opts = compression_configuration::read_options(vm);

    REQUIRE(opts.algorithm == "zlib");
}

TEST_CASE("compression_to_mask_returns_zero_when_disabled", tags) {
    compression_options opts;
    opts.enabled = false;

    auto mask = compression_configuration::to_compression_mask(opts);

    REQUIRE(mask == 0);
}

TEST_CASE("compression_to_mask_returns_zlib", tags) {
    compression_options opts;
    opts.enabled = true;
    opts.algorithm = "zlib";

    auto mask = compression_configuration::to_compression_mask(opts);

    REQUIRE(mask == COMPRESSION_SUPPORT_ZLIB);
}

TEST_CASE("compression_to_mask_returns_gzip", tags) {
    compression_options opts;
    opts.enabled = true;
    opts.algorithm = "gzip";

    auto mask = compression_configuration::to_compression_mask(opts);

    REQUIRE(mask == COMPRESSION_SUPPORT_GZIP);
}

TEST_CASE("compression_to_mask_returns_bzip2", tags) {
    compression_options opts;
    opts.enabled = true;
    opts.algorithm = "bzip2";

    auto mask = compression_configuration::to_compression_mask(opts);

    REQUIRE(mask == COMPRESSION_SUPPORT_BZIP2);
}

TEST_CASE("compression_to_mask_returns_all_for_all", tags) {
    compression_options opts;
    opts.enabled = true;
    opts.algorithm = "all";

    auto mask = compression_configuration::to_compression_mask(opts);

    REQUIRE(mask == COMPRESSION_SUPPORT_ALL);
}

TEST_CASE("compression_to_mask_returns_all_for_unknown", tags) {
    compression_options opts;
    opts.enabled = true;
    opts.algorithm = "unknown";

    auto mask = compression_configuration::to_compression_mask(opts);

    REQUIRE(mask == COMPRESSION_SUPPORT_ALL);
}
