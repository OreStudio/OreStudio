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
#include "ores.nats/config/nats_configuration.hpp"
#include "ores.utility/program_options/environment_mapper_factory.hpp"
#include <boost/program_options.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cstdlib>

namespace {

const std::string_view test_suite("ores.nats.tests");
const std::string tags("[config]");

ores::nats::config::nats_options parse(const std::vector<std::string>& args) {
    using namespace boost::program_options;
    using ores::nats::config::nats_configuration;

    const auto od = nats_configuration::make_options_description();
    variables_map vm;
    store(command_line_parser(args).options(od).run(), vm);
    notify(vm);
    return nats_configuration::read_options(vm);
}

}

TEST_CASE("nats_configuration_defaults", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto result = parse({});

    CHECK(result.url == "nats://localhost:4222");
    CHECK(result.subject_prefix.empty());
}

TEST_CASE("nats_configuration_custom_url", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto result = parse({"--nats-url", "nats://myserver:5555"});

    CHECK(result.url == "nats://myserver:5555");
}

TEST_CASE("nats_configuration_subject_prefix", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto result = parse({"--nats-subject-prefix", "ores.prod.main1"});

    CHECK(result.subject_prefix == "ores.prod.main1");
}

TEST_CASE("nats_configuration_url_and_prefix_together", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto result =
        parse({"--nats-url", "nats://cluster:4222", "--nats-subject-prefix", "ores.staging.node2"});

    CHECK(result.url == "nats://cluster:4222");
    CHECK(result.subject_prefix == "ores.staging.node2");
}

TEST_CASE("nats_configuration_wire_format_defaults_to_msgpack", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto result = parse({});

    CHECK(result.format == ores::nats::wire_format::msgpack);
}

TEST_CASE("nats_configuration_wire_format_msgpack", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto result = parse({"--nats-wire-format", "msgpack"});

    CHECK(result.format == ores::nats::wire_format::msgpack);
}

TEST_CASE("nats_configuration_wire_format_rejects_unknown_value", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    CHECK_THROWS_AS(parse({"--nats-wire-format", "bson"}), std::invalid_argument);
}

TEST_CASE("register_shared_domain_makes_unprefixed_nats_vars_reach_an_unrelated_app", tags) {
    // Regression test for a real bug caught in review: the shared-domain
    // fallback must resolve ORES_NATS_URL to the actual registered option
    // name "nats-url" (not "url"), and must NOT choke on other ORES_NATS_*
    // variables that are not nats_configuration options at all (e.g. a
    // hypothetical server-side ORES_NATS_LISTEN_PORT). Exercises the real
    // parse_environment + store path end to end, the same way a service's
    // parser.cpp does -- a mapper-only unit test would not have caught
    // this, since it never runs parse_environment/store against a real
    // options_description.
    auto lg(ores::logging::make_logger(test_suite));
    using namespace boost::program_options;
    using ores::nats::config::nats_configuration;
    using ores::utility::program_options::environment_mapper_factory;

    nats_configuration::register_shared_domain();

    setenv("ORES_NATS_URL", "nats://regression-check:4222", 1);
    setenv("ORES_NATS_LISTEN_PORT", "4222", 1);

    const auto od = nats_configuration::make_options_description();
    const auto mapper(environment_mapper_factory::make_mapper("SOME_UNRELATED_APP"));

    variables_map vm;
    REQUIRE_NOTHROW(store(parse_environment(od, mapper), vm));
    notify(vm);

    const auto result = nats_configuration::read_options(vm);
    CHECK(result.url == "nats://regression-check:4222");

    unsetenv("ORES_NATS_URL");
    unsetenv("ORES_NATS_LISTEN_PORT");
}

TEST_CASE("register_shared_domain_does_not_resolve_tls_ca", tags) {
    // Regression test for a real bug caught in review: registering TLS_CA
    // alone (cert/key stay excluded, genuinely per-service) would let
    // nats_options.tls_ca_cert resolve non-empty via the shared fallback
    // while cert/key stay unresolved for services with no per-app mirror
    // -- tripping client.cpp's mTLS gate at connect time. The whole
    // nats-tls-* trio stays out of the shared domain until a follow-on
    // task designs per-service cert/key env wiring alongside it.
    auto lg(ores::logging::make_logger(test_suite));
    using namespace boost::program_options;
    using ores::nats::config::nats_configuration;
    using ores::utility::program_options::environment_mapper_factory;

    nats_configuration::register_shared_domain();
    setenv("ORES_NATS_TLS_CA", "/tmp/ca.crt", 1);

    const auto od = nats_configuration::make_options_description();
    const auto mapper(environment_mapper_factory::make_mapper("SOME_UNRELATED_APP"));

    variables_map vm;
    REQUIRE_NOTHROW(store(parse_environment(od, mapper), vm));
    notify(vm);

    const auto result = nats_configuration::read_options(vm);
    CHECK(result.tls_ca_cert.empty());

    unsetenv("ORES_NATS_TLS_CA");
}
