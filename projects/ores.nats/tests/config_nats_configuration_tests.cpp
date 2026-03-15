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
#include "ores.nats/config/nats_configuration.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/program_options.hpp>
#include "ores.logging/make_logger.hpp"

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

    const auto result = parse({
        "--nats-url", "nats://cluster:4222",
        "--nats-subject-prefix", "ores.staging.node2"});

    CHECK(result.url == "nats://cluster:4222");
    CHECK(result.subject_prefix == "ores.staging.node2");
}
