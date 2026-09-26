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
#include "ores.logging/lifecycle_manager.hpp"
#include "ores.logging/logging_options.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/log/core.hpp>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace {

const std::string_view test_suite("ores.logging.tests");
const std::string tags("[logging]");

ores::logging::logging_options make_console_only_options() {
    ores::logging::logging_options r;
    r.severity = "info";
    r.output_to_console = true;
    return r;
}

bool core_logging_enabled() {
    return boost::log::core::get()->get_logging_enabled();
}

}

using ores::logging::lifecycle_manager;

TEST_CASE("absent_configuration_disables_core_logging", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    boost::log::core::get()->set_logging_enabled(true);

    const lifecycle_manager mgr(std::nullopt);

    BOOST_LOG_SEV(lg, ores::logging::info) << "core logging enabled: " << core_logging_enabled();

    CHECK_FALSE(core_logging_enabled());
}

TEST_CASE("valid_configuration_enables_core_logging", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    boost::log::core::get()->set_logging_enabled(false);
    REQUIRE_FALSE(core_logging_enabled());

    const lifecycle_manager mgr(make_console_only_options());

    BOOST_LOG_SEV(lg, ores::logging::info) << "core logging enabled: " << core_logging_enabled();

    CHECK(core_logging_enabled());
}
