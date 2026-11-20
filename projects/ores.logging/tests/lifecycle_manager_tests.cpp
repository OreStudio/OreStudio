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
#include "ores.logging/logger.hpp"
#include "ores.logging/logging_options.hpp"
#include "test_helpers.hpp"
#include "test_suite.hpp"
#include <boost/log/core.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cctype>
#include <filesystem>
#include <fstream>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

using ores::logging::tests::make_console_only_options;

namespace {

bool core_logging_enabled() {
    return boost::log::core::get()->get_logging_enabled();
}

/// True when the line opens with the timestamp a formatter writes.
bool starts_with_timestamp(const std::string& line) {
    const auto digits = [&line](std::string::size_type at, std::string::size_type count) {
        return std::all_of(line.begin() + static_cast<std::ptrdiff_t>(at),
                           line.begin() + static_cast<std::ptrdiff_t>(at + count),
                           [](unsigned char c) { return std::isdigit(c) != 0; });
    };
    return line.size() >= 20 && digits(0, 4) && line[4] == '-' && digits(5, 2) && line[7] == '-' &&
           digits(8, 2) && line[10] == ' ' && digits(11, 2) && line[13] == ':' && digits(14, 2) &&
           line[16] == ':' && digits(17, 2) && line[19] == '.';
}

std::vector<std::string> read_lines(const std::filesystem::path& path) {
    std::vector<std::string> lines;
    std::ifstream in(path);
    for (std::string line; std::getline(in, line);)
        lines.push_back(line);
    return lines;
}

}

using ores::logging::lifecycle_manager;

TEST_CASE("absent_configuration_disables_core_logging", "[logging]") {
    auto lg(ores::logging::make_logger(test_suite));
    boost::log::core::get()->set_logging_enabled(true);

    const lifecycle_manager mgr(std::nullopt);

    ORES_LOG_SEV(lg, ores::logging::info) << "core logging enabled: " << core_logging_enabled();

    CHECK_FALSE(core_logging_enabled());
}

TEST_CASE("valid_configuration_enables_core_logging", "[logging]") {
    auto lg(ores::logging::make_logger(test_suite));
    boost::log::core::get()->set_logging_enabled(false);
    REQUIRE_FALSE(core_logging_enabled());

    const lifecycle_manager mgr(make_console_only_options());

    ORES_LOG_SEV(lg, ores::logging::info) << "core logging enabled: " << core_logging_enabled();

    CHECK(core_logging_enabled());
}

TEST_CASE("every_record_the_manager_writes_carries_a_timestamp", "[logging]") {
    auto lg(ores::logging::make_logger(test_suite));

    auto directory =
        std::filesystem::temp_directory_path() / "ores.logging.lifecycle_manager_tests";
    std::filesystem::remove_all(directory);
    std::filesystem::create_directories(directory);

    auto options = make_console_only_options();
    options.output_to_console = false;
    options.filename = "replica.log";
    options.output_directory = directory;
    options.replica_index = 0;

    {
        const lifecycle_manager mgr(options);
        ORES_LOG_SEV(lg, ores::logging::info) << "a record written after the manager started";
        boost::log::core::get()->flush();
    }

    const auto lines = read_lines(directory / "replica.0.log");
    REQUIRE(lines.size() >= 2);

    bool found_replica_index = false;
    for (const auto& line : lines) {
        INFO("record: " << line);
        CHECK(starts_with_timestamp(line));
        found_replica_index =
            found_replica_index || line.find("Replica index: 0") != std::string::npos;
    }
    CHECK(found_replica_index);

    std::filesystem::remove_all(directory);
}
