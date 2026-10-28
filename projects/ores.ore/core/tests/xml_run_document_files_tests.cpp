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
#include "ores.ore.core/xml/document.hpp"
#include "ores.ore.core/xml/run_document_files.hpp"
#include "test_suite.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <format>
#include <fstream>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

namespace {

std::string read(const std::filesystem::path& path) {
    std::ifstream in(path, std::ios::binary);
    std::ostringstream buffer;
    buffer << in.rdbuf();
    return buffer.str();
}

/**
 * @brief Whether a piece of text carries data rather than formatting.
 */
bool has_text(std::string_view text) {
    return text.find_first_not_of(" \t\r\n") != std::string_view::npos;
}

/**
 * @brief Whether a run document sets @p parameter to a file under its Setup.
 */
bool sets_parameter(const std::filesystem::path& path, std::string_view parameter) {
    auto parsed = ores::ore::xml::document::read(read(path));
    if (!parsed)
        return false;

    const auto& tokens = parsed->tokens();
    for (std::size_t i = 0; i < tokens.size(); ++i) {
        if (tokens[i].kind != ores::ore::xml::token_kind::begin_element ||
            parsed->name(tokens[i]) != "Parameter")
            continue;

        std::string_view name;
        bool has_value = false;
        for (std::size_t j = i + 1; j < tokens.size(); ++j) {
            const auto& token = tokens[j];
            if (token.kind == ores::ore::xml::token_kind::end_element &&
                parsed->name(token) == "Parameter")
                break;
            if (token.kind == ores::ore::xml::token_kind::attribute &&
                parsed->name(token) == "name")
                name = parsed->value(token);
            if (token.kind == ores::ore::xml::token_kind::text &&
                has_text(parsed->value(token)))
                has_value = true;
        }
        if (name == parameter && has_value)
            return true;
    }
    return false;
}

/**
 * @brief The run documents ORE ships, which are the ones named @c ore*.xml.
 */
std::vector<std::filesystem::path> sample_run_documents() {
    const auto root = ores::testing::project_root::resolve("external/ore/examples");
    std::vector<std::filesystem::path> documents;
    for (const auto& entry : std::filesystem::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file())
            continue;
        const auto name = entry.path().filename().string();
        if (name.starts_with("ore") && name.ends_with(".xml"))
            documents.push_back(entry.path());
    }
    return documents;
}

}

TEST_CASE("the input files a run names are declared here", "[ore][run-document]") {
    auto lg(ores::logging::make_logger(test_suite));

    using ores::ore::xml::setup_parameter_of;
    CHECK(setup_parameter_of("conventions") == "conventionsFile");
    CHECK(setup_parameter_of("curve_configuration") == "curveConfigFile");
    CHECK(setup_parameter_of("todays_market") == "marketConfigFile");
    CHECK(setup_parameter_of("pricing_engines") == "pricingEnginesFile");
}

TEST_CASE("a configuration type a run does not name has no parameter", "[ore][run-document]") {
    auto lg(ores::logging::make_logger(test_suite));

    CHECK_FALSE(ores::ore::xml::setup_parameter_of("risk_report_config").has_value());
    CHECK_FALSE(ores::ore::xml::setup_parameter_of("engine_settings").has_value());
}

TEST_CASE("every declared parameter is exercised by a run document ORE ships",
          "[ore][run-document]") {
    auto lg(ores::logging::make_logger(test_suite));

    const auto documents = sample_run_documents();
    REQUIRE(!documents.empty());

    for (const auto& declared : ores::ore::xml::configuration_documents) {
        const auto found = std::ranges::any_of(documents, [&](const auto& path) {
            return sets_parameter(path, declared.setup_parameter);
        });
        INFO("parameter: " << declared.setup_parameter);
        CHECK(found);
    }
}
