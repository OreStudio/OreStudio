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
#include <algorithm>
#include <filesystem>
#include <format>
#include <fstream>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

namespace {

std::string read_text(const std::filesystem::path& path) {
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
 * @brief The file a run document sets @p parameter to under its Setup, if any.
 */
std::optional<std::string> file_named_by(const std::filesystem::path& path,
                                         std::string_view parameter) {
    auto parsed = ores::ore::xml::document::read(read_text(path));
    if (!parsed)
        return std::nullopt;

    const auto& tokens = parsed->tokens();
    for (std::size_t i = 0; i < tokens.size(); ++i) {
        if (tokens[i].kind != ores::ore::xml::token_kind::begin_element ||
            parsed->name(tokens[i]) != "Parameter")
            continue;

        std::string_view name;
        std::string value;
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
                value = std::string(parsed->value(token));
        }
        if (name == parameter && !value.empty())
            return value;
    }
    return std::nullopt;
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

namespace {

/// The root element each parameter's file must have.
struct expected_document {
    std::string_view parameter;
    std::string_view root;
};

inline constexpr expected_document expected_roots[] = {
    {"conventionsFile", "Conventions"},
    {"curveConfigFile", "CurveConfiguration"},
    {"marketConfigFile", "TodaysMarket"},
    {"pricingEnginesFile", "PricingEngines"},
};

}

TEST_CASE("every declared parameter names a document of the kind it states",
          "[ore][run-document]") {
    auto lg(ores::logging::make_logger(test_suite));

    const auto root = ores::testing::project_root::resolve("external/ore/examples");
    INFO("the ORE examples are at " << root);
    REQUIRE(std::filesystem::exists(root));

    const auto documents = sample_run_documents();
    REQUIRE(!documents.empty());

    for (const auto& expected : expected_roots) {
        std::size_t checked = 0;
        for (const auto& document : documents) {
            const auto file = file_named_by(document, expected.parameter);
            if (!file)
                continue;
            const auto target = document.parent_path() / *file;
            if (!std::filesystem::is_regular_file(target))
                continue;

            auto parsed = ores::ore::xml::document::read(read_text(target));
            INFO("parameter " << expected.parameter << " names " << target);
            REQUIRE(parsed.has_value());
            CHECK(parsed->root() == expected.root);
            ++checked;
        }
        INFO("parameter " << expected.parameter);
        CHECK(checked > 0);
    }
}
