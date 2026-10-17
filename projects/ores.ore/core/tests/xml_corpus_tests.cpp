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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.ore.core/xml/document.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

namespace {

const std::string tags("[ore][xml][corpus]");

namespace fs = std::filesystem;
using ores::ore::xml::document;
using ores::ore::xml::token_kind;

auto read_file(const fs::path& path) -> std::string {
    std::ifstream file(path, std::ios::binary);
    return std::string(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
}

auto examples_dir() -> fs::path {
    return fs::path(ORES_SOURCE_DIR) / "external" / "ore" / "examples";
}

/**
 * @brief True when the text is a decimal spelling and nothing else.
 *
 * The test only asks the decimal type about text that claims to be a number,
 * so a name that happens to contain a digit is not counted as a refusal.
 */
auto looks_numeric(std::string_view text) -> bool {
    if (text.empty())
        return false;
    std::size_t digits = 0;
    for (const char c : text) {
        if (c >= '0' && c <= '9') {
            ++digits;
        } else if (c != '+' && c != '-' && c != '.' && c != 'e' && c != 'E') {
            return false;
        }
    }
    return digits != 0;
}

}

TEST_CASE("every_ore_example_survives_a_round_trip_and_writes_back_out", tags) {
    const auto root = examples_dir();
    REQUIRE(fs::is_directory(root));

    const auto output = fs::path(ORES_XML_ROUNDTRIP_DIR);
    std::error_code ignored;
    fs::remove_all(output, ignored);

    std::size_t documents = 0;
    std::size_t failures = 0;
    std::string report;
    std::string written;

    for (const auto& entry : fs::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file() || entry.path().extension() != ".xml")
            continue;
        ++documents;
        const auto text = read_file(entry.path());

        const auto original = document::read(text);
        if (!original) {
            ++failures;
            report += entry.path().string() + ": " + original.error().message + " at line " +
                      std::to_string(original.error().line) + "\n";
            continue;
        }

        written.clear();
        original->write(written);

        const auto reread = document::read(written);
        if (!reread) {
            ++failures;
            report += entry.path().string() +
                      " wrote something it cannot read: " + reread.error().message + "\n";
            continue;
        }
        if (!(*original == *reread)) {
            ++failures;
            report += entry.path().string() + ": the data changed across the round trip\n";
            continue;
        }

        const auto target = output / fs::relative(entry.path(), root);
        std::error_code ignored;
        fs::create_directories(target.parent_path(), ignored);
        std::ofstream file(target, std::ios::binary);
        file << written;
        if (!file) {
            ++failures;
            report += "could not write " + target.string() + "\n";
        }
    }

    INFO(report);
    CHECK(documents > 1500);
    CHECK(failures == 0);
}

TEST_CASE("every_decimal_in_the_corpus_reads_and_writes_as_the_same_value", tags) {
    const auto root = examples_dir();
    REQUIRE(fs::is_directory(root));

    std::size_t numbers = 0;
    std::size_t refused = 0;
    std::size_t changed = 0;
    std::string report;

    for (const auto& entry : fs::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file() || entry.path().extension() != ".xml")
            continue;
        const auto parsed = document::read(read_file(entry.path()));
        if (!parsed)
            continue;

        for (const auto& t : parsed->tokens()) {
            if (t.kind != token_kind::text)
                continue;
            const auto text = parsed->value(t);
            if (!looks_numeric(text))
                continue;
            ++numbers;

            const auto value = ores::utility::decimal::decimal::from_string(text);
            if (!value) {
                ++refused;
                report += entry.path().filename().string() + ": " + std::string(text) +
                          " is not a decimal: " + value.error() + "\n";
                continue;
            }
            const auto again = ores::utility::decimal::decimal::from_string(value->to_string());
            if (!again || !(*again == *value)) {
                ++changed;
                report += entry.path().filename().string() + ": " + std::string(text) +
                          " changed to " + value->to_string() + "\n";
            }
        }
    }

    INFO(report);
    CHECK(numbers > 10000);
    CHECK(refused == 0);
    CHECK(changed == 0);
}
