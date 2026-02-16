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
#include "ores.utility/string/short_code_generator.hpp"

#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[short_code_generator]");

using ores::utility::string::strip_corporate_suffixes;
using ores::utility::string::generate_short_code;
using ores::utility::string::generate_unique_short_code;

}

// ============================================================================
// strip_corporate_suffixes
// ============================================================================

TEST_CASE("strip_corporate_suffixes_removes_plc", tags) {
    CHECK(strip_corporate_suffixes("Barclays Plc") == "Barclays");
}

TEST_CASE("strip_corporate_suffixes_removes_ltd", tags) {
    CHECK(strip_corporate_suffixes("Hargreaves Securities Ltd") ==
        "Hargreaves Securities");
}

TEST_CASE("strip_corporate_suffixes_removes_llc", tags) {
    CHECK(strip_corporate_suffixes("Goldman Capital LLC") ==
        "Goldman Capital");
}

TEST_CASE("strip_corporate_suffixes_removes_inc", tags) {
    CHECK(strip_corporate_suffixes("Hamilton Financial Inc") ==
        "Hamilton Financial");
}

TEST_CASE("strip_corporate_suffixes_removes_sa_dotted", tags) {
    CHECK(strip_corporate_suffixes("BNP Paribas S.A.") == "BNP Paribas");
}

TEST_CASE("strip_corporate_suffixes_removes_gmbh", tags) {
    CHECK(strip_corporate_suffixes("Deutsche Bank AG") == "Deutsche Bank");
}

TEST_CASE("strip_corporate_suffixes_is_case_insensitive", tags) {
    CHECK(strip_corporate_suffixes("Barclays PLC") == "Barclays");
    CHECK(strip_corporate_suffixes("Barclays plc") == "Barclays");
}

TEST_CASE("strip_corporate_suffixes_preserves_name_without_suffix", tags) {
    CHECK(strip_corporate_suffixes("Rutherford Brothers") ==
        "Rutherford Brothers");
}

// ============================================================================
// generate_short_code — matches SQL ores_utility_generate_short_code_fn
// ============================================================================

TEST_CASE("generate_short_code_single_word_uses_consonants", tags) {
    // "BARCLAYS" -> strip "Plc" -> "BARCLAYS"
    // Single word: B + RCLYS (consonants after first) = BRCLYS
    CHECK(generate_short_code("Barclays Plc") == "BRCLYS");
}

TEST_CASE("generate_short_code_two_words_uses_3_plus_3", tags) {
    // "BNP PARIBAS" -> 3+3 = BNPPAR
    CHECK(generate_short_code("BNP Paribas S.A.") == "BNPPAR");
}

TEST_CASE("generate_short_code_three_words_uses_2_plus_2_plus_2", tags) {
    // "DEUTSCHE BANK" -> strip "AG" -> two words "DEUTSCHE BANK" = DEUBAN
    CHECK(generate_short_code("Deutsche Bank AG") == "DEUBAN");
}

TEST_CASE("generate_short_code_empty_returns_unknwn", tags) {
    CHECK(generate_short_code("") == "UNKNWN");
}

TEST_CASE("generate_short_code_only_suffix_returns_unknwn", tags) {
    CHECK(generate_short_code("Plc") == "UNKNWN");
}

TEST_CASE("generate_short_code_short_single_word_is_padded", tags) {
    // "AB" after stripping -> single word "AB" -> first letter A +
    // consonant B = "AB" -> pad to 3 = "ABX"
    CHECK(generate_short_code("AB").size() >= 3);
}

TEST_CASE("generate_short_code_three_plus_words", tags) {
    // "ROYAL RUTHERFORD GROUP" -> 2+2+2 = RORUGR
    CHECK(generate_short_code("Royal Rutherford Group") == "RORUGR");
}

// ============================================================================
// generate_unique_short_code — collision resolution
// ============================================================================

TEST_CASE("generate_unique_short_code_returns_base_on_first_call", tags) {
    std::unordered_set<std::string> used;
    auto code = generate_unique_short_code("Barclays Plc", used);
    CHECK(code == "BRCLYS");
    CHECK(used.contains("BRCLYS"));
}

TEST_CASE("generate_unique_short_code_appends_suffix_on_collision", tags) {
    std::unordered_set<std::string> used;
    auto code1 = generate_unique_short_code("Barclays Plc", used);
    auto code2 = generate_unique_short_code("Barclays Ltd", used);
    CHECK(code1 == "BRCLYS");
    CHECK(code2 == "BRCLYS2");
    CHECK(used.size() == 2);
}

TEST_CASE("generate_unique_short_code_handles_multiple_collisions", tags) {
    std::unordered_set<std::string> used;
    auto code1 = generate_unique_short_code("Barclays Plc", used);
    auto code2 = generate_unique_short_code("Barclays Ltd", used);
    auto code3 = generate_unique_short_code("Barclays Corp", used);
    CHECK(code1 == "BRCLYS");
    CHECK(code2 == "BRCLYS2");
    CHECK(code3 == "BRCLYS3");
}

TEST_CASE("generate_unique_short_code_different_names_no_collision", tags) {
    std::unordered_set<std::string> used;
    auto code1 = generate_unique_short_code("Barclays Plc", used);
    auto code2 = generate_unique_short_code("Goldman Sachs", used);
    CHECK(code1 != code2);
    CHECK(used.size() == 2);
}
