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
#include "ores.diff/engine/compare.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[engine][diff_span]");

}

using ores::diff::domain::diff_span;
using ores::diff::domain::field_value;
using ores::diff::engine::compute;

TEST_CASE("diff_span_single_character_change_is_a_single_span_each_side", tags) {
    const std::vector<field_value> previous{{.name = "Year", .value = "2025"}};
    const std::vector<field_value> current{{.name = "Year", .value = "2026"}};

    const auto result = compute(previous, current);

    REQUIRE(result.entries.size() == 1);
    const auto& entry = result.entries[0];
    REQUIRE(entry.old_spans.size() == 1);
    CHECK(entry.old_spans[0] == diff_span{.offset = 3, .length = 1});
    REQUIRE(entry.new_spans.size() == 1);
    CHECK(entry.new_spans[0] == diff_span{.offset = 3, .length = 1});
}

TEST_CASE("diff_span_whole_word_change_spans_the_word", tags) {
    const std::vector<field_value> previous{{.name = "Status", .value = "Active"}};
    const std::vector<field_value> current{{.name = "Status", .value = "Retired"}};

    const auto result = compute(previous, current);

    REQUIRE(result.entries.size() == 1);
    const auto& entry = result.entries[0];
    REQUIRE(entry.old_spans.size() == 1);
    CHECK(entry.old_spans[0] == diff_span{.offset = 0, .length = 6});
    REQUIRE(entry.new_spans.size() == 1);
    CHECK(entry.new_spans[0] == diff_span{.offset = 0, .length = 7});
}

TEST_CASE("diff_span_common_prefix_only_spans_the_differing_suffix", tags) {
    const std::vector<field_value> previous{{.name = "Code", .value = "ABC123"}};
    const std::vector<field_value> current{{.name = "Code", .value = "ABC456"}};

    const auto result = compute(previous, current);

    REQUIRE(result.entries.size() == 1);
    const auto& entry = result.entries[0];
    REQUIRE(entry.old_spans.size() == 1);
    CHECK(entry.old_spans[0] == diff_span{.offset = 3, .length = 3});
    REQUIRE(entry.new_spans.size() == 1);
    CHECK(entry.new_spans[0] == diff_span{.offset = 3, .length = 3});
}

TEST_CASE("diff_span_common_suffix_only_spans_the_differing_prefix", tags) {
    const std::vector<field_value> previous{{.name = "Code", .value = "123ABC"}};
    const std::vector<field_value> current{{.name = "Code", .value = "456ABC"}};

    const auto result = compute(previous, current);

    REQUIRE(result.entries.size() == 1);
    const auto& entry = result.entries[0];
    REQUIRE(entry.old_spans.size() == 1);
    CHECK(entry.old_spans[0] == diff_span{.offset = 0, .length = 3});
    REQUIRE(entry.new_spans.size() == 1);
    CHECK(entry.new_spans[0] == diff_span{.offset = 0, .length = 3});
}

TEST_CASE("diff_span_no_overlap_spans_the_whole_value_each_side", tags) {
    const std::vector<field_value> previous{{.name = "Symbol", .value = "USD"}};
    const std::vector<field_value> current{{.name = "Symbol", .value = "EUR"}};

    const auto result = compute(previous, current);

    REQUIRE(result.entries.size() == 1);
    const auto& entry = result.entries[0];
    REQUIRE(entry.old_spans.size() == 1);
    CHECK(entry.old_spans[0] == diff_span{.offset = 0, .length = 3});
    REQUIRE(entry.new_spans.size() == 1);
    CHECK(entry.new_spans[0] == diff_span{.offset = 0, .length = 3});
}

TEST_CASE("diff_span_added_field_has_no_old_spans", tags) {
    const std::vector<field_value> previous{};
    const std::vector<field_value> current{{.name = "Market Tier", .value = "Major"}};

    const auto result = compute(previous, current);

    REQUIRE(result.entries.size() == 1);
    CHECK(result.entries[0].old_spans.empty());
    REQUIRE(result.entries[0].new_spans.size() == 1);
    CHECK(result.entries[0].new_spans[0] == diff_span{.offset = 0, .length = 5});
}

TEST_CASE("diff_span_removed_field_has_no_new_spans", tags) {
    const std::vector<field_value> previous{{.name = "Legacy Flag", .value = "true"}};
    const std::vector<field_value> current{};

    const auto result = compute(previous, current);

    REQUIRE(result.entries.size() == 1);
    REQUIRE(result.entries[0].old_spans.size() == 1);
    CHECK(result.entries[0].old_spans[0] == diff_span{.offset = 0, .length = 4});
    CHECK(result.entries[0].new_spans.empty());
}

TEST_CASE("diff_span_identical_values_yield_no_entry_and_no_spans", tags) {
    const std::vector<field_value> fields{{.name = "Name", .value = "US Dollar"}};

    const auto result = compute(fields, fields);

    CHECK(result.entries.empty());
}

TEST_CASE("diff_span_empty_vs_non_empty_spans_the_whole_non_empty_side", tags) {
    const std::vector<field_value> previous{{.name = "Notes", .value = ""}};
    const std::vector<field_value> current{{.name = "Notes", .value = "reviewed"}};

    const auto result = compute(previous, current);

    REQUIRE(result.entries.size() == 1);
    CHECK(result.entries[0].old_spans.empty());
    REQUIRE(result.entries[0].new_spans.size() == 1);
    CHECK(result.entries[0].new_spans[0] == diff_span{.offset = 0, .length = 8});
}

TEST_CASE("diff_span_unicode_content_does_not_split_a_code_point", tags) {
    // "café" -> "café!" in UTF-8: 'é' is the 2-byte sequence C3 A9.
    // The common-prefix boundary must land after the full 'é', not
    // between its two bytes.
    const std::vector<field_value> previous{{.name = "Name", .value = "caf\xC3\xA9"}};
    const std::vector<field_value> current{{.name = "Name", .value = "caf\xC3\xA9!"}};

    const auto result = compute(previous, current);

    REQUIRE(result.entries.size() == 1);
    const auto& entry = result.entries[0];
    CHECK(entry.old_spans.empty());
    REQUIRE(entry.new_spans.size() == 1);
    // "caf" (3 bytes) + "é" (2 bytes) = offset 5, one added byte '!'.
    CHECK(entry.new_spans[0] == diff_span{.offset = 5, .length = 1});
}

TEST_CASE("diff_span_unicode_change_within_a_multi_byte_character", tags) {
    // "café" -> "cafe" changes the multi-byte 'é' (C3 A9) into ASCII
    // 'e'. The prefix boundary must not land between C3 and A9: since
    // the byte at old[3] (0xC3) is not itself a continuation byte,
    // the raw byte-wise prefix already stops correctly at 3.
    const std::vector<field_value> previous{{.name = "Name", .value = "caf\xC3\xA9"}};
    const std::vector<field_value> current{{.name = "Name", .value = "cafe"}};

    const auto result = compute(previous, current);

    REQUIRE(result.entries.size() == 1);
    const auto& entry = result.entries[0];
    REQUIRE(entry.old_spans.size() == 1);
    CHECK(entry.old_spans[0] == diff_span{.offset = 3, .length = 2});
    REQUIRE(entry.new_spans.size() == 1);
    CHECK(entry.new_spans[0] == diff_span{.offset = 3, .length = 1});
}

TEST_CASE("diff_span_multiline_unchanged_lines_produce_no_spans", tags) {
    const std::vector<field_value> previous{
        {.name = "Commentary", .value = "line one\nline two\nline three"}};
    const std::vector<field_value> current{
        {.name = "Commentary", .value = "line one\nline TWO\nline three"}};

    const auto result = compute(previous, current);

    REQUIRE(result.entries.size() == 1);
    const auto& entry = result.entries[0];
    // Only "line two" -> "line TWO" changed; "line one" and
    // "line three" contribute no spans.
    REQUIRE(entry.old_spans.size() == 1);
    REQUIRE(entry.new_spans.size() == 1);
    // "line two" starts at offset 9 ("line one\n" is 9 bytes); the
    // differing suffix "two" starts 5 bytes into that line.
    CHECK(entry.old_spans[0] == diff_span{.offset = 9 + 5, .length = 3});
    CHECK(entry.new_spans[0] == diff_span{.offset = 9 + 5, .length = 3});
}

TEST_CASE("diff_span_multiline_added_line_spans_the_whole_new_line", tags) {
    const std::vector<field_value> previous{{.name = "Commentary", .value = "line one\nline two"}};
    const std::vector<field_value> current{
        {.name = "Commentary", .value = "line one\nline two\nline three"}};

    const auto result = compute(previous, current);

    REQUIRE(result.entries.size() == 1);
    const auto& entry = result.entries[0];
    CHECK(entry.old_spans.empty());
    REQUIRE(entry.new_spans.size() == 1);
    // "line three" starts right after "line one\nline two\n" (18 bytes).
    CHECK(entry.new_spans[0] == diff_span{.offset = 18, .length = 10});
}

TEST_CASE("diff_span_multiline_removed_line_spans_the_whole_old_line", tags) {
    const std::vector<field_value> previous{
        {.name = "Commentary", .value = "line one\nline two\nline three"}};
    const std::vector<field_value> current{{.name = "Commentary", .value = "line one\nline two"}};

    const auto result = compute(previous, current);

    REQUIRE(result.entries.size() == 1);
    const auto& entry = result.entries[0];
    REQUIRE(entry.old_spans.size() == 1);
    CHECK(entry.old_spans[0] == diff_span{.offset = 18, .length = 10});
    CHECK(entry.new_spans.empty());
}

TEST_CASE("diff_span_multiline_per_line_token_diff_within_changed_block", tags) {
    const std::vector<field_value> previous{
        {.name = "Commentary", .value = "Reviewed by Alice\nApproved by Bob"}};
    const std::vector<field_value> current{
        {.name = "Commentary", .value = "Reviewed by Alice\nApproved by Carol"}};

    const auto result = compute(previous, current);

    REQUIRE(result.entries.size() == 1);
    const auto& entry = result.entries[0];
    // First line unchanged: no spans from it. Second line changed
    // "Bob" -> "Carol"; the common prefix "Approved by " is 12 chars,
    // line starts at offset 18 ("Reviewed by Alice\n" is 18 bytes:
    // 17-char line + 1 newline).
    REQUIRE(entry.old_spans.size() == 1);
    CHECK(entry.old_spans[0] == diff_span{.offset = 18 + 12, .length = 3});
    REQUIRE(entry.new_spans.size() == 1);
    CHECK(entry.new_spans[0] == diff_span{.offset = 18 + 12, .length = 5});
}
