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
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/xml/emitter.hpp"
#include "ores.ore.core/xml/record.hpp"
#include "ores.ore.core/xml/record_diff.hpp"
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <cstddef>
#include <cstdint>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][xml][record]");

using namespace ores::logging;
using namespace ores::ore::xml;

/*
 * One table holding a field of every kind the engine has, because the reader
 * and the writer promise the same thing for each of them: the value the table
 * describes is the value the caller gets or sets. A table built here rather
 * than read from a generated family keeps the test about that promise.
 */
struct leaf {
    std::uint64_t present = 0;
    std::string value;
    std::string currency;
    std::vector<std::string> tag;
};

struct entry {
    std::uint64_t present = 0;
    std::string name;
    std::int64_t count = 0;
    double weight = 0;
    ores::utility::decimal::decimal spread;
    std::int16_t kind = 0;
    std::int16_t active = 0;
    leaf child;
    std::vector<leaf> item;
};

struct root {
    std::uint64_t present = 0;
    std::vector<entry> entry;
};

template <typename T>
auto append_to(void* container, void** slot) -> void {
    auto* vector = static_cast<std::vector<T>*>(container);
    vector->emplace_back();
    *slot = &vector->back();
}

template <typename T>
auto count_of(const void* container) -> std::size_t {
    return static_cast<const std::vector<T>*>(container)->size();
}

template <typename T>
auto at_of(const void* container, std::size_t index) -> const void* {
    return &(*static_cast<const std::vector<T>*>(container))[index];
}

constexpr vector_ops leaf_item_ops{&append_to<leaf>, &count_of<leaf>, &at_of<leaf>};
constexpr vector_ops leaf_tag_ops{
    &append_to<std::string>, &count_of<std::string>, &at_of<std::string>};
constexpr vector_ops root_entry_ops{&append_to<entry>, &count_of<entry>, &at_of<entry>};

const char* kind_tokens[] = {"A", "B", nullptr};
const char* active_tokens[] = {"N", "Y", "true", nullptr};

const field leaf_fields[] = {
    {"",
     0u,
     field_kind::text,
     std::uint32_t(offsetof(leaf, value)),
     0,
     false,
     false,
     nullptr,
     nullptr,
     scalar_kind::text,
     nullptr},
    {"currency",
     3723658806u,
     field_kind::text,
     std::uint32_t(offsetof(leaf, currency)),
     1,
     true,
     false,
     nullptr,
     nullptr,
     scalar_kind::text,
     nullptr},
    {"Tag",
     4169356339u,
     field_kind::repeated_leaf,
     std::uint32_t(offsetof(leaf, tag)),
     2,
     false,
     false,
     nullptr,
     nullptr,
     scalar_kind::text,
     &leaf_tag_ops},
};
const record_type leaf_record{"leaf", leaf_fields, 3, 0};

const field entry_fields[] = {
    {"Name",
     266367750u,
     field_kind::text,
     std::uint32_t(offsetof(entry, name)),
     0,
     false,
     true,
     nullptr,
     nullptr,
     scalar_kind::text,
     nullptr},
    {"Count",
     3790059668u,
     field_kind::integer,
     std::uint32_t(offsetof(entry, count)),
     1,
     false,
     false,
     nullptr,
     nullptr,
     scalar_kind::integer,
     nullptr},
    {"Weight",
     2570065113u,
     field_kind::real,
     std::uint32_t(offsetof(entry, weight)),
     2,
     false,
     false,
     nullptr,
     nullptr,
     scalar_kind::real,
     nullptr},
    {"Spread",
     3147598830u,
     field_kind::decimal,
     std::uint32_t(offsetof(entry, spread)),
     3,
     false,
     false,
     nullptr,
     nullptr,
     scalar_kind::decimal,
     nullptr},
    {"Kind",
     1073024099u,
     field_kind::enumeration,
     std::uint32_t(offsetof(entry, kind)),
     4,
     false,
     false,
     kind_tokens,
     nullptr,
     scalar_kind::enumeration,
     nullptr},
    {"Child",
     2379610877u,
     field_kind::nested,
     std::uint32_t(offsetof(entry, child)),
     5,
     false,
     false,
     nullptr,
     &leaf_record,
     scalar_kind::text,
     nullptr},
    {"Item",
     526680070u,
     field_kind::repeated_nested,
     std::uint32_t(offsetof(entry, item)),
     6,
     false,
     false,
     nullptr,
     &leaf_record,
     scalar_kind::text,
     &leaf_item_ops},
    {"Active",
     529077071u,
     field_kind::enumeration,
     std::uint32_t(offsetof(entry, active)),
     7,
     false,
     false,
     active_tokens,
     nullptr,
     scalar_kind::enumeration,
     nullptr},
};
const record_type entry_record{"entry", entry_fields, 8, 0xFF};

const field root_fields[] = {
    {"Entry",
     2629823795u,
     field_kind::repeated_nested,
     std::uint32_t(offsetof(root, entry)),
     0,
     false,
     false,
     nullptr,
     &entry_record,
     scalar_kind::text,
     &root_entry_ops},
};
const record_type root_record{"root", root_fields, 1, 0xFF};

const document_type root_document{"Root", &root_record};

auto read_root(const std::string& text, root& record) -> bool {
    return binder::bind(root_document, text, &record).has_value();
}

auto emitted(const root& record) -> std::string {
    std::string out;
    const auto written = emitter::emit(root_document, &record, out);
    if (!written)
        throw std::runtime_error(written.error().message);
    return out;
}

}

TEST_CASE("a_reader_states_what_the_document_states", tags) {
    auto lg(make_logger(test_suite));

    const std::string text = "<Root><Entry><Name>a</Name><Count>-3</Count>"
                             "<Weight>0.5</Weight><Spread>1.25</Spread><Kind>B</Kind>"
                             "<Child currency=\"EUR\">inner</Child>"
                             "<Item>first</Item><Item>second</Item>"
                             "</Entry></Root>";
    root record{};
    REQUIRE(read_root(text, record));

    const auto read = record_reader(root_record, &record);
    const auto entries = read.children("Entry");
    REQUIRE(entries.size() == 1);
    const auto& e = entries.front();

    CHECK(e.text("Name") == std::optional<std::string>("a"));
    CHECK(e.integer("Count") == std::optional<std::int64_t>(-3));
    CHECK(e.real("Weight") == std::optional<double>(0.5));
    const auto expected = ores::utility::decimal::decimal::from_string("1.25");
    REQUIRE(expected.has_value());
    CHECK(e.decimal("Spread") == std::optional<ores::utility::decimal::decimal>(*expected));
    CHECK(e.spelling("Kind") == std::optional<std::string>("B"));

    const auto child = e.child("Child");
    REQUIRE(child);
    CHECK(child->own_text() == std::optional<std::string>("inner"));
    CHECK(child->text("currency") == std::optional<std::string>("EUR"));

    const auto items = e.children("Item");
    REQUIRE(items.size() == 2);
    CHECK(items[0].own_text() == std::optional<std::string>("first"));
    CHECK(items[1].own_text() == std::optional<std::string>("second"));
}

TEST_CASE("a_reader_tells_an_absent_element_from_an_empty_one", tags) {
    auto lg(make_logger(test_suite));

    const std::string text = "<Root><Entry><Name></Name></Entry></Root>";
    root record{};
    REQUIRE(read_root(text, record));

    const auto entries = record_reader(root_record, &record).children("Entry");
    REQUIRE(entries.size() == 1);
    const auto& e = entries.front();

    // The element arrived and is empty, which is a value the document states.
    CHECK(e.has("Name"));
    CHECK(e.text("Name") == std::optional<std::string>(""));
    // The element never arrived, which is what a null column holds.
    CHECK_FALSE(e.has("Count"));
    CHECK(e.integer("Count") == std::nullopt);
}

TEST_CASE("a_writer_marks_every_row_it_fills", tags) {
    auto lg(make_logger(test_suite));

    root written{};
    auto w = record_writer(root_record, &written);
    auto e = w.append("Entry");
    e.set_text("Name", "a");
    e.set_integer("Count", -3);
    e.set_real("Weight", 0.5);
    const auto spread = ores::utility::decimal::decimal::from_string("1.25");
    REQUIRE(spread.has_value());
    e.set_decimal("Spread", *spread);
    e.set_spelling("Kind", "B", "kind");
    auto c = e.child("Child");
    c.set_own_text("inner");
    c.set_text("currency", "EUR");
    auto first = e.append("Item");
    first.set_own_text("first");
    auto second = e.append("Item");
    second.set_own_text("second");

    root back{};
    REQUIRE(read_root(emitted(written), back));
    const auto difference = compare_records(root_record, &written, &back);
    REQUIRE_FALSE(difference);
    CHECK(written.entry.front().name == "a");
    CHECK(written.entry.front().count == -3);
    CHECK(written.entry.front().item.size() == 2);
}

TEST_CASE("a_writer_writes_only_the_rows_it_was_given", tags) {
    auto lg(make_logger(test_suite));

    root written{};
    auto w = record_writer(root_record, &written);
    auto e = w.append("Entry");
    e.set_text("Name", "only the name");
    // An optional value the caller does not hold states nothing, and the
    // writer still refuses a row its table does not have.
    e.set_optional_integer("Count", std::optional<std::int64_t>());
    e.set_optional_spelling("Kind", std::optional<std::string>(), "kind");

    const std::string text = emitted(written);
    CHECK(text.find("<Count>") == std::string::npos);
    CHECK(text.find("<Name>only the name</Name>") != std::string::npos);
}

TEST_CASE("a_flag_reads_every_spelling_and_writes_two", tags) {
    auto lg(make_logger(test_suite));

    // ORE writes a boolean as any spelling the schema allows, so the reading
    // accepts them all while the writing states the one every schema carries.
    const std::string text = "<Root><Entry><Name>on</Name><Active>true</Active></Entry>"
                             "<Entry><Name>off</Name><Active>N</Active></Entry></Root>";
    root parsed{};
    REQUIRE(read_root(text, parsed));
    const auto entries = record_reader(root_record, &parsed).children("Entry");
    REQUIRE(entries.size() == 2);
    CHECK(entries[0].flag("Active") == std::optional<bool>(true));
    CHECK(entries[1].flag("Active") == std::optional<bool>(false));

    root written{};
    auto w = record_writer(root_record, &written);
    auto e = w.append("Entry");
    e.set_text("Name", "written");
    e.set_flag("Active", true);

    root back{};
    REQUIRE(read_root(emitted(written), back));
    const auto written_entries = record_reader(root_record, &back).children("Entry");
    REQUIRE(written_entries.size() == 1);
    CHECK(written_entries.front().flag("Active") == std::optional<bool>(true));
    CHECK(emitted(written).find("<Active>Y</Active>") != std::string::npos);
}

TEST_CASE("a_reader_and_a_writer_refuse_a_name_the_table_lacks", tags) {
    auto lg(make_logger(test_suite));

    root record{};
    const auto read = record_reader(root_record, &record);
    CHECK_THROWS_WITH(read.children("Entries"),
                      "the root table has no 'Entries' field");

    auto write = record_writer(root_record, &record);
    CHECK_THROWS_WITH(write.append("Entries"),
                      "the root table has no 'Entries' field");

    root other{};
    auto write_entry = record_writer(entry_record, &other.entry.emplace_back());
    CHECK_THROWS_WITH(write_entry.set_spelling("Kind", "C", "kind"),
                      "'C' is not a valid kind");
}
