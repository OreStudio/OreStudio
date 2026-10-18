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
    CHECK_THROWS_WITH(read.children("Entries"), "the root table has no 'Entries' field");

    auto write = record_writer(root_record, &record);
    CHECK_THROWS_WITH(write.append("Entries"), "the root table has no 'Entries' field");

    root other{};
    auto write_entry = record_writer(entry_record, &other.entry.emplace_back());
    CHECK_THROWS_WITH(write_entry.set_spelling("Kind", "C", "kind"), "'C' is not a valid kind");
}

TEST_CASE("a_reader_and_a_writer_refuse_a_member_of_another_kind", tags) {
    auto lg(make_logger(test_suite));

    root record{};
    const auto read = record_reader(root_record, &record);
    CHECK_THROWS_WITH(read.integer("Entry"),
                      "'Entry' of the root table holds a repeated child element, not an integer");

    auto write = record_writer(root_record, &record);
    CHECK_THROWS_WITH(write.set_text("Entry", "x"),
                      "'Entry' of the root table holds a repeated child element, not text");
}

TEST_CASE("a_reader_states_which_members_the_table_declares", tags) {
    auto lg(make_logger(test_suite));

    root record{};
    const auto read = record_reader(root_record, &record);
    // Declared by the table, absent from this document.
    CHECK(read.declares("Entry"));
    CHECK_FALSE(read.has("Entry"));
    // Not declared at all, which is a different answer.
    CHECK_FALSE(read.declares("Entries"));

    const auto entries = record_reader(entry_record, &record.entry.emplace_back());
    CHECK(entries.declares("Active"));
    CHECK_FALSE(entries.declares("active"));
}

TEST_CASE("a_repeated_leaf_reads_and_writes_its_values", tags) {
    auto lg(make_logger(test_suite));

    const std::string text = "<Root><Entry><Name>leaves</Name>"
                             "<Item><Tag>a</Tag><Tag>b</Tag></Item></Entry></Root>";
    root parsed{};
    REQUIRE(read_root(text, parsed));

    const auto entries = record_reader(root_record, &parsed).children("Entry");
    REQUIRE(entries.size() == 1);
    const auto items = entries.front().children("Item");
    REQUIRE(items.size() == 1);
    CHECK(items.front().values("Tag") == std::vector<std::string>({"a", "b"}));

    root written{};
    auto w = record_writer(root_record, &written);
    auto e = w.append("Entry");
    e.set_text("Name", "written");
    auto item = e.append("Item");
    item.append_value("Tag", "a");
    item.append_value("Tag", "b");

    root back{};
    REQUIRE(read_root(emitted(written), back));
    const auto written_items =
        record_reader(root_record, &back).children("Entry").front().children("Item");
    REQUIRE(written_items.size() == 1);
    CHECK(written_items.front().values("Tag") == std::vector<std::string>({"a", "b"}));
}

namespace {

/*
 * A type that holds itself. A record cannot, so the child is an index into an
 * arena the record owns.
 */
struct node {
    std::uint64_t present = 0;
    std::string value;
    std::vector<std::uint32_t> child;
    std::vector<node> child_arena;
};

extern const record_type node_record;

constexpr vector_ops node_child_arena_ops{&append_to<node>, &count_of<node>, &at_of<node>};

const field node_fields[] = {
    {"Value",
     3511155050u,
     field_kind::text,
     std::uint32_t(offsetof(node, value)),
     0,
     false,
     false,
     nullptr,
     nullptr,
     scalar_kind::text,
     nullptr},
    {"Child",
     2379610877u,
     field_kind::indexed_nested,
     std::uint32_t(offsetof(node, child)),
     1,
     false,
     false,
     nullptr,
     &node_record,
     scalar_kind::text,
     &node_child_arena_ops,
     std::uint32_t(offsetof(node, child_arena))},
};
const record_type node_record{"node", node_fields, 2, 0xFF};

// The same layout with the schema's cardinality stated: an element that may
// appear once is refused a second time rather than appended.
const field once_node_fields[] = {
    {"Value",
     3511155050u,
     field_kind::text,
     std::uint32_t(offsetof(node, value)),
     0,
     false,
     false,
     nullptr,
     nullptr,
     scalar_kind::text,
     nullptr},
    {"Child",
     2379610877u,
     field_kind::indexed_nested,
     std::uint32_t(offsetof(node, child)),
     1,
     false,
     false,
     nullptr,
     &node_record,
     scalar_kind::text,
     &node_child_arena_ops,
     std::uint32_t(offsetof(node, child_arena)),
     true},
};
const record_type once_node_record{"node", once_node_fields, 2, 0xFF};
const document_type once_node_document{"Node", &once_node_record};

const document_type node_document{"Node", &node_record};

auto read_node(const std::string& text, node& record) -> bool {
    const auto bound = binder::bind(node_document, text, &record);
    if (!bound)
        throw std::runtime_error(bound.error().message);
    return true;
}

auto emitted_node(const node& record) -> std::string {
    std::string out;
    const auto written = emitter::emit(node_document, &record, out);
    if (!written)
        throw std::runtime_error(written.error().message);
    return out;
}

}

TEST_CASE("a_type_that_holds_itself_indexes_its_rows", tags) {
    auto lg(make_logger(test_suite));

    const std::string text = "<Node><Value>a</Value>"
                             "<Child><Value>b</Value>"
                             "<Child><Value>c</Value></Child>"
                             "</Child></Node>";
    node parsed{};
    REQUIRE(read_node(text, parsed));

    const auto read = record_reader(node_record, &parsed);
    CHECK(read.text("Value") == std::optional<std::string>("a"));
    const auto children = read.indexed("Child");
    REQUIRE(children.size() == 1);
    CHECK(children[0].text("Value") == std::optional<std::string>("b"));
    const auto grandchildren = children[0].indexed("Child");
    REQUIRE(grandchildren.size() == 1);
    CHECK(grandchildren[0].text("Value") == std::optional<std::string>("c"));
    // Each record owns the rows of its own recursive member, so the root's
    // arena holds a's child and that child's arena holds c.
    REQUIRE(parsed.child_arena.size() == 1);
    CHECK(parsed.child_arena[0].child_arena.size() == 1);

    node back{};
    REQUIRE(read_node(emitted_node(parsed), back));
    CHECK_FALSE(compare_records(node_record, &parsed, &back));

    // A hand-built record can state a row that does not exist, and the writing
    // refuses it rather than reading past the arena.
    node broken{};
    broken.present = 0x1u;
    broken.value = "a";
    broken.present |= 0x2u;
    broken.child.push_back(7);
    CHECK_THROWS_AS(emitted_node(broken), std::runtime_error);
    CHECK_THROWS_AS(record_reader(node_record, &broken).indexed("Child"), std::runtime_error);

    // A row no index names has no place in a document. Dropping it would lose
    // the value without a word, so the writing refuses the record.
    node orphan = parsed;
    orphan.child.clear();
    CHECK_THROWS_AS(emitted_node(orphan), std::runtime_error);
}

TEST_CASE("a_member_the_schema_allows_once_refuses_a_second_row", tags) {
    auto lg(make_logger(test_suite));

    node twice{};
    const auto bound = binder::bind(once_node_document, "<Node><Child/><Child/></Node>", &twice);
    REQUIRE_FALSE(bound);
    CHECK(bound.error().message.find("more than once") != std::string::npos);

    node written{};
    auto w = record_writer(once_node_record, &written);
    w.append_indexed("Child");
    CHECK_THROWS_AS(w.append_indexed("Child"), std::runtime_error);
}

TEST_CASE("a_writer_appends_indexed_rows", tags) {
    auto lg(make_logger(test_suite));

    node written{};
    auto w = record_writer(node_record, &written);
    w.set_text("Value", "a");
    auto child = w.append_indexed("Child");
    child.set_text("Value", "b");
    auto grandchild = child.append_indexed("Child");
    grandchild.set_text("Value", "c");

    node back{};
    REQUIRE(read_node(emitted_node(written), back));
    CHECK_FALSE(compare_records(node_record, &written, &back));
    const auto rows = record_reader(node_record, &back).indexed("Child");
    REQUIRE(rows.size() == 1);
    CHECK(rows[0].indexed("Child")[0].text("Value") == std::optional<std::string>("c"));
}

namespace {

/*
 * A type written as xs:simpleContent whose text is a number. The record keeps
 * the digits, and the reader and writer speak the number as text.
 */
struct measure {
    std::uint64_t present = 0;
    ores::utility::decimal::decimal value;
};

const field measure_fields[] = {
    {"",
     0,
     field_kind::decimal,
     std::uint32_t(offsetof(measure, value)),
     0,
     false,
     false,
     nullptr,
     nullptr,
     scalar_kind::decimal,
     nullptr},
};
const record_type measure_type{"Measure", measure_fields, 1, 0};
const document_type measure_document{"Measure", &measure_type};

}

TEST_CASE("a_type_whose_own_text_is_a_number_keeps_its_digits", tags) {
    auto lg(make_logger(test_suite));

    measure parsed{};
    const auto bound =
        binder::bind(measure_document, "<Measure>7342.8000000000002</Measure>", &parsed);
    REQUIRE(bound);
    CHECK(record_reader(measure_type, &parsed).own_text() ==
          std::optional<std::string>("7342.8000000000002"));

    std::string out;
    REQUIRE(emitter::emit(measure_document, &parsed, out));
    CHECK(out.find("7342.8000000000002") != std::string::npos);

    measure written{};
    record_writer(measure_type, &written).set_own_text("0.050000000000000003");
    CHECK(record_reader(measure_type, &written).own_text() ==
          std::optional<std::string>("0.050000000000000003"));

    // The text may arrive on its own indented line.
    measure indented{};
    REQUIRE(binder::bind(measure_document, "<Measure>\n  0.005\n</Measure>", &indented));
    CHECK(record_reader(measure_type, &indented).own_text() == std::optional<std::string>("0.005"));

    // An element with no text states nothing, and the writing does not invent
    // the member's zero for it.
    measure absent{};
    REQUIRE(binder::bind(measure_document, "<Measure/>", &absent));
    CHECK_FALSE(record_reader(measure_type, &absent).own_text().has_value());
    std::string written_out;
    REQUIRE(emitter::emit(measure_document, &absent, written_out));
    CHECK(written_out.find("<Measure/>") != std::string::npos);
    CHECK(written_out.find("<Measure>0</Measure>") == std::string::npos);
}
