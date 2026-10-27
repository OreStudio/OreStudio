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
#include "ores.marketdata.api/datum/market_index.hpp"
#include "ores.marketdata.api/datum/ore_types.hpp"
#include "ores.marketdata.api/datum/schema.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/service/oresmd_schema_describer.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <set>
#include <string>
#include <string_view>
#include <vector>

/**
 * @file service_oresmd_schema_describer_tests.cpp
 * @brief The schema the describer answers, against the codec's own tables.
 *
 * The describer holds no copy of the grammar, so these tests compare its answer
 * with the tables and with what the codec accepts. A row, a field or a code that
 * the answer drops or invents would fail here before a client offered it.
 */

namespace {

using namespace ores::marketdata;
using namespace ores::marketdata::datum;
using ores::marketdata::service::oresmd_schema_describer;

const messaging::describe_oresmd_schema_response& schema_answer() {
    static const auto answer = oresmd_schema_describer::describe();
    return answer;
}

const messaging::oresmd_instrument* instrument_coded(std::string_view code) {
    for (const auto& row : schema_answer().instruments) {
        if (row.code == code)
            return &row;
    }
    return nullptr;
}

bool has_field(const messaging::oresmd_instrument& row, const std::string& name) {
    return std::any_of(
        row.fields.begin(), row.fields.end(), [&](const auto& spec) { return spec.name == name; });
}

}

TEST_CASE("the answer holds every asset class, instrument type and quote type", "[marketdata][service][oresmd_schema_describer]") {
    const auto& answer = schema_answer();
    CHECK(answer.success);
    CHECK(answer.assets.size() == asset_class_count);
    CHECK(answer.instruments.size() == instrument_type_count);
    CHECK(answer.quotes.size() == quote_type_count);
    CHECK(answer.indexes.size() == index_family_count);
}

TEST_CASE("an instrument's code is the text the codec writes, and names its row", "[marketdata][service][oresmd_schema_describer]") {
    const auto& answer = schema_answer();
    std::set<std::string> codes;
    for (std::size_t i = 0; i < instrument_type_count; ++i) {
        const auto type = static_cast<instrument_type>(i);
        const auto& row = answer.instruments[i];
        CHECK(row.code == oresmd_uri_codec::instrument_spelling(type));
        CHECK(row.ore_name == ore_name(type));
        CHECK(row.asset == name_of(schema_of(type).asset));
        CHECK(row.subject == name_of(schema_of(type).subject));
        codes.insert(row.code);
    }
    CHECK(codes.size() == instrument_type_count);
}

TEST_CASE("a row's fields follow the schema in order, with role, none and kind", "[marketdata][service][oresmd_schema_describer]") {
    const auto& answer = schema_answer();
    for (std::size_t i = 0; i < instrument_type_count; ++i) {
        const auto& expected = schema_of(static_cast<instrument_type>(i)).fields;
        const auto& described = answer.instruments[i].fields;
        REQUIRE(described.size() == expected.size());
        for (std::size_t j = 0; j < expected.size(); ++j) {
            CHECK(described[j].name == name_of(expected[j].name));
            CHECK(described[j].role ==
                  (expected[j].role == field_role::identity ? "identity" : "coordinate"));
            CHECK(described[j].may_be_none == expected[j].may_be_none);
        }
    }
}

TEST_CASE("an interest rate swap holds the fields the codec writes, with the index name optional",
          "[marketdata][service][oresmd_schema_describer]") {
    const auto* swap = instrument_coded("ir_swap");
    REQUIRE(swap != nullptr);
    CHECK(swap->asset == "ir");
    CHECK(swap->subject == "ccy");
    std::vector<std::string> names;
    for (const auto& spec : swap->fields)
        names.push_back(spec.name);
    CHECK(names == std::vector<std::string>{"ccy", "index_name", "fwd_start", "tenor", "term"});
    const auto index_name = std::find_if(
        swap->fields.begin(), swap->fields.end(), [](auto& f) { return f.name == "index_name"; });
    CHECK(index_name->may_be_none);
}

TEST_CASE("a code field carries the tokens ORE accepts and no other field carries any", "[marketdata][service][oresmd_schema_describer]") {
    std::size_t code_fields = 0;
    for (std::size_t i = 0; i < instrument_type_count; ++i) {
        const auto& expected = schema_of(static_cast<instrument_type>(i)).fields;
        for (std::size_t j = 0; j < expected.size(); ++j) {
            const auto& described = schema_answer().instruments[i].fields[j];
            const auto tokens = codes_of(expected[j].name);
            CHECK(described.codes.size() == tokens.size());
            if (kind_of(expected[j].name) == value_kind::code) {
                ++code_fields;
                CHECK(described.kind == "code");
                CHECK_FALSE(described.codes.empty());
            } else {
                CHECK(described.codes.empty());
            }
        }
    }
    CHECK(code_fields > 0);
}

TEST_CASE("every key of a URI the codec reads is a field its row describes", "[marketdata][service][oresmd_schema_describer]") {
    const std::string uri =
        "oresmd://credit/ACME?type=quote&instrument=cds&quote=credit_spread&seniority=SNRFOR"
        "&ccy=USD&doc_clause=XR14&term=5Y";
    REQUIRE(oresmd_uri_codec::read(uri).has_value());
    const auto* cds = instrument_coded("cds");
    REQUIRE(cds != nullptr);
    CHECK(cds->asset == "credit");
    for (const auto* key : {"seniority", "ccy", "doc_clause", "term"})
        CHECK(has_field(*cds, key));
    CHECK(cds->subject == "underlying_name");
}

TEST_CASE("the quotes are the spellings a URI writes, each once", "[marketdata][service][oresmd_schema_describer]") {
    const auto& quotes = schema_answer().quotes;
    CHECK(std::set<std::string>(quotes.begin(), quotes.end()).size() == quote_type_count);
    CHECK(std::find(quotes.begin(), quotes.end(), "credit_spread") != quotes.end());
    CHECK(std::find(quotes.begin(), quotes.end(), "rate") != quotes.end());
    for (std::size_t i = 0; i < quote_type_count; ++i)
        CHECK(quotes[i] == oresmd_uri_codec::quote_spelling(static_cast<quote_type>(i)));
}

TEST_CASE("an index family holds what the codec reads for a fixing", "[marketdata][service][oresmd_schema_describer]") {
    const auto& indexes = schema_answer().indexes;
    const auto ibor =
        std::find_if(indexes.begin(), indexes.end(), [](auto& f) { return f.code == "ibor"; });
    REQUIRE(ibor != indexes.end());
    CHECK(ibor->asset == "ir");
    CHECK(ibor->subject == "ccy");
    REQUIRE(ibor->fields.size() == 2);
    CHECK(ibor->fields[0].name == "name");
    CHECK_FALSE(ibor->fields[0].optional);
    CHECK(ibor->fields[1].name == "tenor");
    CHECK(ibor->fields[1].optional);
    CHECK(
        oresmd_uri_codec::read_index("oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M")
            .has_value());
}

TEST_CASE("every title names only the subject and the fields of its row", "[marketdata][service][oresmd_schema_describer]") {
    const auto& answer = schema_answer();
    for (std::size_t i = 0; i < instrument_type_count; ++i) {
        const auto& row = answer.instruments[i];
        CHECK_FALSE(row.title.empty());
        CHECK(row.title.find("{subject}") != std::string::npos);
        std::size_t at = 0;
        while ((at = row.title.find('{', at)) != std::string::npos) {
            const auto close = row.title.find('}', at);
            REQUIRE(close != std::string::npos);
            const auto name = row.title.substr(at + 1, close - at - 1);
            INFO(row.code << " names {" << name << "}");
            CHECK((name == "subject" || has_field(row, name)));
            at = close;
        }
    }
}

TEST_CASE("the answer gives one title for each quote type, empty only for none", "[marketdata][service][oresmd_schema_describer]") {
    const auto& answer = schema_answer();
    REQUIRE(answer.quote_titles.size() == answer.quotes.size());
    for (std::size_t i = 0; i < quote_type_count; ++i) {
        const auto type = static_cast<quote_type>(i);
        CHECK(answer.quote_titles[i] == quote_title_of(type));
        CHECK(answer.quote_titles[i].empty() == (type == quote_type::none));
    }
}
