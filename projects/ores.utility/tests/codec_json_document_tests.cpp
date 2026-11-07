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
#include "ores.utility/codec/json_document.hpp"
#include "ores.utility/codec/json_document_wire.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstdint>
#include <span>
#include <string>

namespace doc = ores::utility::codec;

TEST_CASE("a parsed document keeps every kind its text states", "[codec][json][document]") {
    const auto parsed = doc::json_document::parse(
        R"({"name":"acme","count":7,"live":true,"items":["a","b"],"nested":{"k":"v"}})");
    REQUIRE(parsed.has_value());

    const auto root = parsed->root();
    REQUIRE(root.is_object());
    CHECK(root.size() == 5);
    CHECK(root.member("name").text_or() == "acme");
    CHECK(root.member("count").integer() == 7);
    CHECK(root.member("live").flag() == true);
    CHECK(root.member("items").is_array());
    CHECK(root.member("items").size() == 2);
    CHECK(root.member("items").element(1).text_or() == "b");
    CHECK(root.member("nested").member("k").text_or() == "v");
}

TEST_CASE("a member the document does not state reads as nothing", "[codec][json][document]") {
    const auto parsed = doc::json_document::parse(R"({"name":"acme"})");
    REQUIRE(parsed.has_value());

    CHECK_FALSE(parsed->root().member("absent").exists());
    CHECK_FALSE(parsed->root().member("absent").has("anything"));
    CHECK(parsed->root().member("absent").text() == std::nullopt);
    CHECK(parsed->root().member("name").integer() == std::nullopt);
}

TEST_CASE("a document built member by member writes what it was given", "[codec][json][document]") {
    auto built = doc::json_document::make_object();
    auto root = built.root();
    root.set("subject", "currency");
    root.set("offset", std::int64_t{10});
    root.set("descending", true);
    auto order = root.add_object("order");
    order.set("field", "code");
    auto rows = root.add_array("rows");
    rows.push("USD");
    rows.push("EUR");

    CHECK(built.write() ==
          R"({"subject":"currency","offset":10,"descending":true,"order":{"field":"code"},)"
          R"("rows":["USD","EUR"]})");
}

TEST_CASE("a value copies from one document into another", "[codec][json][document]") {
    const auto source = doc::json_document::parse(R"({"schema":{"type":"object"}})");
    REQUIRE(source.has_value());

    auto target = doc::json_document::make_object();
    target.root().add("media", source->root().member("schema"));

    CHECK(target.write() == R"({"media":{"type":"object"}})");
}

TEST_CASE("text that is not JSON states why it reads as nothing", "[codec][json][document]") {
    const auto failed = doc::json_document::parse("{not json");

    REQUIRE_FALSE(failed.has_value());
    CHECK(failed.error().starts_with("Not JSON"));
}

TEST_CASE("a document survives a MessagePack round trip", "[codec][json][document]") {
    const auto parsed =
        doc::json_document::parse(R"({"subject":"currency","offset":10,"ratio":0.25,"live":true,)"
                                  R"("rows":["USD",7,null,{"k":"v"}]})");
    REQUIRE(parsed.has_value());

    const auto packed = doc::write_document_msgpack(*parsed);
    const auto read = doc::read_document_msgpack(std::as_bytes(std::span(packed)));

    REQUIRE(read.has_value());
    CHECK(read->write() == parsed->write());
}

TEST_CASE("MessagePack that states no document is refused", "[codec][json][document]") {
    const auto text = std::string("not msgpack at all");
    const auto read = doc::read_document_msgpack(std::as_bytes(std::span(text)));

    CHECK_FALSE(read.has_value());
}
