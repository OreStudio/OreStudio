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
#include "datum_catalogue.hpp"
#include "ores.marketdata.core/datum/ore_index_codec.hpp"
#include "ores.marketdata.core/datum/ore_key_codec.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/service/oresmd_uri_service.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <set>
#include <string>
#include <vector>

/**
 * @file service_oresmd_uri_service_tests.cpp
 * @brief Parse and compose against the codec and the ORE reference catalogue.
 *
 * The service adds no grammar, so every URI the codec writes for the catalogue's
 * keys and indexes must parse and compose back to the same text, and every
 * refusal must be the codec's own words. A rule the service invented or dropped
 * would fail here.
 */

namespace {

using namespace ores::marketdata;
using namespace ores::marketdata::datum;
using ores::marketdata::service::oresmd_uri_service;
using ores::marketdata::test::accepted_by_ore;
using ores::marketdata::test::catalogue_corpus;
using ores::marketdata::test::catalogue_forms;
using ores::marketdata::test::catalogue_index_corpus;
using ores::marketdata::test::catalogue_index_forms;

messaging::parse_oresmd_uri_response parse(const std::string& uri) {
    messaging::parse_oresmd_uri_request request;
    request.uri = uri;
    return oresmd_uri_service::parse(request);
}

messaging::compose_oresmd_uri_response compose(const messaging::parse_oresmd_uri_response& parts) {
    messaging::compose_oresmd_uri_request request;
    request.kind = parts.kind;
    request.instrument = parts.instrument;
    request.quote = parts.quote;
    request.subject = parts.subject;
    request.fields = parts.fields;
    return oresmd_uri_service::compose(request);
}

messaging::compose_oresmd_uri_request
quote_request(std::string instrument,
              std::string quote,
              std::string subject,
              std::vector<std::pair<std::string, std::string>> fields) {
    messaging::compose_oresmd_uri_request request;
    request.kind = "quote";
    request.instrument = std::move(instrument);
    request.quote = std::move(quote);
    request.subject = std::move(subject);
    for (auto& [name, text] : fields)
        request.fields.push_back({std::move(name), std::move(text)});
    return request;
}

/// Every URI the codec writes for the catalogue: the quote of each key, and its
/// series.
std::set<std::string> catalogue_uris() {
    std::set<std::string> out;
    for (const auto* lines : {&catalogue_forms(), &catalogue_corpus()}) {
        for (const auto& line : *lines) {
            if (!accepted_by_ore(line))
                continue;
            const auto datum = ore_key_codec::read(line.at("key"));
            if (!datum)
                continue;
            if (const auto uri = oresmd_uri_codec::write(*datum))
                out.insert(*uri);
            if (const auto uri = oresmd_uri_codec::write(series_of(*datum)))
                out.insert(*uri);
        }
    }
    return out;
}

std::set<std::string> index_uris() {
    std::set<std::string> out;
    for (const auto* lines : {&catalogue_index_forms(), &catalogue_index_corpus()}) {
        for (const auto& line : *lines) {
            const auto index = ore_index_codec::read(line.at("key"));
            if (!index)
                continue;
            if (const auto uri = oresmd_uri_codec::write_index(*index))
                out.insert(*uri);
        }
    }
    return out;
}

}

TEST_CASE("every URI the codec writes for the catalogue parses and composes back to itself", "[marketdata][service][oresmd_uri_service]") {
    const auto uris = catalogue_uris();
    REQUIRE(uris.size() > 100);
    std::vector<std::string> failures;
    for (const auto& uri : uris) {
        const auto parts = parse(uri);
        if (!parts.success) {
            failures.push_back(uri + " does not parse: " + parts.message);
            continue;
        }
        const auto back = compose(parts);
        if (!back.success || back.uri != uri)
            failures.push_back(uri + " composes as " + (back.success ? back.uri : back.message));
    }
    for (std::size_t i = 0; i < failures.size() && i < 20; ++i)
        UNSCOPED_INFO(failures[i]);
    CHECK(failures.empty());
}

TEST_CASE("every fixing URI the codec writes for the catalogue parses and composes back", "[marketdata][service][oresmd_uri_service]") {
    const auto uris = index_uris();
    REQUIRE(uris.size() > 40);
    std::vector<std::string> failures;
    for (const auto& uri : uris) {
        const auto parts = parse(uri);
        if (!parts.success || parts.kind != "fixing") {
            failures.push_back(uri + " does not parse as a fixing: " + parts.message);
            continue;
        }
        const auto back = compose(parts);
        if (!back.success || back.uri != uri)
            failures.push_back(uri + " composes as " + (back.success ? back.uri : back.message));
    }
    for (std::size_t i = 0; i < failures.size() && i < 20; ++i)
        UNSCOPED_INFO(failures[i]);
    CHECK(failures.empty());
}

TEST_CASE("parse names the kind, the instrument, the quote, the subject and the other fields",
          "[marketdata][service][oresmd_uri_service]") {
    const auto parts = parse("oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate"
                             "&index_name=EUR-EURIBOR-6M&fwd_start=0D&tenor=6M&term=10Y");
    REQUIRE(parts.success);
    CHECK(parts.kind == "quote");
    CHECK(parts.instrument == "ir_swap");
    CHECK(parts.quote == "rate");
    CHECK(parts.asset == "ir");
    CHECK(parts.subject == "EUR");
    REQUIRE(parts.fields.size() == 4);
    CHECK(parts.fields[0].name == "index_name");
    CHECK(parts.fields[3].name == "term");
    CHECK(parts.fields[3].text == "10Y");
}

TEST_CASE("parse of a series leaves out the coordinate fields", "[marketdata][service][oresmd_uri_service]") {
    const auto parts =
        parse("oresmd://ir/USD?type=series&instrument=discount&quote=rate&curve_id=USD-SOFR");
    REQUIRE(parts.success);
    CHECK(parts.kind == "series");
    for (const auto& field : parts.fields)
        CHECK(field.name != "term");
}

TEST_CASE("parse refuses with the codec's own words", "[marketdata][service][oresmd_uri_service]") {
    const std::vector<std::string> bad{
        "http://example.org/",
        "oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate",
        "oresmd://ir/EUR?type=quote&instrument=nonsense&quote=rate",
        "oresmd://ir/EUR?instrument=discount&type=series&quote=rate&curve_id=X",
        "oresmd://ir/EUR?type=series&instrument=discount&quote=rate&curve_id=X&curve_id=Y",
        "oresmd://ir/EUR?type=series&instrument=discount&quote=rate&curve_id=",
        "oresmd://ir/EUR?type=series&instrument=discount&quote=rate&curve_id=X&foo=bar",
        ""};
    for (const auto& uri : bad) {
        const auto parts = parse(uri);
        CHECK_FALSE(parts.success);
        const auto codec = oresmd_uri_codec::read(uri);
        REQUIRE_FALSE(codec.has_value());
        CHECK(parts.message == codec.error());
    }
}

TEST_CASE("parse of a refused fixing gives the index reader's words", "[marketdata][service][oresmd_uri_service]") {
    const std::string uri = "oresmd://ir/EUR?type=fixing&index=ibor&tenor=6M";
    const auto parts = parse(uri);
    CHECK_FALSE(parts.success);
    CHECK(parts.message == oresmd_uri_codec::read_index(uri).error());
}

TEST_CASE("only the type key makes a URI a fixing when the reader's words are chosen", "[marketdata][service][oresmd_uri_service]") {
    const std::string fixing = "oresmd://ir/EUR?type=fixing&index=ibor&tenor=6M";
    CHECK(parse(fixing).message == oresmd_uri_codec::read_index(fixing).error());
    const std::string not_a_fixing =
        "oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&index_name=type=fixing";
    CHECK(parse(not_a_fixing).message == oresmd_uri_codec::read(not_a_fixing).error());
    const std::string other_key = "oresmd://ir/EUR?xtype=fixing&type=quote";
    CHECK(parse(other_key).message == oresmd_uri_codec::read(other_key).error());
}

TEST_CASE("compose writes the URI in the codec's order whatever order the fields come in", "[marketdata][service][oresmd_uri_service]") {
    const auto forward = oresmd_uri_service::compose(quote_request(
        "ir_swap",
        "rate",
        "EUR",
        {{"index_name", "EUR-EURIBOR-6M"}, {"fwd_start", "0D"}, {"tenor", "6M"}, {"term", "10Y"}}));
    const auto backward = oresmd_uri_service::compose(quote_request(
        "ir_swap",
        "rate",
        "EUR",
        {{"term", "10Y"}, {"tenor", "6M"}, {"fwd_start", "0D"}, {"index_name", "EUR-EURIBOR-6M"}}));
    REQUIRE(forward.success);
    CHECK(forward.uri == backward.uri);
    CHECK(oresmd_uri_codec::read(forward.uri).has_value());
}

TEST_CASE("compose treats a field with no text as a field the URI leaves out", "[marketdata][service][oresmd_uri_service]") {
    const auto without = oresmd_uri_service::compose(quote_request(
        "ir_swap", "rate", "EUR", {{"fwd_start", "0D"}, {"tenor", "6M"}, {"term", "10Y"}}));
    const auto blank = oresmd_uri_service::compose(
        quote_request("ir_swap",
                      "rate",
                      "EUR",
                      {{"index_name", ""}, {"fwd_start", "0D"}, {"tenor", "6M"}, {"term", "10Y"}}));
    REQUIRE(without.success);
    CHECK(blank.uri == without.uri);
}

TEST_CASE("compose says why the parts make no URI", "[marketdata][service][oresmd_uri_service]") {
    const auto required = oresmd_uri_service::compose(
        quote_request("ir_swap", "rate", "EUR", {{"tenor", "6M"}, {"term", "10Y"}}));
    CHECK_FALSE(required.success);
    CHECK(required.uri.empty());
    CHECK_FALSE(required.message.empty());

    const auto unknown =
        oresmd_uri_service::compose(quote_request("ir_swap", "rate", "EUR", {{"foo", "bar"}}));
    CHECK_FALSE(unknown.success);
    CHECK(unknown.message.find("'foo' is not a field") != std::string::npos);

    const auto twice = oresmd_uri_service::compose(
        quote_request("ir_swap", "rate", "EUR", {{"tenor", "6M"}, {"tenor", "3M"}}));
    CHECK(twice.message.find("given twice") != std::string::npos);

    const auto subject = oresmd_uri_service::compose(
        quote_request("ir_swap",
                      "rate",
                      "EUR",
                      {{"ccy", "USD"}, {"fwd_start", "0D"}, {"tenor", "6M"}, {"term", "10Y"}}));
    CHECK_FALSE(subject.success);

    const auto form = oresmd_uri_service::compose(quote_request(
        "ir_swap", "rate", "EUR", {{"fwd_start", "soon"}, {"tenor", "6M"}, {"term", "10Y"}}));
    CHECK_FALSE(form.success);
    CHECK(form.message.find("fwd_start") != std::string::npos);

    CHECK_FALSE(oresmd_uri_service::compose(quote_request("nonsense", "rate", "EUR", {})).success);
    CHECK_FALSE(
        oresmd_uri_service::compose(quote_request("ir_swap", "nonsense", "EUR", {})).success);
    auto kind = quote_request("ir_swap", "rate", "EUR", {});
    kind.kind = "curve";
    CHECK_FALSE(oresmd_uri_service::compose(kind).success);
}

TEST_CASE("compose of a series refuses a coordinate field", "[marketdata][service][oresmd_uri_service]") {
    auto request = quote_request("discount", "rate", "USD", {{"curve_id", "USD-SOFR"}});
    request.kind = "series";
    const auto series = oresmd_uri_service::compose(request);
    REQUIRE(series.success);
    CHECK(series.uri.find("type=series") != std::string::npos);
    request.fields.push_back({"term", "5Y"});
    CHECK_FALSE(oresmd_uri_service::compose(request).success);
}

TEST_CASE("compose of a fixing is as strict as the codec", "[marketdata][service][oresmd_uri_service]") {
    messaging::compose_oresmd_uri_request request;
    request.kind = "fixing";
    request.instrument = "ibor";
    request.subject = "EUR";
    request.fields = {{"name", "EURIBOR"}, {"tenor", "6M"}};
    const auto fixing = oresmd_uri_service::compose(request);
    REQUIRE(fixing.success);
    CHECK(fixing.uri == "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M");
    request.fields = {{"name", "EURIBOR"}, {"tenor", "6M"}, {"tenor", "3M"}};
    CHECK_FALSE(oresmd_uri_service::compose(request).success);
    request.fields = {{"tenor", "6M"}};
    CHECK_FALSE(oresmd_uri_service::compose(request).success);
    request.instrument = "nonsense";
    CHECK_FALSE(oresmd_uri_service::compose(request).success);
}

TEST_CASE("an input over the limits is refused before the codec sees it", "[marketdata][service][oresmd_uri_service]") {
    const auto long_uri = parse(std::string(oresmd_uri_service::max_text + 1, 'a'));
    CHECK_FALSE(long_uri.success);
    auto request = quote_request("ir_swap", "rate", "EUR", {});
    for (std::size_t i = 0; i <= oresmd_uri_service::max_fields; ++i)
        request.fields.push_back({"tenor", "6M"});
    const auto many = oresmd_uri_service::compose(request);
    CHECK_FALSE(many.success);
    CHECK(many.message.find("at most") != std::string::npos);
    request.fields.clear();
    request.subject = std::string(oresmd_uri_service::max_text + 1, 'a');
    CHECK_FALSE(oresmd_uri_service::compose(request).success);
}
