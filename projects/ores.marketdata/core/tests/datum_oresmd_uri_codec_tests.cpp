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
#include "ores.marketdata.core/datum/ore_key_codec.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <optional>
#include <random>
#include <regex>
#include <set>
#include <string>
#include <vector>

/**
 * @file datum_oresmd_uri_codec_tests.cpp
 * @brief The oresmd URI codec against the ORE reference catalogue.
 *
 * Every key ORE accepts in the catalogue reads through the ORE key codec into a
 * datum. That datum and its series must write as URIs that read back to the
 * same datum and write again to the same text, and a quote URI must give back
 * the canonical ORE key. Property tests then replace the free-text and code
 * values of each documented form, to reach the characters and vocabularies the
 * catalogue does not hold.
 */

namespace {

const std::string tags("[marketdata][datum][oresmd_uri_codec]");

using namespace ores::marketdata::datum;
using ores::marketdata::test::accepted_by_ore;
using ores::marketdata::test::canonical_key;
using ores::marketdata::test::catalogue_corpus;
using ores::marketdata::test::catalogue_forms;
using ores::marketdata::test::catalogue_line;
using ores::marketdata::test::catalogue_quote_matrix;

struct tally {
    std::size_t datums = 0;
    std::set<std::string> keys;
    std::set<std::string> uris;
    std::vector<std::string> failures;

    void fail(const std::string& subject, const std::string& why) {
        if (failures.size() < 40)
            failures.push_back(subject + " -- " + why);
    }
};

/// Whether @p d writes as a URI that reads back to @p d and writes again to
/// the same text; the failure, if not.
std::optional<std::string> round_trip_fails(const market_datum& d) {
    const auto uri = oresmd_uri_codec::write(d);
    if (!uri)
        return "does not write: " + uri.error();
    const auto back = oresmd_uri_codec::read(*uri);
    if (!back)
        return "does not read back: " + back.error();
    if (*back != d)
        return *uri + " reads back as another datum";
    const auto again = oresmd_uri_codec::write(*back);
    if (!again || *again != *uri)
        return *uri + " writes again as " + (again ? *again : again.error());
    return std::nullopt;
}

tally check(const std::vector<catalogue_line>& lines) {
    tally t;
    for (const auto& line : lines) {
        if (!accepted_by_ore(line))
            continue;
        const auto& key = line.at("key");
        const auto datum = ore_key_codec::read(key);
        if (!datum)
            continue;
        ++t.datums;

        if (const auto why = round_trip_fails(*datum)) {
            t.fail(key, *why);
            continue;
        }
        const auto uri = *oresmd_uri_codec::write(*datum);
        const auto ore_key = ore_key_codec::write(*oresmd_uri_codec::read(uri));
        if (!ore_key || *ore_key != canonical_key(key))
            t.fail(key, uri + " gives back the ORE key " + (ore_key ? *ore_key : ore_key.error()));
        t.keys.insert(canonical_key(key));
        t.uris.insert(uri);

        if (const auto why = round_trip_fails(series_of(*datum)))
            t.fail(key, "series: " + *why);
    }
    return t;
}

void report(const tally& t) {
    for (const auto& f : t.failures)
        UNSCOPED_INFO(f);
    CHECK(t.failures.empty());
    CHECK(t.uris.size() == t.keys.size());
}

/// Free text with the characters a URI must encode, in both cases.
std::string random_text(std::mt19937& rng) {
    static constexpr std::string_view alphabet = "AaBbZz09 :.-_~&=?#%+!$'()*,;@[]";
    std::uniform_int_distribution<std::size_t> length(1, 12);
    std::uniform_int_distribution<std::size_t> pick(0, alphabet.size() - 1);
    std::string out;
    for (auto n = length(rng); n > 0; --n)
        out += alphabet[pick(rng)];
    return out;
}

/// The datum with @p f holding @p v, if the schema admits it and ORE has a key
/// for it; a value the ORE key cannot hold is no URI either.
std::optional<market_datum> with(const market_datum& d, field f, value v) {
    std::vector<field_value> fields(d.fields().begin(), d.fields().end());
    for (auto& fv : fields) {
        if (fv.name == f)
            fv.held = std::move(v);
    }
    auto changed = market_datum::make(d.type(), d.quote(), std::move(fields));
    if (!changed || !ore_key_codec::write(*changed))
        return std::nullopt;
    return std::move(*changed);
}

std::vector<market_datum> form_datums() {
    std::vector<market_datum> out;
    for (const auto& line : catalogue_forms()) {
        if (!accepted_by_ore(line))
            continue;
        if (auto d = ore_key_codec::read(line.at("key")))
            out.push_back(std::move(*d));
    }
    return out;
}

}

TEST_CASE("every_documented_form_round_trips_through_its_uri", tags) {
    const auto t = check(catalogue_forms());
    CHECK(t.datums >= 120);
    report(t);
}

TEST_CASE("every_corpus_key_round_trips_through_its_uri", tags) {
    const auto t = check(catalogue_corpus());
    const auto accepted = std::ranges::count_if(catalogue_corpus(), accepted_by_ore);
    CHECK(t.datums == static_cast<std::size_t>(accepted));
    report(t);
}

TEST_CASE("every_quote_type_ore_admits_round_trips_through_its_uri", tags) {
    const auto t = check(catalogue_quote_matrix());
    CHECK(t.datums > 0);
    report(t);

    const auto none = ore_key_codec::read("FX/NULL/EUR/USD");
    REQUIRE(none);
    CHECK(oresmd_uri_codec::write(*none) ==
          "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=null&ccy=USD");
}

TEST_CASE("a_quote_type_no_ore_key_can_name_is_refused", tags) {
    const auto uri = oresmd_uri_codec::read(
        "oresmd://credit/ACME?type=quote&instrument=hazard_rate&quote=hazard_rate"
        "&seniority=SNRFOR&ccy=USD&term=5Y");
    REQUIRE_FALSE(uri);
    CHECK(uri.error().contains("no ORE key names this datum"));
}

TEST_CASE("a_quote_uri_has_the_agreed_shape", tags) {
    const auto cds = ore_key_codec::read("CDS/CREDIT_SPREAD/ACME/SNRFOR/USD/XR14/5Y");
    REQUIRE(cds);
    CHECK(oresmd_uri_codec::write(*cds) ==
          "oresmd://credit/ACME?type=quote&instrument=cds&quote=credit_spread"
          "&seniority=SNRFOR&ccy=USD&doc_clause=XR14&term=5Y");

    const auto swaption = ore_key_codec::read("SWAPTION/RATE_LNVOL/EUR/5Y/2Y/ATM");
    REQUIRE(swaption);
    CHECK(oresmd_uri_codec::write(*swaption) ==
          "oresmd://ir/EUR?type=quote&instrument=swaption&quote=rate_lnvol"
          "&expiry=5Y&term=2Y&dimension=ATM");
}

TEST_CASE("a_series_uri_holds_the_identity_fields_only", tags) {
    const auto cds = ore_key_codec::read("CDS/CREDIT_SPREAD/ACME/SNRFOR/USD/XR14/5Y");
    REQUIRE(cds);
    CHECK(oresmd_uri_codec::write(series_of(*cds)) ==
          "oresmd://credit/ACME?type=series&instrument=cds&quote=credit_spread"
          "&seniority=SNRFOR&ccy=USD&doc_clause=XR14");
}

TEST_CASE("the_reader_refuses_any_spelling_but_the_writers", tags) {
    for (const auto* uri :
         {"oresmd://credit/ACME?term=5Y&quote=credit_spread&doc_clause=XR14&type=quote"
          "&ccy=USD&instrument=cds&seniority=SNRFOR",
          "oresmd://credit/%41CME?type=quote&instrument=cds&quote=credit_spread"
          "&seniority=SNRFOR&ccy=USD&doc_clause=XR14&term=5Y"}) {
        INFO("uri: " << uri);
        const auto d = oresmd_uri_codec::read(uri);
        REQUIRE_FALSE(d);
        CHECK(d.error().contains("oresmd://credit/ACME?type=quote&instrument=cds"
                                 "&quote=credit_spread&seniority=SNRFOR&ccy=USD"
                                 "&doc_clause=XR14&term=5Y"));
    }
}

TEST_CASE("a_series_with_empty_text_has_no_uri", tags) {
    const auto series = market_datum::make_series(
        instrument_type::equity_spot,
        quote_type::price,
        {{field::eq_name, std::string()}, {field::ccy, std::string("USD")}});
    CHECK_FALSE(series);
}

TEST_CASE("the_reader_refuses_a_uri_that_breaks_the_contract", tags) {
    const std::string base = "oresmd://credit/ACME?type=quote&instrument=cds&quote=credit_spread";
    for (const auto& uri : std::vector<std::string>{
             base + "&seniority=SNRFOR&doc_clause=XR14&term=5Y",
             base + "&seniority=SNRFOR&ccy=USD&term=5Y&colour=blue",
             base + "&seniority=SNRFOR&ccy=USD&ccy=USD&term=5Y",
             base + "&seniority=SNRFOR&ccy=USD&doc_clause=&term=5Y",
             base + "&underlying_name=ACME&seniority=SNRFOR&ccy=USD&term=5Y",
             base + "&seniority=SNRFOR&ccy=USD&term=5X",
             base + "&ccy=USD&doc_clause=XR14&term=5Y",
             "oresmd://ir/ACME?type=quote&instrument=cds&quote=credit_spread&ccy=USD&term=5Y",
             "oresmd://credit/ACME?type=quote&instrument=CDS&quote=credit_spread&ccy=USD&term=5Y",
             "oresmd://credit/ACME?type=curve&instrument=cds&quote=credit_spread&ccy=USD&term=5Y",
             "oresmd://credit/ACME?instrument=cds&quote=credit_spread&ccy=USD&term=5Y",
             "oresmd://credit/ACME?type=series&instrument=cds&quote=credit_spread&ccy=USD&term=5Y",
             "http://credit/ACME?type=quote&instrument=cds&quote=credit_spread&ccy=USD&term=5Y",
             "oresmd://credit/ACME/X?type=quote&instrument=cds&quote=credit_spread&ccy=USD&term=5Y",
             "oresmd://credit/ACME?type=quote&instrument=cds&quote=credit_spread&ccy=USD&term=5Y#x",
             "oresmd://credit:80/"
             "ACME?type=quote&instrument=cds&quote=credit_spread&ccy=USD&term=5Y",
             "oresmd://credit/?type=quote&instrument=cds&quote=credit_spread&ccy=USD&term=5Y"}) {
        INFO("uri: " << uri);
        const auto d = oresmd_uri_codec::read(uri);
        CHECK_FALSE(d);
        if (!d)
            CHECK_FALSE(d.error().empty());
    }
}

TEST_CASE("free_text_with_reserved_characters_round_trips", tags) {
    std::mt19937 rng(20261002);
    std::size_t tried = 0;
    std::size_t exercised = 0;
    std::vector<std::string> failures;
    for (const auto& d : form_datums()) {
        for (const auto& fv : d.fields()) {
            if (kind_of(fv.name) != value_kind::text || std::holds_alternative<none_t>(fv.held))
                continue;
            for (int i = 0; i < 20; ++i) {
                ++tried;
                const auto variant = with(d, fv.name, value(random_text(rng)));
                if (!variant)
                    continue;
                ++exercised;
                if (const auto why = round_trip_fails(*variant); why && failures.size() < 20)
                    failures.push_back(*why);
            }
        }
    }
    for (const auto& f : failures)
        UNSCOPED_INFO(f);
    CHECK(failures.empty());
    INFO("exercised " << exercised << " of " << tried);
    CHECK(exercised * 2 > tried);
}

TEST_CASE("every_code_in_a_fields_vocabulary_round_trips", tags) {
    std::size_t exercised = 0;
    std::vector<std::string> failures;
    for (const auto& d : form_datums()) {
        for (const auto& fv : d.fields()) {
            for (const auto token : codes_of(fv.name)) {
                const auto variant = with(d, fv.name, value(*code::parse(token)));
                if (!variant)
                    continue;
                ++exercised;
                if (const auto why = round_trip_fails(*variant); why && failures.size() < 20)
                    failures.push_back(*why);
            }
        }
    }
    for (const auto& f : failures)
        UNSCOPED_INFO(f);
    CHECK(failures.empty());
    CHECK(exercised > 0);
}

TEST_CASE("reserved_characters_are_percent_encoded_and_case_is_kept", tags) {
    const auto spot = ore_key_codec::read("COMMODITY/PRICE/ICE:Brent/USD");
    REQUIRE(spot);
    CHECK(oresmd_uri_codec::write(*spot) ==
          "oresmd://commodity/ICE:Brent?type=quote&instrument=commodity_spot&quote=price&ccy=USD");

    const auto equity = ore_key_codec::read("EQUITY/PRICE/RIC:.NDX/USD");
    REQUIRE(equity);
    auto odd = with(*equity, field::eq_name, value(std::string("A&B C%#?")));
    REQUIRE(odd);
    odd = with(*odd, field::ccy, value(std::string("x&y=z+w")));
    REQUIRE(odd);
    const auto uri = oresmd_uri_codec::write(*odd);
    REQUIRE(uri);
    CHECK(*uri == "oresmd://equity/A&B%20C%25%23%3F?type=quote&instrument=equity_spot&quote=price"
                  "&ccy=x%26y=z+w");
    const auto back = oresmd_uri_codec::read(*uri);
    REQUIRE(back);
    CHECK(*back == *odd);
}

TEST_CASE("no_field_is_named_like_a_reserved_query_key", tags) {
    for (const auto* key : {"type", "instrument", "quote"}) {
        INFO("key: " << key);
        CHECK_FALSE(field_named(key));
    }
}

TEST_CASE("every_quote_uri_the_sql_seeds_hold_is_the_codecs_spelling", tags) {
    // The seeds state URIs in SQL, which cannot call the codec, so a seed written
    // in another spelling would name a series no reader finds.
    const auto root = ores::testing::project_root::resolve("projects/ores.sql/populate");
    const std::regex literal("'(oresmd://[^']*)'");
    std::size_t checked = 0;
    for (const auto& entry : std::filesystem::recursive_directory_iterator(root)) {
        if (entry.path().extension() != ".sql")
            continue;
        const auto text = ores::platform::filesystem::file::read_content(entry.path());
        for (std::sregex_iterator it(text.begin(), text.end(), literal), end; it != end; ++it) {
            const auto uri = (*it)[1].str();
            if (!uri.contains('?'))
                continue;
            INFO(entry.path().filename().string() << ": " << uri);
            const auto d = oresmd_uri_codec::read(uri);
            CHECK(d);
            if (!d)
                UNSCOPED_INFO(d.error());
            ++checked;
        }
    }
    CHECK(checked > 0);
}

TEST_CASE("the_sql_seed_spelling_of_an_fx_spot_uri_is_the_codecs", tags) {
    // The FX driver rate seeds build both URIs from a pair such as EUR/USD as
    // 'oresmd://fx/' || base || '?type=<type>&instrument=fx_spot&quote=rate&ccy=' || quote.
    const auto d = ore_key_codec::read("FX/RATE/EUR/USD");
    REQUIRE(d);
    CHECK(oresmd_uri_codec::write(series_of(*d)) ==
          "oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD");
    CHECK(oresmd_uri_codec::write(*d) ==
          "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD");
}
