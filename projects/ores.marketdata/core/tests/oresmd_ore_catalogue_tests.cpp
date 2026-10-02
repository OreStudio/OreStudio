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
#include "ores.marketdata.api/domain/oresmd_uri.hpp"
#include "ores.marketdata.core/oresmd/oresmd_parser.hpp"
#include "ores.marketdata.core/oresmd/oresmd_projections.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.utility/compression/gzip.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <filesystem>
#include <format>
#include <map>
#include <optional>
#include <rfl/json.hpp>
#include <set>
#include <sstream>
#include <string>
#include <vector>

/**
 * @file oresmd_ore_catalogue_tests.cpp
 * @brief Reads the ORE market datum catalogue: what ORE's own parser makes of
 * every key form it documents and of every key the example corpus carries.
 *
 * The catalogue is built outside this build, by the harness under
 * external/ore/tools/datum_catalogue, because this build does not link ORE. The
 * cases here check that the catalogue covers ORE, and measure the generated
 * oresmd grammar against it through the path the database stores: key, URI,
 * parse, key.
 */

namespace {

const std::string tags("[marketdata][oresmd][catalogue]");

using catalogue_line = std::map<std::string, std::string>;

std::vector<catalogue_line> lines_of(const std::string& content) {
    std::vector<catalogue_line> result;
    std::istringstream stream(content);
    std::string line;
    while (std::getline(stream, line)) {
        if (line.empty())
            continue;
        auto parsed = rfl::json::read<catalogue_line>(line);
        if (!parsed)
            FAIL("unreadable catalogue line: " << line);
        result.push_back(std::move(*parsed));
    }
    return result;
}

std::filesystem::path catalogue_dir() {
    return ores::testing::project_root::resolve("external/ore/catalogue");
}

const std::vector<catalogue_line>& forms() {
    static const auto result =
        lines_of(ores::platform::filesystem::file::read_content(catalogue_dir() / "forms.jsonl"));
    return result;
}

const std::vector<catalogue_line>& corpus() {
    static const auto result = [] {
        const auto compressed =
            ores::platform::filesystem::file::read_content(catalogue_dir() / "corpus.jsonl.gz");
        const auto content = ores::utility::compression::gzip_decompress(compressed);
        return lines_of(std::string(content.begin(), content.end()));
    }();
    return result;
}

bool accepted(const catalogue_line& line) {
    return line.at("accepted") == "true";
}

std::string type_token(const catalogue_line& line) {
    const auto& key = line.at("key");
    return key.substr(0, key.find('/'));
}

/// The hint a failing pin carries: its figures move only when the corpus or the
/// grammar does.
const std::string regenerate_hint("Counts changed? Regenerate the catalogue with "
                                  "external/ore/tools/datum_catalogue/regenerate.sh, then re-pin.");

/// What happens to one key on the path the database stores. The URI checks run
/// only for a key the grammar names, because an unnamed key has no URI.
struct stored_path {
    bool named = false;
    bool uri_unwritable = false;
    bool uri_rejected = false;
    bool reads_back = false;
    bool series_uri_rejected = false;
};

stored_path walk(const std::string& key) {
    using ores::marketdata::core::oresmd_parser;
    using ores::marketdata::core::oresmd_projections;
    stored_path result;
    const auto id = oresmd_projections::from_ore_key(key);
    if (!id)
        return result;
    result.named = true;
    std::optional<ores::marketdata::domain::oresmd_uri> uri;
    try {
        uri = oresmd_parser::to_uri(*id);
    } catch (const std::exception&) {
        result.uri_unwritable = true;
        return result;
    }
    try {
        const auto back = oresmd_projections::to_quote_key(oresmd_parser::parse(*uri));
        result.reads_back = back && *back == key;
    } catch (const std::exception&) {
        result.uri_rejected = true;
    }
    try {
        (void)oresmd_parser::parse(oresmd_parser::to_series_uri(*id));
    } catch (const std::exception&) {
        result.series_uri_rejected = true;
    }
    return result;
}

}

TEST_CASE("every_ore_instrument_type_has_a_documented_form_in_the_catalogue", tags) {
    // ORE's InstrumentType enum and the key tokens its parser reads as each
    // member, extracted from ORE's own sources with the catalogue: FX_SPOT is
    // read from FX and from FX_SPOT. A type is covered when a form ORE accepts
    // begins with any of its tokens.
    std::map<std::string, std::vector<std::string>> ore_types;
    {
        std::istringstream stream(ores::platform::filesystem::file::read_content(
            catalogue_dir() / "instrument_types.txt"));
        std::string line;
        while (std::getline(stream, line)) {
            std::istringstream fields(line);
            std::string type, token;
            fields >> type;
            auto& tokens = ore_types[type];
            while (fields >> token)
                tokens.push_back(token);
        }
    }
    INFO(regenerate_hint);
    REQUIRE(ore_types.size() == 45);

    std::set<std::string> covered;
    std::size_t refused = 0;
    for (const auto& line : forms()) {
        if (accepted(line))
            covered.insert(type_token(line));
        else
            ++refused;
    }

    for (const auto& [type, tokens] : ore_types) {
        INFO("ORE instrument type: " << type);
        CHECK(std::ranges::any_of(tokens, [&](const auto& t) { return covered.contains(t); }));
    }
    CHECK(refused > 0);
}

TEST_CASE("ore_accepts_every_key_the_example_corpus_carries", tags) {
    const auto& lines = corpus();
    INFO(regenerate_hint);
    REQUIRE(lines.size() == 108057);
    for (const auto& line : lines) {
        if (!accepted(line))
            FAIL("ORE refuses corpus key " << line.at("key") << ": " << line.at("error"));
    }
}

TEST_CASE("the_generated_grammar_baseline_through_the_stored_uri", tags) {
    // The figures the 2026-10-02 review measured with a throwaway probe, now
    // measured here: every corpus key ORE accepts is named by oresmd, but on the
    // way through the URI the database stores, some are refused by oresmd's own
    // parser and some come back as a different key. The case pins the generated
    // grammar's figures so they cannot move unnoticed; the hand-written codec
    // replaces this case with a field-by-field comparison against the catalogue.
    std::size_t keys = 0, named = 0, uri_unwritable = 0, uri_rejected = 0, not_reading_back = 0,
                series_uri_rejected = 0;
    for (const auto& line : corpus()) {
        ++keys;
        const auto path = walk(line.at("key"));
        named += path.named;
        uri_unwritable += path.uri_unwritable;
        uri_rejected += path.uri_rejected;
        not_reading_back += path.named && !path.reads_back;
        series_uri_rejected += path.series_uri_rejected;
    }
    WARN(std::format("corpus: {} keys, {} named, {} URIs unwritable, {} URIs refused, "
                     "{} not reading back, {} series URIs refused",
                     keys,
                     named,
                     uri_unwritable,
                     uri_rejected,
                     not_reading_back,
                     series_uri_rejected));

    INFO(regenerate_hint);
    CHECK(named == 108057);
    CHECK(uri_unwritable == 0);
    CHECK(uri_rejected == 4269);
    CHECK(not_reading_back == 7254);
    CHECK(series_uri_rejected == 3007);

    std::size_t forms_accepted = 0, forms_named = 0;
    for (const auto& line : forms()) {
        if (!accepted(line))
            continue;
        ++forms_accepted;
        forms_named += walk(line.at("key")).named;
    }
    WARN(std::format(
        "documented forms: {} accepted by ORE, {} named by oresmd", forms_accepted, forms_named));
}
