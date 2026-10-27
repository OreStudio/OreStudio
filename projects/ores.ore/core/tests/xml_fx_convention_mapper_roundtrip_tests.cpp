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
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/conventions_package.hpp"
#include "ores.ore.core/xml/record.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include "test_suite.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <map>

using Catch::Approx;

/**
 * @file xml_fx_convention_mapper_roundtrip_tests.cpp
 * @brief Round-trips ORE's conventions.xml <FX> element through
 * conventions_mapper::map_fx and the package join against a real example file,
 * not just the migration task's unit-level acceptance criteria.
 *
 * The real example file (external/ore/examples/Input/conventions.xml)
 * surfaces a genuine data-loss case: two <FX> entries
 * (EUR-USD-FX-CONVENTIONS, FX-ECB-EUR-USD) both target the EUR/USD pair
 * with different SpotDays. Since currency_pair_convention is keyed 1:1 by
 * pair_code (by design — see ores.refdata.currency_pair_convention.org),
 * only one survives an import; this is documented here, not silently
 * accepted.
 */

namespace {

using ores::ore::domain::conventions;
using ores::ore::domain::conventions_mapper;
using ores::ore::xml::record_reader;
using namespace ores::logging;

std::filesystem::path conventions_path() {
    return ores::testing::project_root::resolve("external/ore/examples/Input/conventions.xml");
}

conventions load_conventions() {
    using ores::platform::filesystem::file;
    return conventions_mapper::read(file::read_content(conventions_path()),
                                    conventions_path().string());
}

record_reader root_of(const conventions& document) {
    return record_reader(*ores::ore::xml::generated::conventions::conventions_document().root,
                         &document);
}

/**
 * @brief Round-trip a single FX element through map_fx then back through the
 * package join (individual reverse_fx is not exposed publicly).
 */
conventions roundtrip_single(const record_reader& original) {
    const ores::refdata::messaging::fx_convention mapped = conventions_mapper::map_fx(original);

    ores::ore::domain::conventions_package wrapper;
    wrapper.fx.push_back(mapped);
    auto reversed = ores::ore::domain::join_conventions(wrapper);

    REQUIRE(reversed.fx.size() == 1);
    return reversed;
}

} // namespace

// =============================================================================
// Real example file: field-by-field round-trip for every <FX> entry
// =============================================================================

TEST_CASE("fx_convention_roundtrip_real_example_file_field_values", "[ore][xml][mapper][roundtrip][fx_convention]") {
    auto lg(make_logger(test_suite));
    const auto original = load_conventions();
    const auto entries = root_of(original).children("FX");
    REQUIRE(!entries.empty());

    for (const auto& fx : entries) {
        const auto reversed = roundtrip_single(fx);
        const auto roundtripped = root_of(reversed).children("FX").front();

        CHECK(roundtripped.spelling("SourceCurrency") == fx.spelling("SourceCurrency"));
        CHECK(roundtripped.spelling("TargetCurrency") == fx.spelling("TargetCurrency"));
        CHECK(roundtripped.integer("SpotDays") == fx.integer("SpotDays"));
        CHECK(roundtripped.decimal("PointsFactor")->to_double() ==
              Approx(fx.decimal("PointsFactor")->to_double()).epsilon(1e-9));

        if (fx.has("AdvanceCalendar")) {
            REQUIRE(roundtripped.has("AdvanceCalendar"));
            CHECK(roundtripped.text("AdvanceCalendar") == fx.text("AdvanceCalendar"));
        } else {
            CHECK(!roundtripped.has("AdvanceCalendar"));
        }

        if (fx.has("SpotRelative")) {
            REQUIRE(roundtripped.has("SpotRelative"));
            CHECK(roundtripped.flag("SpotRelative") == fx.flag("SpotRelative"));
        }

        // Id is NOT preserved: reverse_fx regenerates it as
        // "<base>-<quote>-FX-CONVENTIONS" rather than keeping the source
        // XML's original Id (e.g. "FX-ECB-EUR-USD" becomes
        // "EUR-USD-FX-CONVENTIONS"). Documented, not asserted equal.
    }

    BOOST_LOG_SEV(lg, info) << "Field-by-field FX convention round-trip passed for "
                            << entries.size() << " entries";
}

// =============================================================================
// Documented data-loss case: multiple <FX> entries per pair collide
// =============================================================================

TEST_CASE("fx_convention_roundtrip_duplicate_pair_collision_is_documented", "[ore][xml][mapper][roundtrip][fx_convention]") {
    auto lg(make_logger(test_suite));
    const auto original = load_conventions();
    const auto entries = root_of(original).children("FX");

    std::map<std::pair<std::string, std::string>, int> pair_counts;
    for (const auto& fx : entries)
        ++pair_counts[{*fx.spelling("SourceCurrency"), *fx.spelling("TargetCurrency")}];

    // The real example file (as of this writing) has exactly one colliding
    // pair: EUR/USD, entered twice (EUR-USD-FX-CONVENTIONS with SpotDays=0
    // and FX-ECB-EUR-USD with SpotDays=2). currency_pair_convention is
    // keyed 1:1 by pair_code (base_currency + "/" + quote_currency), so
    // importing both into ORES loses one — whichever is written last wins
    // (upsert-by-key semantics, same pattern documented in task
    // 03FDF102-2379-4C73-BFD8-FA71BF670BF5 for currency_pair itself).
    int duplicated_pairs = 0;
    for (const auto& [pair, count] : pair_counts) {
        if (count > 1) {
            ++duplicated_pairs;
            BOOST_LOG_SEV(lg, info)
                << "Colliding FX convention pair: " << pair.first << "/" << pair.second << " ("
                << count << " entries in source XML, " << "only 1 survives import)";
        }
    }

    CHECK(duplicated_pairs == 1);
    CHECK(entries.size() == 24);
    CHECK(pair_counts.size() == 23);
}

// =============================================================================
// Synthetic case: EOM and Convention fields (not exercised by the real file)
// =============================================================================

TEST_CASE("fx_convention_roundtrip_synthetic_eom_and_business_day_convention", "[ore][xml][mapper][roundtrip][fx_convention]") {
    const auto original = conventions_mapper::read("<Conventions>"
                                                   "<FX>"
                                                   "<Id>EUR-USD-FX-CONVENTIONS</Id>"
                                                   "<SpotDays>2</SpotDays>"
                                                   "<SourceCurrency>EUR</SourceCurrency>"
                                                   "<TargetCurrency>USD</TargetCurrency>"
                                                   "<PointsFactor>10000</PointsFactor>"
                                                   "<EOM>true</EOM>"
                                                   "<Convention>Following</Convention>"
                                                   "</FX>"
                                                   "</Conventions>");

    const auto reversed = roundtrip_single(root_of(original).children("FX").front());
    const auto roundtripped = root_of(reversed).children("FX").front();

    CHECK(roundtripped.flag("EOM") == true);
    CHECK(roundtripped.spelling("Convention") == "Following");
}
