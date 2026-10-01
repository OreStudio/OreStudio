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
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/stress_test_mapper.hpp"
#include "ores.ore.core/xml/roundtrip_harness.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <map>
#include <optional>
#include <sstream>
#include <string>
#include <vector>

/**
 * @file xml_stress_test_mapper_roundtrip_tests.cpp
 * @brief The stress document's library, scenarios and shifts, mapped and mapped
 * back.
 *
 * Every family a scenario applies is mapped, so the case compares each family's
 * original entries against the entries reverse reconstructs. The comparison is
 * the binding's own writer: an entry is wrapped in a document of its own and
 * the two documents are compared as text, so a field the mapper loses makes the
 * two disagree. An empty container is written back as no container, which the
 * document's own text would notice but the entry comparison does not.
 */

namespace {

const std::string tags("[ore][xml][roundtrip][stress]");

std::filesystem::path corpus_root() {
    return ores::testing::project_root::resolve("external/ore/examples");
}

std::string read(const std::filesystem::path& path) {
    std::ifstream in(path, std::ios::binary);
    std::ostringstream buffer;
    buffer << in.rdbuf();
    return buffer.str();
}

ores::ore::domain::stresstesting load(const std::filesystem::path& path) {
    ores::ore::domain::stresstesting document;
    ores::ore::domain::load_data(read(path), document);
    return document;
}

/**
 * A document holding one entry in one scenario, so the entry can be mapped on
 * its own.
 */
template <typename Container, typename Entry>
ores::ore::domain::stresstesting
one_entry_document(const Entry& entry,
                   xsd::optional<Container> ores::ore::domain::stresstest::* container_member,
                   xsd::vector<Entry> Container::* vector_member) {
    ores::ore::domain::stresstesting document;
    ores::ore::domain::stresstest scenario;
    scenario.id = "one";
    scenario.*container_member = Container{};
    Container& container = *(scenario.*container_member);
    (container.*vector_member).push_back(entry);
    document.StressTest.push_back(std::move(scenario));
    return document;
}

/**
 * The document text of one entry: the binding writes the entry as it would in a
 * scenario of its own, so two entries agree when they write the same bytes.
 */
template <typename Container, typename Entry>
std::string entry_text(const Entry& entry,
                       xsd::optional<Container> ores::ore::domain::stresstest::* container_member,
                       xsd::vector<Entry> Container::* vector_member) {
    return ores::ore::domain::save_data(
        one_entry_document(entry, container_member, vector_member));
}

template <typename Container, typename Entry>
const xsd::vector<Entry>& entries_of(const xsd::optional<Container>& container,
                                     xsd::vector<Entry> Container::* vector_member) {
    static const Container empty{};
    if (!container)
        return empty.*vector_member;
    return (*container).*vector_member;
}

/**
 * Maps the document that holds one entry, reverses it, and compares the rebuilt
 * entry's own text with the original. The rebuilt entry is returned so a
 * section can assert on the fields that came back.
 */
template <typename Container, typename Entry>
Entry reverse_one_entry(const Entry& entry,
                        xsd::optional<Container> ores::ore::domain::stresstest::* container_member,
                        xsd::vector<Entry> Container::* vector_member) {
    const auto document = one_entry_document(entry, container_member, vector_member);
    const auto mapped = ores::ore::domain::stress_test_mapper::map(document);
    REQUIRE(mapped.scenarios.size() == 1);
    REQUIRE(mapped.scenarios.front().shifts.size() == 1);

    const auto reversed = ores::ore::domain::stress_test_mapper::reverse(mapped);
    REQUIRE(reversed.StressTest.size() == 1);
    const auto& container = reversed.StressTest.front().*container_member;
    REQUIRE(static_cast<bool>(container));
    const auto& entries = (*container).*vector_member;
    REQUIRE(entries.size() == 1);

    CHECK(entry_text<Container, Entry>(entry, container_member, vector_member) ==
          entry_text<Container, Entry>(entries.front(), container_member, vector_member));
    return entries.front();
}

/**
 * Compares one family's entries: the rows the map made for it, the document's
 * own entries, and the entries reverse rebuilt. The count is asserted on both
 * sides so that a dropped or duplicated entry is a failure of its own.
 */
template <typename Container, typename Entry>
void check_family(const std::string& path,
                  std::size_t scenario,
                  const std::string& family,
                  const std::vector<ores::analytics::domain::stress_test_shift>& rows,
                  const xsd::optional<Container>& original,
                  const xsd::optional<Container>& rebuilt,
                  xsd::optional<Container> ores::ore::domain::stresstest::* container_member,
                  xsd::vector<Entry> Container::* vector_member) {
    const auto& original_entries = entries_of(original, vector_member);
    const auto& rebuilt_entries = entries_of(rebuilt, vector_member);

    std::size_t mapped = 0;
    for (const auto& row : rows)
        if (row.family == family)
            ++mapped;

    INFO(path + ": scenario " + std::to_string(scenario) + " " + family);
    REQUIRE(mapped == original_entries.size());
    REQUIRE(rebuilt_entries.size() == original_entries.size());
    for (std::size_t j = 0; j < original_entries.size(); ++j) {
        INFO("entry " + std::to_string(j));
        CHECK(entry_text<Container, Entry>(original_entries.at(j), container_member, vector_member) ==
              entry_text<Container, Entry>(rebuilt_entries.at(j), container_member, vector_member));
    }
}

std::string par_shifts_text(const xsd::optional<ores::ore::domain::stresstestparshifts>& par) {
    ores::ore::domain::stresstesting document;
    ores::ore::domain::stresstest scenario;
    scenario.id = "one";
    scenario.ParShifts = par;
    document.StressTest.push_back(std::move(scenario));
    return ores::ore::domain::save_data(document);
}

}

TEST_CASE("stress_test_library_and_scenarios_round_trip_over_the_corpus", tags) {
    using namespace ores::ore::domain;

    int files = 0;
    int scenarios = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("stress", corpus_root())) {
        ++files;
        const auto document = load(path);

        const auto mapped = stress_test_mapper::map(document);
        REQUIRE(mapped.scenarios.size() == document.StressTest.size());
        CHECK(mapped.unmodelled.empty());
        if (document.UseSpreadedTermStructures)
            REQUIRE(mapped.library.use_spreaded_term_structures.has_value());

        const auto reversed = stress_test_mapper::reverse(mapped);
        REQUIRE(reversed.StressTest.size() == document.StressTest.size());

        for (std::size_t i = 0; i < mapped.scenarios.size(); ++i) {
            ++scenarios;
            const auto& in = document.StressTest.at(i);
            const auto& out = reversed.StressTest.at(i);
            const auto& row = mapped.scenarios.at(i);

            INFO(path.string() + ": scenario " + std::to_string(i));
            CHECK(row.scenario.name == in.id);
            CHECK(out.id == in.id);
            CHECK(row.scenario.position == static_cast<int>(i) + 1);
            CHECK(static_cast<bool>(out.Date) == static_cast<bool>(in.Date));
            if (in.Date)
                CHECK(std::string(*out.Date) == std::string(*in.Date));

            std::size_t par_shifts = 0;
            for (const auto& shift : row.shifts)
                if (shift.family == "ParShifts")
                    ++par_shifts;
            CHECK(par_shifts == (in.ParShifts ? 1u : 0u));
            CHECK(static_cast<bool>(out.ParShifts) == static_cast<bool>(in.ParShifts));
            if (in.ParShifts)
                CHECK(par_shifts_text(in.ParShifts) == par_shifts_text(out.ParShifts));

            check_family(path.string(), i, "DiscountCurves", row.shifts, in.DiscountCurves,
                         out.DiscountCurves, &stresstest::DiscountCurves,
                         &stressdiscountcurves::DiscountCurve);
            check_family(path.string(), i, "IndexCurves", row.shifts, in.IndexCurves,
                         out.IndexCurves, &stresstest::IndexCurves,
                         &stressindexcurves::IndexCurve);
            check_family(path.string(), i, "YieldCurves", row.shifts, in.YieldCurves,
                         out.YieldCurves, &stresstest::YieldCurves,
                         &stressyieldcurves::YieldCurve);
            check_family(path.string(), i, "FxSpots", row.shifts, in.FxSpots, out.FxSpots,
                         &stresstest::FxSpots, &fxspots::FxSpot);
            check_family(path.string(), i, "FxVolatilities", row.shifts, in.FxVolatilities,
                         out.FxVolatilities, &stresstest::FxVolatilities,
                         &stressfxvolatilities::FxVolatility);
            check_family(path.string(), i, "SwaptionVolatilities", row.shifts,
                         in.SwaptionVolatilities, out.SwaptionVolatilities,
                         &stresstest::SwaptionVolatilities,
                         &stressswaptionvolatilities::SwaptionVolatility);
            check_family(path.string(), i, "CapFloorVolatilities", row.shifts,
                         in.CapFloorVolatilities, out.CapFloorVolatilities,
                         &stresstest::CapFloorVolatilities,
                         &stresscapfloorvolatilities::CapFloorVolatility);
            check_family(path.string(), i, "EquitySpots", row.shifts, in.EquitySpots,
                         out.EquitySpots, &stresstest::EquitySpots, &equityspots::EquitySpot);
            check_family(path.string(), i, "EquityVolatilities", row.shifts,
                         in.EquityVolatilities, out.EquityVolatilities,
                         &stresstest::EquityVolatilities,
                         &equityvolatilities::EquityVolatility);
            check_family(path.string(), i, "CommodityCurves", row.shifts, in.CommodityCurves,
                         out.CommodityCurves, &stresstest::CommodityCurves,
                         &stresscommoditycurves::CommodityCurve);
            check_family(path.string(), i, "IntradayPowerCurves", row.shifts,
                         in.IntradayPowerCurves, out.IntradayPowerCurves,
                         &stresstest::IntradayPowerCurves,
                         &stressintradaypowercurves::IntradayPowerCurve);
            check_family(path.string(), i, "CommodityVolatilities", row.shifts,
                         in.CommodityVolatilities, out.CommodityVolatilities,
                         &stresstest::CommodityVolatilities,
                         &stresscommodityvolatilities::CommodityVolatility);
            check_family(path.string(), i, "SecuritySpreads", row.shifts, in.SecuritySpreads,
                         out.SecuritySpreads, &stresstest::SecuritySpreads,
                         &securityspreads::SecuritySpread);
            check_family(path.string(), i, "RecoveryRates", row.shifts, in.RecoveryRates,
                         out.RecoveryRates, &stresstest::RecoveryRates,
                         &recoveryrates::RecoverRate);
            check_family(path.string(), i, "SurvivalProbabilities", row.shifts,
                         in.SurvivalProbabilities, out.SurvivalProbabilities,
                         &stresstest::SurvivalProbabilities,
                         &survivalprobabilities::SurvivalProbability);
        }

        CHECK(static_cast<bool>(reversed.UseSpreadedTermStructures) ==
              static_cast<bool>(document.UseSpreadedTermStructures));
    }

    CHECK(files == 20);
    CHECK(scenarios == 46);
}

// The cases the corpus does not carry: a scenario with no date, a document with
// no scenarios at all, and the library setting absent, true and false. Each is
// built by hand because the corpus has none of them.
TEST_CASE("stress_test_mapper_keeps_the_shapes_the_corpus_lacks", tags) {
    using namespace ores::ore::domain;

    const auto round_trip = [](const std::string& xml) {
        stresstesting original;
        load_data(xml, original);
        const auto mapped = stress_test_mapper::map(original);
        const auto reversed = stress_test_mapper::reverse(mapped);
        return std::make_pair(original, reversed);
    };

    SECTION("a scenario with no date keeps no date") {
        const auto [original, reversed] = round_trip(R"(<StressTesting>
  <StressTest id="theta">
    <DiscountCurves/>
  </StressTest>
</StressTesting>)");
        REQUIRE(reversed.StressTest.size() == 1);
        CHECK(reversed.StressTest.front().id == "theta");
        CHECK(static_cast<bool>(reversed.StressTest.front().Date) ==
              static_cast<bool>(original.StressTest.front().Date));
        CHECK(!reversed.StressTest.front().Date);
    }

    // A document with no scenarios at all cannot be built: ORE's schema makes
    // StressTest occur at least once, and the binding refuses it. The case says
    // so rather than pretending the shape exists.
    SECTION("a document with no scenarios is refused by the schema") {
        stresstesting empty;
        CHECK_THROWS(load_data("<StressTesting></StressTesting>", empty));
    }

    SECTION("the library setting is absent, and stays absent") {
        const auto [original, reversed] =
            round_trip("<StressTesting><StressTest id=\"theta\"/></StressTesting>");
        CHECK(!reversed.UseSpreadedTermStructures);
    }

    // The binding gives each spelling of a boolean its own enumerator, so true
    // written back as True is a different enumerator holding the same value.
    // The comparison is the mapper's own: map the reversal and compare that.
    SECTION("the library setting is true, and stays true") {
        const auto xml = std::string("<StressTesting><UseSpreadedTermStructures>true"
                                     "</UseSpreadedTermStructures><StressTest id=\"theta\"/>"
                                     "</StressTesting>");
        stresstesting original;
        load_data(xml, original);
        const auto mapped = stress_test_mapper::map(original);
        REQUIRE(mapped.library.use_spreaded_term_structures.has_value());
        CHECK(*mapped.library.use_spreaded_term_structures);

        const auto reversals = stress_test_mapper::map(stress_test_mapper::reverse(mapped));
        CHECK(reversals.library.use_spreaded_term_structures ==
              mapped.library.use_spreaded_term_structures);
    }

    SECTION("the library setting is false, and stays false") {
        const auto xml = std::string("<StressTesting><UseSpreadedTermStructures>false"
                                     "</UseSpreadedTermStructures><StressTest id=\"theta\"/>"
                                     "</StressTesting>");
        stresstesting original;
        load_data(xml, original);
        const auto mapped = stress_test_mapper::map(original);
        REQUIRE(mapped.library.use_spreaded_term_structures.has_value());
        CHECK(!*mapped.library.use_spreaded_term_structures);

        const auto reversals = stress_test_mapper::map(stress_test_mapper::reverse(mapped));
        CHECK(reversals.library.use_spreaded_term_structures ==
              mapped.library.use_spreaded_term_structures);
    }

    // ParConversion is an optional member of IndexCurve, YieldCurve and
    // SurvivalProbability. Every sub-field must survive, including each
    // Convention's id and every Convention after the first, so the separator
    // inside a Convention must differ from the one joining the fields.
    SECTION("a ParConversion keeps every sub-field") {
        stressindexcurve entry;
        entry.index = "EUR-EURIBOR-6M";

        parconversion conversion;
        conversion.Instruments.push_back(parconversion_Instruments_t("Swap"));
        conversion.Instruments.push_back(parconversion_Instruments_t("Deposit"));
        conversion.SingleCurve.push_back(true);
        conversion.SingleCurve.push_back(false);
        conversion.DiscountCurve = parconversion_DiscountCurve_t("EUR-OIS");
        conversion.OtherCurrency = currencyCode::USD;
        conversion.RateComputationPeriod = parconversion_RateComputationPeriod_t("3M");

        parconversion_Conventions_t conventions;
        parconversion_Conventions_t_Convention_t first;
        static_cast<xsd::string&>(first) = "ACT/360";
        first.id = "one";
        conventions.Convention.push_back(first);
        parconversion_Conventions_t_Convention_t second;
        static_cast<xsd::string&>(second) = "30/360";
        conventions.Convention.push_back(second);
        conversion.Conventions = conventions;
        entry.ParConversion = conversion;

        const auto rebuilt = reverse_one_entry<stressindexcurves, stressindexcurve>(
            entry, &stresstest::IndexCurves, &stressindexcurves::IndexCurve);

        REQUIRE(static_cast<bool>(rebuilt.ParConversion));
        const auto& back = *rebuilt.ParConversion;
        REQUIRE(back.Instruments.size() == 2);
        CHECK(std::string(back.Instruments.at(0)) == "Swap");
        CHECK(std::string(back.Instruments.at(1)) == "Deposit");
        REQUIRE(back.SingleCurve.size() == 2);
        CHECK(static_cast<bool>(back.SingleCurve.at(0)));
        CHECK(!static_cast<bool>(back.SingleCurve.at(1)));
        REQUIRE(static_cast<bool>(back.DiscountCurve));
        CHECK(std::string(*back.DiscountCurve) == "EUR-OIS");
        REQUIRE(static_cast<bool>(back.OtherCurrency));
        CHECK(*back.OtherCurrency == currencyCode::USD);
        REQUIRE(static_cast<bool>(back.RateComputationPeriod));
        CHECK(std::string(*back.RateComputationPeriod) == "3M");
        REQUIRE(static_cast<bool>(back.Conventions));
        REQUIRE(back.Conventions->Convention.size() == 2);
        CHECK(std::string(back.Conventions->Convention.at(0)) == "ACT/360");
        REQUIRE(static_cast<bool>(back.Conventions->Convention.at(0).id));
        CHECK(std::string(*back.Conventions->Convention.at(0).id) == "one");
        CHECK(std::string(back.Conventions->Convention.at(1)) == "30/360");
    }

    // ShiftScheme is a keyed vector on eight families. Every scheme and every
    // key must survive, in order.
    SECTION("a ShiftScheme keeps every scheme and key") {
        fxspot entry;
        entry.ccypair = "EUR/USD";

        shiftSchemeEntry forward;
        static_cast<shiftScheme&>(forward) = shiftScheme::Forward;
        forward.key = "3M";
        entry.ShiftScheme.push_back(forward);

        shiftSchemeEntry backward;
        static_cast<shiftScheme&>(backward) = shiftScheme::Backward;
        entry.ShiftScheme.push_back(backward);

        shiftSchemeEntry central;
        static_cast<shiftScheme&>(central) = shiftScheme::Central;
        central.key = "1Y";
        entry.ShiftScheme.push_back(central);

        const auto rebuilt =
            reverse_one_entry<fxspots, fxspot>(entry, &stresstest::FxSpots, &fxspots::FxSpot);

        REQUIRE(rebuilt.ShiftScheme.size() == 3);
        CHECK(static_cast<shiftScheme>(rebuilt.ShiftScheme.at(0)) == shiftScheme::Forward);
        REQUIRE(static_cast<bool>(rebuilt.ShiftScheme.at(0).key));
        CHECK(std::string(*rebuilt.ShiftScheme.at(0).key) == "3M");
        CHECK(static_cast<shiftScheme>(rebuilt.ShiftScheme.at(1)) == shiftScheme::Backward);
        CHECK(!rebuilt.ShiftScheme.at(1).key);
        CHECK(static_cast<shiftScheme>(rebuilt.ShiftScheme.at(2)) == shiftScheme::Central);
        REQUIRE(static_cast<bool>(rebuilt.ShiftScheme.at(2).key));
        CHECK(std::string(*rebuilt.ShiftScheme.at(2).key) == "1Y");
    }

    SECTION("a CurveType travels with the yield curve") {
        stressyieldcurve entry;
        entry.name = "EUR-OIS";
        entry.CurveType = stressyieldcurve_CurveType_t("Zero");

        const auto rebuilt = reverse_one_entry<stressyieldcurves, stressyieldcurve>(
            entry, &stresstest::YieldCurves, &stressyieldcurves::YieldCurve);

        REQUIRE(static_cast<bool>(rebuilt.CurveType));
        CHECK(std::string(*rebuilt.CurveType) == "Zero");
    }

    SECTION("IsRelative true stays true") {
        stresscapfloorvolatility entry;
        entry.ccy = currencyCode::EUR;
        entry.IsRelative = true;

        const auto rebuilt =
            reverse_one_entry<stresscapfloorvolatilities, stresscapfloorvolatility>(
                entry, &stresstest::CapFloorVolatilities,
                &stresscapfloorvolatilities::CapFloorVolatility);

        REQUIRE(static_cast<bool>(rebuilt.IsRelative));
        CHECK(*rebuilt.IsRelative);
    }

    SECTION("IsRelative false stays false") {
        stresscapfloorvolatility entry;
        entry.ccy = currencyCode::EUR;
        entry.IsRelative = false;

        const auto rebuilt =
            reverse_one_entry<stresscapfloorvolatilities, stresscapfloorvolatility>(
                entry, &stresstest::CapFloorVolatilities,
                &stresscapfloorvolatilities::CapFloorVolatility);

        REQUIRE(static_cast<bool>(rebuilt.IsRelative));
        CHECK(!*rebuilt.IsRelative);
    }
}
