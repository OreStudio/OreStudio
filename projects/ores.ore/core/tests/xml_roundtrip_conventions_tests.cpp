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
#include "conventions_rows.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/conventions_package.hpp"
#include "ores.ore.core/xml/record.hpp"
#include "ores.ore.core/xml/roundtrip_harness.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.utility/crypto/sha256.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <functional>
#include <map>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

/**
 * @file xml_roundtrip_conventions_tests.cpp
 * @brief The conventions kind: what rounds trips, and what the mapper drops.
 *
 * The document carries twenty-six convention categories and the mapper models
 * twenty-five. The files that use only those twenty-five round trip, and all
 * seventy-two of them do. A category the mapper does not model is content the
 * export cannot write, and the measurement below says which categories those
 * are and how many files each one costs.
 */

using ores::ore::tests::rows_of;

namespace {

const std::string tags("[ore][xml][roundtrip][conventions]");

using namespace ores::ore::domain;

std::filesystem::path corpus_root() {
    return ores::testing::project_root::resolve("external/ore/examples");
}

std::string read(const std::filesystem::path& path) {
    std::ifstream in(path, std::ios::binary);
    std::ostringstream buffer;
    buffer << in.rdbuf();
    return buffer.str();
}

conventions load(const std::filesystem::path& path) {
    return conventions_mapper::read(read(path), path.string());
}

ores::ore::xml::record_reader root_of(const conventions& document) {
    return ores::ore::xml::record_reader(
        *ores::ore::xml::generated::conventions::conventions_document().root, &document);
}

ores::ore::xml::roundtrip_kind conventions_kind() {
    return ores::ore::xml::make_roundtrip_kind<conventions, ores::ore::domain::conventions_package>(
        "conventions",
        "conventions",
        &conventions_mapper::read,
        &conventions_mapper::write,
        &ores::ore::domain::split_conventions,
        &ores::ore::domain::join_conventions,
        conventions_difference);
}

int element_count(const std::string& xml, const std::string& name) {
    const std::string open = "<" + name;
    int count = 0;
    std::size_t at = 0;
    while ((at = xml.find(open, at)) != std::string::npos) {
        const auto next = at + open.size();
        if (next < xml.size() && (xml[next] == '>' || xml[next] == ' ' || xml[next] == '/'))
            ++count;
        at = next;
    }
    return count;
}

/**
 * @brief One family a document element packs into: how to read its ids and rows
 * back out of a package, and how many corpus files carry it.
 *
 * A family that has no pinned file count carries -1, and the case that walks
 * the families only measures it.
 */
struct family {
    std::string key;
    std::string element;
    int files;
    std::function<std::vector<std::string>(const conventions_package&)> ids;
    std::function<bool(const conventions_package&, const conventions_package&, std::size_t)>
        same_row;
};

template <typename Row>
family make_family(std::string key, std::string element, int files) {
    return {
        key,
        std::move(element),
        files,
        [key](const conventions_package& package) {
            std::vector<std::string> ids;
            for (const auto& row : rows_of<Row>(package, key))
                ids.push_back(row.id);
            return ids;
        },
        [key](const conventions_package& left, const conventions_package& right, std::size_t i) {
            return rows_of<Row>(left, key)[i] == rows_of<Row>(right, key)[i];
        }};
}

std::vector<family> families() {
    namespace rd = ores::refdata::domain;
    return {
        make_family<rd::average_ois_convention>("average_ois", "AverageOIS", 36),
        make_family<rd::bma_basis_swap_convention>("bma_basis_swap", "BMABasisSwap", 16),
        make_family<rd::bond_yield_convention>("bond_yield", "BondYield", 1),
        make_family<rd::cds_convention>("cds", "CDS", 40),
        make_family<rd::cms_spread_option_convention>("cms_spread_option", "CmsSpreadOption", 12),
        make_family<rd::commodity_forward_convention>("commodity_forward", "CommodityForward", 2),
        make_family<rd::commodity_future_convention>("commodity_future", "CommodityFuture", 5),
        make_family<rd::cross_currency_basis_convention>(
            "cross_currency_basis", "CrossCurrencyBasis", 49),
        make_family<rd::cross_currency_fix_float_convention>(
            "cross_currency_fix_float", "CrossCurrencyFixFloat", 18),
        make_family<rd::deposit_convention>("deposit", "Deposit", 67),
        make_family<rd::fra_convention>("fra", "FRA", 53),
        make_family<rd::future_convention>("future", "Future", 39),
        make_family<rd::fx_option_convention>("fx_option", "FxOption", 36),
        make_family<rd::ibor_index_convention>("ibor_index", "IborIndex", 6),
        make_family<rd::inflation_swap_convention>("inflation_swap", "InflationSwap", 22),
        make_family<rd::intraday_power_load_convention>(
            "intraday_power_load", "IntradayPowerLoad", 1),
        make_family<rd::ois_convention>("ois", "OIS", 68),
        make_family<rd::overnight_index_convention>("overnight_index", "OvernightIndex", 7),
        make_family<rd::swap_convention>("swap", "Swap", 54),
        make_family<rd::swap_index_convention>("swap_index", "SwapIndex", 47),
        make_family<rd::tenor_basis_two_swap_convention>(
            "tenor_basis_two_swap", "TenorBasisTwoSwap", 46),
        make_family<rd::zero_convention>("zero", "Zero", 37),
        make_family<rd::zero_inflation_index_convention>(
            "zero_inflation_index", "ZeroInflationIndex", 6),
    };
}

std::vector<std::string> ids_in(const conventions& document, const std::string& element) {
    std::vector<std::string> ids;
    for (const auto& x : root_of(document).children(element))
        ids.push_back(x.text("Id").value_or(std::string()));
    return ids;
}

}

// Every family of the package lands one row per document element, in document
// order, and the export writes as many elements as the document held. The rows
// that come back from the export are the rows that went in, field for field,
// because the mapper stores canonical spellings and the value is what has to
// survive.
TEST_CASE("every_conventions_family_round_trips", tags) {
    for (const auto& f : families()) {
        INFO("family " << f.key);
        int files = 0;
        for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
            const auto document = load(path);
            const auto expected_ids = ids_in(document, f.element);
            if (expected_ids.empty())
                continue;

            ++files;
            const auto mapped = split_conventions(document);
            INFO(path.string());
            CHECK(f.ids(mapped) == expected_ids);

            const std::string exported = conventions_mapper::write(join_conventions(mapped));
            CHECK(element_count(exported, f.element) == static_cast<int>(expected_ids.size()));

            const auto reparsed = conventions_mapper::read(exported, path.string());
            REQUIRE(ids_in(reparsed, f.element).size() == expected_ids.size());

            const auto remapped = split_conventions(reparsed);
            for (std::size_t i = 0; i < expected_ids.size(); ++i) {
                INFO(f.element << " element " << i);
                CHECK(f.same_row(remapped, mapped, i));
            }
        }
        if (f.files >= 0)
            CHECK(files == f.files);
        else
            WARN(f.key << " in " << files << " file(s)");
    }
}

TEST_CASE("conventions_intraday_power_load_round_trips", tags) {
    std::size_t elements = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        const auto profiles = root_of(document).children("IntradayPowerLoad");
        if (profiles.empty())
            continue;

        elements += profiles.size();
        const auto mapped = split_conventions(document);
        const auto rows = rows_of<ores::refdata::domain::intraday_power_load_convention>(
            mapped, "intraday_power_load");
        REQUIRE(rows.size() == profiles.size());
        for (std::size_t i = 0; i < profiles.size(); ++i) {
            if (const auto profile = profiles[i].child("PowerLoadProfileData")) {
                if (profile->has("ExplicitDates"))
                    CHECK(rows[i].explicit_load_profile.has_value());
                if (profile->has("BusinessDayRules"))
                    CHECK(rows[i].business_day_load_rules.has_value());
            }
        }

        // The two load profiles are written as text columns, so the mapper must
        // not report the category as skipped.
        CHECK(conventions_mapper::unmodelled(document).empty());
    }

    // One of the seventy-two files carries it, in two elements, one of each form.
    CHECK(elements == 2);
}

TEST_CASE("conventions_bond_yield_round_trips", tags) {
    std::size_t elements = 0;
    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root()))
        elements += root_of(load(path)).children("BondYield").size();

    // One of the seventy-two files carries it, in five elements.
    CHECK(elements == 5);
}

// The corpus never sets all nine of the optional fields of this convention, so
// it cannot reach the half of the mapping that reads them. This document sets
// all nine, including a zero-valued fixing days and a false resettable flag,
// which are the two cases a mapper that tested the value rather than the
// presence would drop.
TEST_CASE("conventions_cross_currency_fix_float_optional_fields_survive", tags) {
    const std::string text = R"(<Conventions>
  <CrossCurrencyFixFloat>
    <Id>USD-TRY-XCCY-FIX-FLOAT-CONVENTIONS</Id>
    <SettlementDays>2</SettlementDays>
    <SettlementCalendar>US,UK,TRY</SettlementCalendar>
    <SettlementConvention>F</SettlementConvention>
    <FixedCurrency>TRY</FixedCurrency>
    <FixedFrequency>Semiannual</FixedFrequency>
    <FixedConvention>MF</FixedConvention>
    <FixedDayCounter>A360</FixedDayCounter>
    <Index>USD-LIBOR-3M</Index>
    <EOM>true</EOM>
    <IsResettable>true</IsResettable>
    <FloatIndexIsResettable>false</FloatIndexIsResettable>
    <IncludeSpread>true</IncludeSpread>
    <Lookback>1D</Lookback>
    <FixingDays>0</FixingDays>
    <RateCutoff>2</RateCutoff>
    <IsAveraged>true</IsAveraged>
    <ObservationShift>true</ObservationShift>
  </CrossCurrencyFixFloat>
</Conventions>)";

    const auto document = conventions_mapper::read(text);
    REQUIRE(root_of(document).children("CrossCurrencyFixFloat").size() == 1);

    const auto mapped = split_conventions(document);
    const auto rows = rows_of<ores::refdata::domain::cross_currency_fix_float_convention>(
        mapped, "cross_currency_fix_float");
    REQUIRE(rows.size() == 1);
    const auto& out = rows[0];
    CHECK(out.eom.has_value());
    CHECK(out.is_resettable.has_value());
    CHECK(out.float_index_is_resettable == false);
    CHECK(out.include_spread.has_value());
    REQUIRE(out.lookback.has_value());
    CHECK(*out.lookback == "1D");
    REQUIRE(out.fixing_days.has_value());
    CHECK(*out.fixing_days == 0);
    REQUIRE(out.rate_cutoff.has_value());
    CHECK(*out.rate_cutoff == 2);
    CHECK(out.is_averaged.has_value());
    CHECK(out.observation_shift.has_value());

    const auto remapped = split_conventions(
        conventions_mapper::read(conventions_mapper::write(join_conventions(mapped))));
    const auto again = rows_of<ores::refdata::domain::cross_currency_fix_float_convention>(
        remapped, "cross_currency_fix_float");
    REQUIRE(again.size() == 1);
    CHECK(again[0] == out);
}

TEST_CASE("conventions_inflation_swap_publication_schedules_are_modelled", tags) {
    int files_with_a_publication_schedule = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        const auto swaps = root_of(document).children("InflationSwap");
        const bool carries_a_schedule = std::ranges::any_of(
            swaps, [](const auto& swap) { return swap.has("PublicationSchedule"); });
        if (!carries_a_schedule)
            continue;

        // A schedule of rules, dates and derived groups is written as text
        // columns, so the mapper must not report it as skipped.
        ++files_with_a_publication_schedule;
        CHECK(conventions_mapper::unmodelled(document).count("InflationSwap.PublicationSchedule") ==
              0);
        for (const auto& out : rows_of<ores::refdata::domain::inflation_swap_convention>(
                 split_conventions(document), "inflation_swap")) {
            if (out.publication_schedule_rules)
                CHECK(!out.publication_schedule_rules->empty());
        }
    }

    CHECK(files_with_a_publication_schedule == 2);
}

// The entity holds the index's seven scalar fields and not its rebasing events,
// which ORE types as a list of doubles and the refdata schema has no column
// for. No shipped file sets one, so the corpus walk cannot show what happens
// when one appears. This case builds that document and asserts the mapper
// counts the field instead of dropping it: a skip that is counted keeps the
// file out of the round-trip set, and a skip that is silent loses content.
TEST_CASE("conventions_zero_inflation_index_rebasing_events_are_counted", tags) {
    const std::string without = R"(<Conventions>
  <ZeroInflationIndex>
    <Id>UKRPI</Id>
    <RegionName>UK</RegionName>
    <RegionCode>UK</RegionCode>
    <Revised>false</Revised>
    <Frequency>Monthly</Frequency>
    <AvailabilityLag>1M</AvailabilityLag>
    <Currency>GBP</Currency>
  </ZeroInflationIndex>
</Conventions>)";
    CHECK(conventions_mapper::unmodelled(conventions_mapper::read(without)).empty());

    const std::string with = R"(<Conventions>
  <ZeroInflationIndex>
    <Id>UKRPI</Id>
    <RegionName>UK</RegionName>
    <RegionCode>UK</RegionCode>
    <Revised>false</Revised>
    <Frequency>Monthly</Frequency>
    <AvailabilityLag>1M</AvailabilityLag>
    <Currency>GBP</Currency>
    <RebasingEvents>
      <Event date="2020-01-01">1.5</Event>
    </RebasingEvents>
  </ZeroInflationIndex>
</Conventions>)";
    const auto unmodelled = conventions_mapper::unmodelled(conventions_mapper::read(with));
    REQUIRE(unmodelled.count("ZeroInflationIndex.RebasingEvents") == 1);
    CHECK(unmodelled.at("ZeroInflationIndex.RebasingEvents") == 1);
}

TEST_CASE("conventions_tenor_basis_swap_round_trips", tags) {
    int files = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        const auto swaps = root_of(document).children("TenorBasisSwap");
        if (swaps.empty())
            continue;

        ++files;
        const auto mapped = split_conventions(document);
        const auto centrals = rows_of<ores::refdata::domain::convention>(mapped, "convention");
        const auto pay_receives = rows_of<ores::refdata::domain::tenor_basis_swap_pay_receive>(
            mapped, "tenor_basis_swap_pay_receive");
        const auto long_shorts = rows_of<ores::refdata::domain::tenor_basis_swap_long_short>(
            mapped, "tenor_basis_swap_long_short");
        REQUIRE(centrals.size() == swaps.size());

        for (std::size_t i = 0; i < swaps.size(); ++i) {
            const auto& in = swaps[i];
            const auto& central = centrals[i];
            CHECK(central.kind_code == "TENOR_BASIS_SWAP");
            CHECK(central.name == in.text("Id").value_or(std::string()));

            const auto pay_receive = std::ranges::find_if(
                pay_receives, [&](const auto& p) { return p.convention_id == central.id; });
            const auto long_short = std::ranges::find_if(
                long_shorts, [&](const auto& l) { return l.convention_id == central.id; });
            if (in.has("PayIndex"))
                CHECK(pay_receive->pay_index == in.text("PayIndex"));
            if (in.has("ReceiveIndex"))
                CHECK(pay_receive->receive_index == in.text("ReceiveIndex"));
            if (in.has("LongIndex"))
                CHECK(long_short->long_index == in.text("LongIndex"));
            if (in.has("ShortIndex"))
                CHECK(long_short->short_index == in.text("ShortIndex"));
        }

        const std::string exported = conventions_mapper::write(join_conventions(mapped));
        CHECK(element_count(exported, "TenorBasisSwap") == static_cast<int>(swaps.size()));

        // The mapper mints a provisional key for each convention so its part can
        // reach it inside one mapped document. Two maps of one document agree on
        // the stated key and not on that key, which is why the comparison below
        // pairs the rows by the convention they belong to.
        const auto reparsed = conventions_mapper::read(exported, path.string());
        INFO(path.string());
        CHECK(conventions_difference(document, reparsed, path.string()).empty());
    }

    // Twenty-nine of the seventy-two files carry it.
    CHECK(files == 29);
}

TEST_CASE("conventions_files_using_only_modelled_categories_round_trip", tags) {
    const auto kind = conventions_kind();
    int walked = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        // A file that carries a category the mapper does not model cannot round
        // trip, and saying which those are is the measurement's job rather than
        // this case's.
        if (!conventions_mapper::unmodelled(load(path)).empty())
            continue;

        ++walked;
        const auto outcome = kind.check(path);
        INFO(outcome.detail);
        CHECK(outcome.passed);
    }

    // All seventy-two once the publication schedule and the intraday power load
    // were written as text columns. The count is asserted so that a change in
    // it is noticed rather than absorbed.
    CHECK(walked == 72);
}

// Hidden by default, and run on demand:
//   ores.ore.core.tests "[.][conventions]"
// The categories below are the ones the corpus needs, ordered by how many files
// use them. Modelling starts at the top of that list.
// Which category, modelled next, would clear the most files outright. A file
// round trips only when every one of its categories is modelled, so the
// shortest path is the category that appears alone most often.
TEST_CASE("conventions_files_cleared_by_each_category", "[.][conventions][measurement]") {
    std::map<std::string, int> cleared_by;
    std::map<int, int> files_by_category_count;
    int files = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto unmodelled = conventions_mapper::unmodelled(load(path));
        ++files;
        ++files_by_category_count[static_cast<int>(unmodelled.size())];
        if (unmodelled.size() == 1)
            ++cleared_by[unmodelled.begin()->first];
    }

    WARN("conventions files=" + std::to_string(files));
    for (const auto& [count, file_count] : files_by_category_count) {
        WARN("  " + std::to_string(count) + " unmodelled category(ies) in " +
             std::to_string(file_count) + " file(s)");
    }
    for (const auto& [name, file_count] : cleared_by) {
        WARN("  " + name + " alone would clear " + std::to_string(file_count) + " file(s)");
    }

    CHECK(files == 72);
}

TEST_CASE("conventions_unmodelled_categories_measurement", "[.][conventions][measurement]") {
    std::map<std::string, int> files_by_category;
    std::map<std::string, int> elements_by_category;
    int files = 0;
    int files_with_unmodelled = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto unmodelled = conventions_mapper::unmodelled(load(path));
        ++files;
        if (!unmodelled.empty())
            ++files_with_unmodelled;
        for (const auto& [name, count] : unmodelled) {
            ++files_by_category[name];
            elements_by_category[name] += static_cast<int>(count);
        }
    }

    WARN("conventions files=" + std::to_string(files) +
         " with unmodelled categories=" + std::to_string(files_with_unmodelled));
    for (const auto& [name, count] : files_by_category) {
        WARN("  " + name + " in " + std::to_string(count) + " file(s), " +
             std::to_string(elements_by_category[name]) + " element(s)");
    }

    CHECK(files == 72);
}

namespace {

std::filesystem::path digests_path() {
    return ores::testing::project_root::resolve(
        "projects/ores.ore/core/tests/evidence/conventions_corpus_export.sha256");
}

/**
 * @brief The bytes the export writes for one corpus file.
 *
 * This is the whole pipeline the export runs: parse the file, map it to the
 * store's shape, and write that shape back out.
 */
std::string exported_bytes(const std::filesystem::path& path) {
    return conventions_mapper::write(join_conventions(split_conventions(load(path))));
}

/**
 * @brief The recorded digests, keyed by the path relative to the corpus root.
 *
 * A line holds the digest, a space, then the path. Blank lines and lines
 * starting with a hash are notes.
 */
std::map<std::string, std::string> read_digests(const std::filesystem::path& path) {
    std::ifstream in(path, std::ios::binary);
    std::map<std::string, std::string> digests;
    std::string line;
    while (std::getline(in, line)) {
        if (line.empty() || line.front() == '#')
            continue;
        const auto space = line.find(' ');
        if (space == std::string::npos)
            continue;
        digests[line.substr(space + 1)] = line.substr(0, space);
    }
    return digests;
}

}

TEST_CASE("the exported conventions corpus matches its recorded digests", tags) {
    const auto root = corpus_root();
    const auto recorded = read_digests(digests_path());
    REQUIRE_FALSE(recorded.empty());

    int compared = 0;
    for (const auto& path : ores::ore::xml::files_of_kind("conventions", root)) {
        const auto key = std::filesystem::relative(path, root).generic_string();
        INFO("file: " << key);
        const auto found = recorded.find(key);
        REQUIRE(found != recorded.end());
        CHECK(ores::utility::crypto::sha256::hex_digest(exported_bytes(path)) == found->second);
        ++compared;
    }

    // The corpus is pinned at seventy-two files, so a file the expectations do
    // not name, or one they name that the corpus lost, is a change to the pin
    // rather than a silent shrink.
    CHECK(compared == static_cast<int>(recorded.size()));
}
