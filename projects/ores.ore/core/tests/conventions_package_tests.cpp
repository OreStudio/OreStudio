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
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/conventions_package.hpp"
#include "ores.ore.core/xml/record.hpp"
#include "ores.ore.core/xml/roundtrip_harness.hpp"
#include "ores.refdata.api/domain/convention_family_registry.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <rfl/json.hpp>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>

/**
 * @file conventions_package_tests.cpp
 * @brief The ORE document split at the family boundary, and put back together.
 *
 * The package is the mapper's intermediate shape. The corpus is the comparison:
 * every file carries as many rows as it has elements, and the export bytes the
 * round trip tests pin come out of the package.
 */

namespace {

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

std::size_t count(const conventions& document, std::string_view element) {
    return root_of(document).children(element).size();
}

}

TEST_CASE("the_package_carries_one_row_for_every_document_element", "[ore][conventions][package]") {
    int checked = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        const auto package = split_conventions(document);

        const auto rows_of = [&package](std::string_view family) {
            std::size_t count = 0;
            for (const auto& row : package.bundle.rows)
                if (row.family == family)
                    ++count;
            return count;
        };

        INFO(path.string());
        CHECK(rows_of("deposit") == count(document, "Deposit"));
        CHECK(rows_of("ois") == count(document, "OIS"));
        CHECK(rows_of("ibor_index") == count(document, "IborIndex"));
        CHECK(rows_of("zero_inflation_index") == count(document, "ZeroInflationIndex"));
        CHECK(rows_of("tenor_basis_two_swap") == count(document, "TenorBasisTwoSwap"));

        // The one document element that is three families.
        CHECK(rows_of("convention") == count(document, "TenorBasisSwap"));

        // Every other family packs one row per element, so the rows outside
        // the three tenor basis swap families equal all the elements.
        const std::size_t elements =
            count(document, "AverageOIS") + count(document, "BMABasisSwap") +
            count(document, "BondYield") + count(document, "CDS") +
            count(document, "CmsSpreadOption") + count(document, "CommodityForward") +
            count(document, "CommodityFuture") + count(document, "CrossCurrencyBasis") +
            count(document, "CrossCurrencyFixFloat") + count(document, "Deposit") +
            count(document, "FRA") + count(document, "Future") + count(document, "FxOption") +
            count(document, "IborIndex") + count(document, "InflationSwap") +
            count(document, "IntradayPowerLoad") + count(document, "OIS") +
            count(document, "OvernightIndex") + count(document, "Swap") +
            count(document, "SwapIndex") + count(document, "TenorBasisTwoSwap") +
            count(document, "Zero") + count(document, "ZeroInflationIndex");
        const std::size_t outside_tenor_basis_swap =
            package.bundle.rows.size() - rows_of("convention") -
            rows_of("tenor_basis_swap_pay_receive") - rows_of("tenor_basis_swap_long_short");
        CHECK(outside_tenor_basis_swap == elements);
        ++checked;
    }

    CHECK(checked == 72);
}

TEST_CASE("the_package_keeps_the_fx_convention_out_of_the_bundle", "[ore][conventions][package]") {
    int files_with_fx = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (count(document, "FX") == 0)
            continue;

        ++files_with_fx;
        const auto package = split_conventions(document);

        INFO(path.string());
        CHECK(package.fx.size() == count(document, "FX"));
        for (const auto& row : package.bundle.rows)
            CHECK(row.family != "fx");

        // An FX convention is a projection of the pair, its convention and its
        // calendars, so it comes back unchanged and was never stored.
        const auto joined = join_conventions(package);
        REQUIRE(count(joined, "FX") == count(document, "FX"));
        const auto before = root_of(document).children("FX");
        const auto after = root_of(joined).children("FX");
        for (std::size_t i = 0; i < before.size(); ++i) {
            CHECK(after[i].spelling("SourceCurrency") == before[i].spelling("SourceCurrency"));
            CHECK(after[i].spelling("TargetCurrency") == before[i].spelling("TargetCurrency"));
        }
    }

    CHECK(files_with_fx > 0);
}

TEST_CASE("a_payload_no_family_can_decode_is_refused", "[ore][conventions][package]") {
    conventions_package package;
    package.bundle.rows.push_back({"zero", 0, "not a payload"});

    CHECK_THROWS_AS(join_conventions(package), std::runtime_error);
}

TEST_CASE("a_payload_from_a_family_no_element_carries_is_refused", "[ore][conventions][package]") {
    conventions_package package;
    package.bundle.rows.push_back({"no_such_family", 0, "{}"});

    CHECK_THROWS_AS(join_conventions(package), std::runtime_error);
}

// The one table a bundle reads for the shared family holds every kind, so a
// read hands the join rows that have no document element. Writing one as a
// tenor basis swap would put a leg-less element in the export.
TEST_CASE("a_central_row_of_another_kind_is_not_written_as_a_tenor_basis_swap",
          "[ore][conventions][package]") {
    ores::refdata::domain::convention c;
    c.kind_code = "SWAP";
    c.name = "not a tenor basis swap";

    conventions_package package;
    package.bundle.rows.push_back({"convention", 0, rfl::json::write(c)});

    CHECK(count(join_conventions(package), "TenorBasisSwap") == 0);
}

TEST_CASE("every_family_the_package_names_is_one_the_registry_names",
          "[ore][conventions][package]") {
    std::set<std::string> seen;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto package = split_conventions(load(path));
        for (const auto& row : package.bundle.rows) {
            INFO(path.string() << " " << row.family);
            CHECK(std::ranges::any_of(
                ores::refdata::domain::convention_families,
                [&row](const auto& family) { return family.key == row.family; }));
            seen.insert(row.family);
        }
    }

    std::set<std::string> registry;
    for (const auto& family : ores::refdata::domain::convention_families)
        registry.insert(std::string(family.key));

    // The corpus is what the equivalence test compares, so a family it never
    // exercises is a family the package has never been shown to carry.
    CHECK(seen == registry);
}

// The central convention family holds only the tenor basis swap rows, because
// every other kind packs into a family of its own. The difference therefore
// still sees a changed deposit, which is what these two cases pin.
TEST_CASE("the_difference_of_a_document_with_itself_is_empty", "[ore][conventions][package]") {
    int walked = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (!conventions_mapper::unmodelled(document).empty())
            continue;

        ++walked;
        INFO(path.string());
        CHECK(conventions_difference(document, document, path.string()).empty());
    }

    CHECK(walked > 0);
}

TEST_CASE("the_difference_reports_a_changed_non_tenor_convention", "[ore][conventions][package]") {
    int changed = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        auto document = load(path);
        if (count(document, "Deposit") == 0 || !conventions_mapper::unmodelled(document).empty())
            continue;

        auto edited = document;
        edited.deposit.front().id += "-CHANGED";

        ++changed;
        INFO(path.string());
        CHECK_FALSE(conventions_difference(document, edited, path.string()).empty());
    }

    CHECK(changed > 0);
}
