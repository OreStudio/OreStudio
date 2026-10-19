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
#include "ores.ore.core/domain/domain.hpp"
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

const std::string tags("[ore][conventions][package]");

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
    conventions document;
    load_data(read(path), document);
    return document;
}

}

TEST_CASE("the_package_carries_one_row_for_every_document_element", tags) {
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
        CHECK(rows_of("deposit") == document.Deposit.size());
        CHECK(rows_of("ois") == document.OIS.size());
        CHECK(rows_of("ibor_index") == document.IborIndex.size());
        CHECK(rows_of("zero_inflation_index") == document.ZeroInflationIndex.size());
        CHECK(rows_of("tenor_basis_two_swap") == document.TenorBasisTwoSwap.size());

        // The one document element that is three families.
        CHECK(rows_of("convention") == document.TenorBasisSwap.size());

        // Every other family packs one row per element, so the rows outside
        // the three tenor basis swap families equal all the elements.
        const std::size_t elements =
            document.AverageOIS.size() + document.BMABasisSwap.size() + document.BondYield.size() +
            document.CDS.size() + document.CmsSpreadOption.size() +
            document.CommodityForward.size() + document.CommodityFuture.size() +
            document.CrossCurrencyBasis.size() + document.CrossCurrencyFixFloat.size() +
            document.Deposit.size() + document.FRA.size() + document.Future.size() +
            document.FxOption.size() + document.IborIndex.size() + document.InflationSwap.size() +
            document.IntradayPowerLoad.size() + document.OIS.size() +
            document.OvernightIndex.size() + document.Swap.size() + document.SwapIndex.size() +
            document.TenorBasisTwoSwap.size() + document.Zero.size() +
            document.ZeroInflationIndex.size();
        const std::size_t outside_tenor_basis_swap =
            package.bundle.rows.size() - rows_of("convention") -
            rows_of("tenor_basis_swap_pay_receive") - rows_of("tenor_basis_swap_long_short");
        CHECK(outside_tenor_basis_swap == elements);
        ++checked;
    }

    CHECK(checked == 72);
}

TEST_CASE("the_package_keeps_the_fx_convention_out_of_the_bundle", tags) {
    int files_with_fx = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (document.FX.empty())
            continue;

        ++files_with_fx;
        const auto package = split_conventions(document);

        INFO(path.string());
        CHECK(package.fx.size() == document.FX.size());
        for (const auto& row : package.bundle.rows)
            CHECK(row.family != "fx");

        // An FX convention is a projection of the pair, its convention and its
        // calendars, so it comes back unchanged and was never stored.
        const auto joined = join_conventions(package);
        REQUIRE(joined.FX.size() == document.FX.size());
        for (std::size_t i = 0; i < document.FX.size(); ++i) {
            CHECK(to_string(joined.FX[i].SourceCurrency) ==
                  to_string(document.FX[i].SourceCurrency));
            CHECK(to_string(joined.FX[i].TargetCurrency) ==
                  to_string(document.FX[i].TargetCurrency));
        }
    }

    CHECK(files_with_fx > 0);
}

TEST_CASE("a_payload_no_family_can_decode_is_refused", tags) {
    conventions_package package;
    package.bundle.rows.push_back({"zero", 0, "not a payload"});

    CHECK_THROWS_AS(join_conventions(package), std::runtime_error);
}

TEST_CASE("a_payload_from_a_family_no_element_carries_is_refused", tags) {
    conventions_package package;
    package.bundle.rows.push_back({"no_such_family", 0, "{}"});

    CHECK_THROWS_AS(join_conventions(package), std::runtime_error);
}

// The one table a bundle reads for the shared family holds every kind, so a
// read hands the join rows that have no document element. Writing one as a
// tenor basis swap would put a leg-less element in the export.
TEST_CASE("a_central_row_of_another_kind_is_not_written_as_a_tenor_basis_swap", tags) {
    ores::refdata::domain::convention c;
    c.kind_code = "SWAP";
    c.name = "not a tenor basis swap";

    conventions_package package;
    package.bundle.rows.push_back({"convention", 0, rfl::json::write(c)});

    CHECK(join_conventions(package).TenorBasisSwap.empty());
}

TEST_CASE("every_family_the_package_names_is_one_the_registry_names", tags) {
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
