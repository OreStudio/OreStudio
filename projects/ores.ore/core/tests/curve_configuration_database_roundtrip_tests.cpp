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
#include "ores.ore.core/domain/curve_configuration_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/party_scope.hpp"
#include "ores.ore.core/store/document_store.hpp"
#include "ores.ore.core/xml/roundtrip_harness.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.refdata.core/repository/average_ois_convention_repository.hpp"
#include "ores.refdata.core/repository/cds_convention_repository.hpp"
#include "ores.refdata.core/repository/commodity_future_convention_repository.hpp"
#include "ores.refdata.core/repository/curve_configuration_repository.hpp"
#include "ores.refdata.core/repository/curve_definition_repository.hpp"
#include "ores.refdata.core/repository/curve_segment_repository.hpp"
#include "ores.refdata.core/repository/deposit_convention_repository.hpp"
#include "ores.refdata.core/repository/equity_curve_config_repository.hpp"
#include "ores.refdata.core/repository/inflation_swap_convention_repository.hpp"
#include "ores.refdata.core/repository/ois_convention_repository.hpp"
#include "ores.refdata.core/repository/yield_curve_config_repository.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "party_fixture.hpp"
#include <boost/uuid/random_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>

/**
 * @file curve_configuration_database_roundtrip_tests.cpp
 * @brief The curve configuration through the database.
 *
 * The in-memory walk proves the mapper over the corpus; this proves the tables
 * hold what the mapper produces, and that the database refuses what the model
 * says it must: a convention, a day counter or a segment type it does not hold.
 */

namespace {

const std::string tags("[curveconfig][database][roundtrip]");

using namespace ores::ore::domain;
using namespace ores::refdata::repository;
using ores::platform::filesystem::file;

const std::string example = "external/ore/examples/Legacy/Example_62/Input/";

template <typename Document>
Document load(const std::string& name) {
    Document d;
    load_data(file::read_content(ores::testing::project_root::resolve(example + name)), d);
    return d;
}

const std::string equity_example = "external/ore/examples/Input/curveconfig.xml";
const std::string products_example = "external/ore/examples/Products/Input/curveconfig.xml";

// A corpus document without its yield and commodity curves, so only the
// inflation swap and CDS conventions have to be written first.
curveconfiguration equity_and_securities() {
    curveconfiguration d;
    load_data(file::read_content(ores::testing::project_root::resolve(equity_example)), d);
    if (d.YieldCurves)
        d.YieldCurves->YieldCurve.clear();
    if (d.CommodityCurves)
        d.CommodityCurves->CommodityCurve.clear();
    return d;
}

// Writes the conventions of one kind that the tenant does not hold yet. The
// test tenant is shared by every case in a run, so a second case finds them
// already written.
template <typename Repository, typename Row>
void write_missing(const ores::database::context& ctx,
                   Repository repo,
                   const std::vector<Row>& rows) {
    std::set<std::string> held;
    for (const auto& r : repo.read_latest(ctx))
        held.insert(r.id);
    std::vector<Row> missing;
    for (const auto& r : rows)
        if (!held.contains(r.id))
            missing.push_back(r);
    if (!missing.empty())
        repo.write(ctx, missing);
}

// The conventions the example's curves name, written so the segment trigger
// can resolve them. Only these three kinds are written, because the curves
// name nothing else.
void write_conventions(const ores::database::context& ctx) {
    const auto mapped = conventions_mapper::map(load<conventions>("conventions.xml"));
    write_missing(ctx, deposit_convention_repository(), mapped.deposit);
    write_missing(ctx, ois_convention_repository(), mapped.ois);
    write_missing(ctx, average_ois_convention_repository(), mapped.average_ois);
}

// The inflation and default curves of the equity example name inflation swap
// and CDS conventions, which the example's own conventions file defines.
void write_input_conventions(const ores::database::context& ctx) {
    conventions c;
    load_data(file::read_content(ores::testing::project_root::resolve(
                  "external/ore/examples/Input/conventions.xml")),
              c);
    const auto mapped = conventions_mapper::map(c);
    write_missing(ctx, inflation_swap_convention_repository(), mapped.inflation_swap);
    write_missing(ctx, cds_convention_repository(), mapped.cds);
}

// A curve configuration belongs to a party, so every document a case writes is
// stamped with one; the cases that need two parties make their own.
void write(const ores::database::context& ctx, mapped_curve_configuration m) {
    static const auto owner = boost::uuids::random_generator()();
    ores::ore::domain::assign_party(m, owner);
    ores::ore::store::write(ctx, std::move(m));
}

mapped_curve_configuration read_back(const ores::database::context& ctx,
                                     const mapped_curve_configuration& written) {
    return ores::ore::store::read_curve_configuration(ctx, written.config.id);
}

}

TEST_CASE("curve_configuration_roundtrip_through_the_database", tags) {
    ores::testing::scoped_database_helper h;
    write_conventions(h.context());

    const auto original = load<curveconfiguration>("curveconfig.xml");
    const auto mapped = curve_configuration_mapper::map(original);
    REQUIRE(!mapped.definitions.empty());
    REQUIRE(!mapped.segments.empty());
    REQUIRE(!mapped.quotes.empty());

    write(h.context(), mapped);
    const auto back = read_back(h.context(), mapped);

    CHECK(back.sections.size() == mapped.sections.size());
    CHECK(back.definitions.size() == mapped.definitions.size());
    CHECK(back.yield_curves.size() == mapped.yield_curves.size());
    CHECK(back.segments.size() == mapped.segments.size());
    CHECK(back.quotes.size() == mapped.quotes.size());

    curveconfiguration exported;
    load_data(save_data(curve_configuration_mapper::reverse(back)), exported);
    const auto difference = ores::ore::xml::parsed_text_difference(original, exported, example);
    INFO(difference);
    CHECK(difference.empty());
}

TEST_CASE("a segment naming a convention the tenant does not hold is refused", tags) {
    ores::testing::scoped_database_helper h;
    write_conventions(h.context());

    auto mapped = curve_configuration_mapper::map(load<curveconfiguration>("curveconfig.xml"));
    REQUIRE(!mapped.segments.empty());
    mapped.segments.front().conventions = "NO-SUCH-CONVENTION";

    curve_configuration_repository().write(h.context(), mapped.config);
    curve_definition_repository().write(h.context(), mapped.definitions);
    CHECK_THROWS(curve_segment_repository().write(h.context(), mapped.segments.front()));
}

TEST_CASE("a yield curve naming a day counter ORE does not spell is refused", tags) {
    ores::testing::scoped_database_helper h;

    auto mapped = curve_configuration_mapper::map(load<curveconfiguration>("curveconfig.xml"));
    REQUIRE(!mapped.yield_curves.empty());
    mapped.yield_curves.front().day_counter = "Actual/365 Fixed";

    curve_configuration_repository().write(h.context(), mapped.config);
    curve_definition_repository().write(h.context(), mapped.definitions);
    CHECK_THROWS(yield_curve_config_repository().write(h.context(), mapped.yield_curves.front()));
}

TEST_CASE("a segment of a type the vocabulary does not hold is refused", tags) {
    ores::testing::scoped_database_helper h;
    write_conventions(h.context());

    auto mapped = curve_configuration_mapper::map(load<curveconfiguration>("curveconfig.xml"));
    REQUIRE(!mapped.segments.empty());
    mapped.segments.front().segment_type = "Nonexistent";

    curve_configuration_repository().write(h.context(), mapped.config);
    curve_definition_repository().write(h.context(), mapped.definitions);
    CHECK_THROWS(curve_segment_repository().write(h.context(), mapped.segments.front()));
}

TEST_CASE("default, equity and inflation curves and securities round trip through the database",
          tags) {
    ores::testing::scoped_database_helper h;
    write_input_conventions(h.context());

    const auto original = equity_and_securities();
    const auto mapped = curve_configuration_mapper::map(original);
    REQUIRE(!mapped.equity_curves.empty());
    REQUIRE(!mapped.securities.empty());
    REQUIRE(!mapped.inflation_curves.empty());
    REQUIRE(!mapped.default_curve_configurations.empty());
    REQUIRE(!mapped.fx_volatilities.empty());
    REQUIRE(!mapped.base_correlations.empty());
    REQUIRE(!mapped.correlations.empty());
    REQUIRE(!mapped.cds_volatilities.empty());
    REQUIRE(!mapped.inflation_cap_floor_volatilities.empty());
    REQUIRE(!mapped.swaption_volatilities.empty());
    REQUIRE(!mapped.cap_floor_volatilities.empty());
    REQUIRE(!mapped.equity_volatilities.empty());

    write(h.context(), mapped);
    const auto back = read_back(h.context(), mapped);
    CHECK(back.equity_curves.size() == mapped.equity_curves.size());
    CHECK(back.securities.size() == mapped.securities.size());
    CHECK(back.inflation_curves.size() == mapped.inflation_curves.size());
    CHECK(back.default_curve_configurations.size() == mapped.default_curve_configurations.size());
    CHECK(back.seasonality_factors.size() == mapped.seasonality_factors.size());
    CHECK(back.fx_volatilities.size() == mapped.fx_volatilities.size());
    CHECK(back.base_correlations.size() == mapped.base_correlations.size());
    CHECK(back.correlations.size() == mapped.correlations.size());
    CHECK(back.cds_volatilities.size() == mapped.cds_volatilities.size());
    CHECK(back.inflation_cap_floor_volatilities.size() ==
          mapped.inflation_cap_floor_volatilities.size());
    CHECK(back.swaption_volatilities.size() == mapped.swaption_volatilities.size());
    CHECK(back.cap_floor_volatilities.size() == mapped.cap_floor_volatilities.size());
    CHECK(back.equity_volatilities.size() == mapped.equity_volatilities.size());
    CHECK(back.commodity_volatilities.size() == mapped.commodity_volatilities.size());
    CHECK(back.volatility_configs.size() == mapped.volatility_configs.size());
    CHECK(back.quotes.size() == mapped.quotes.size());

    curveconfiguration exported;
    load_data(save_data(curve_configuration_mapper::reverse(back)), exported);
    const auto difference =
        ores::ore::xml::parsed_text_difference(original, exported, equity_example);
    INFO(difference);
    CHECK(difference.empty());
}

TEST_CASE("equity and commodity volatility configurations and the report configuration round trip "
          "through the database",
          tags) {
    ores::testing::scoped_database_helper h;

    conventions c;
    load_data(file::read_content(ores::testing::project_root::resolve(
                  "external/ore/examples/Products/Input/conventions.xml")),
              c);
    write_missing(h.context(),
                  commodity_future_convention_repository(),
                  conventions_mapper::map(c).commodity_future);

    curveconfiguration full;
    load_data(file::read_content(ores::testing::project_root::resolve(products_example)), full);
    curveconfiguration original;
    original.EquityVolatilities = full.EquityVolatilities;
    original.CommodityVolatilities = full.CommodityVolatilities;
    original.ReportConfiguration = full.ReportConfiguration;
    const auto mapped = curve_configuration_mapper::map(original);
    REQUIRE(
        std::ranges::any_of(mapped.volatility_configs, [](const auto& v) { return v.is_wrapped; }));
    REQUIRE(!mapped.global_reports.empty());

    write(h.context(), mapped);
    const auto back = read_back(h.context(), mapped);
    CHECK(back.volatility_configs.size() == mapped.volatility_configs.size());
    CHECK(back.quotes.size() == mapped.quotes.size());
    CHECK(back.global_reports.size() == mapped.global_reports.size());

    curveconfiguration exported;
    load_data(save_data(curve_configuration_mapper::reverse(back)), exported);
    const auto difference =
        ores::ore::xml::parsed_text_difference(original, exported, products_example);
    INFO(difference);
    CHECK(difference.empty());
}

TEST_CASE("swaption and cap and floor proxies and smiles round trip through the database", tags) {
    for (const std::string path :
         {"external/ore/examples/Legacy/Example_63/Input/curveconfig.xml",
          "external/ore/examples/CurveBuilding/Input/curveconfig_sabr.xml"}) {
        INFO(path);
        ores::testing::scoped_database_helper h;

        curveconfiguration full;
        load_data(file::read_content(ores::testing::project_root::resolve(path)), full);
        curveconfiguration original;
        original.SwaptionVolatilities = full.SwaptionVolatilities;
        original.CapFloorVolatilities = full.CapFloorVolatilities;
        const auto mapped = curve_configuration_mapper::map(original);
        const bool proxies = std::ranges::any_of(mapped.cap_floor_volatilities,
                                                 [](const auto& c) { return c.has_proxy_config; });
        CHECK((proxies || !mapped.parametric_smiles.empty()));

        write(h.context(), mapped);
        const auto back = read_back(h.context(), mapped);
        CHECK(back.parametric_smiles.size() == mapped.parametric_smiles.size());
        CHECK(back.parametric_smile_parameters.size() == mapped.parametric_smile_parameters.size());

        curveconfiguration exported;
        load_data(save_data(curve_configuration_mapper::reverse(back)), exported);
        const auto difference = ores::ore::xml::parsed_text_difference(original, exported, path);
        INFO(difference);
        CHECK(difference.empty());
    }
}

TEST_CASE("a CDS volatility's terms and strike surface round trip through the database", tags) {
    ores::testing::scoped_database_helper h;

    curveconfiguration full;
    load_data(file::read_content(ores::testing::project_root::resolve(products_example)), full);
    REQUIRE(full.CDSVolatilities);
    curveconfiguration original;
    original.CDSVolatilities = full.CDSVolatilities;
    const auto mapped = curve_configuration_mapper::map(original);
    REQUIRE(!mapped.cds_volatility_terms.empty());
    REQUIRE(mapped.volatility_configs.size() == 1);

    write(h.context(), mapped);
    const auto back = read_back(h.context(), mapped);
    CHECK(back.cds_volatility_terms.size() == mapped.cds_volatility_terms.size());
    CHECK(back.volatility_configs.size() == 1);

    curveconfiguration exported;
    load_data(save_data(curve_configuration_mapper::reverse(back)), exported);
    const auto difference =
        ores::ore::xml::parsed_text_difference(original, exported, products_example);
    INFO(difference);
    CHECK(difference.empty());
}

TEST_CASE("an FX volatility's report configuration round trips through the database", tags) {
    ores::testing::scoped_database_helper h;
    write_input_conventions(h.context());

    auto original = equity_and_securities();
    REQUIRE(original.FXVolatilities);
    REQUIRE(!original.FXVolatilities->FXVolatility.empty());
    reportConfiguration report;
    report.ReportOnDeltaGrid = bool_::true_;
    report.Deltas = reportConfiguration_Deltas_t{};
    static_cast<std::string&>(*report.Deltas) = "10P, ATM, 10C";
    report.Expiries = reportConfiguration_Expiries_t{};
    static_cast<std::string&>(*report.Expiries) = "1M, 1Y";
    original.FXVolatilities->FXVolatility.front().Report = report;
    const auto mapped = curve_configuration_mapper::map(original);
    REQUIRE(mapped.report_configurations.size() == 1);

    write(h.context(), mapped);
    const auto back = read_back(h.context(), mapped);
    REQUIRE(back.report_configurations.size() == 1);
    CHECK(back.report_configurations.front().deltas == "10P, ATM, 10C");

    curveconfiguration exported;
    load_data(save_data(curve_configuration_mapper::reverse(back)), exported);
    const auto difference =
        ores::ore::xml::parsed_text_difference(original, exported, equity_example);
    INFO(difference);
    CHECK(difference.empty());
}

TEST_CASE("an equity curve naming a calendar ORE does not accept is refused", tags) {
    ores::testing::scoped_database_helper h;

    auto mapped = curve_configuration_mapper::map(equity_and_securities());
    REQUIRE(!mapped.equity_curves.empty());
    mapped.equity_curves.front().calendar = "JoinHolidays(TARGET, Atlantis)";

    curve_configuration_repository().write(h.context(), mapped.config);
    curve_definition_repository().write(h.context(), mapped.definitions);
    CHECK_THROWS(equity_curve_config_repository().write(h.context(), mapped.equity_curves.front()));
}

TEST_CASE("an equity curve naming a joined calendar ORE accepts is stored", tags) {
    ores::testing::scoped_database_helper h;

    auto mapped = curve_configuration_mapper::map(equity_and_securities());
    REQUIRE(!mapped.equity_curves.empty());
    mapped.equity_curves.front().calendar =
        "JoinHolidays(TARGET, US settlement, XNYS, CUSTOM_DESK)";

    curve_configuration_repository().write(h.context(), mapped.config);
    curve_definition_repository().write(h.context(), mapped.definitions);
    CHECK_NOTHROW(
        equity_curve_config_repository().write(h.context(), mapped.equity_curves.front()));
}

TEST_CASE("a party sees only its own curve configuration", tags) {
    ores::testing::scoped_database_helper h;
    auto parties = ores::ore::tests::make_two_parties(h);
    auto mapped = curve_configuration_mapper::map(equity_and_securities());
    ores::ore::domain::assign_party(mapped, parties.a);

    curve_configuration_repository repo;
    repo.write(parties.a_context, mapped.config);

    const auto owns = [&](const auto& rows) {
        return std::ranges::any_of(rows, [&](const auto& r) { return r.id == mapped.config.id; });
    };
    CHECK(owns(repo.read_latest(parties.a_context)));
    CHECK_FALSE(owns(repo.read_latest(parties.b_context)));
}
