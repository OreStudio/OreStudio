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
#include "ores.database/domain/party_scope.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/conventions_package.hpp"
#include "ores.ore.core/domain/curve_configuration_projection.hpp"
#include "ores.ore.core/xml/generated/curve_configuration.hpp"
#include "ores.ore.core/xml/record_diff.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.refdata.core/repository/average_ois_convention_repository.hpp"
#include "ores.refdata.core/repository/business_day_convention_code.hpp"
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
#include "ores.refdata.core/service/curve_configuration_document_service.hpp"
#include "ores.testing/conventions_rows.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.testing/two_parties.hpp"
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
 * The in-memory walk proves the projection over the corpus; this proves the
 * tables hold what the projection produces, and that the database refuses what the model
 * says it must: a convention, a day counter or a segment type it does not hold.
 */

using ores::testing::rows_of;

namespace {

using namespace ores::ore::domain;
using namespace ores::refdata::repository;
using ores::platform::filesystem::file;

const std::string example = "external/ore/examples/Legacy/Example_62/Input/";

using curve_document = curve_configuration_projection::document;

/// The record a curve configuration document states, read through the codec.
curve_document load_document(const std::string& path) {
    return curve_configuration_projection::read(
        file::read_content(ores::testing::project_root::resolve(path)));
}

/// States a section this document copies from another one.
void keep(curve_document& d, std::string_view name) {
    if (!ores::ore::xml::mark_present(
            *ores::ore::xml::generated::curve_configuration::curve_configuration_document().root,
            &d,
            name))
        throw std::runtime_error("the curve configuration table has no " + std::string(name));
}

/// The rows the store receives: the conventions stated as the store keeps them.
auto as_stored(const ores::refdata::messaging::curve_configuration_document& rows) {
    auto stored = rows;
    const auto canonical = [](auto& family) {
        for (auto& row : family)
            if constexpr (requires { row.business_day_convention; })
                row.business_day_convention =
                    ores::refdata::repository::stored_business_day_convention(
                        row.business_day_convention);
    };
    canonical(stored.yield_curves);
    canonical(stored.equity_curves);
    canonical(stored.inflation_curves);
    canonical(stored.default_curves);
    canonical(stored.commodity_curves);
    canonical(stored.fx_volatilities);
    canonical(stored.yield_volatilities);
    canonical(stored.base_correlations);
    canonical(stored.correlations);
    canonical(stored.cds_volatilities);
    canonical(stored.volatility_configs);
    canonical(stored.inflation_cap_floor_volatilities);
    canonical(stored.swaption_volatilities);
    canonical(stored.cap_floor_volatilities);
    canonical(stored.equity_volatilities);
    canonical(stored.commodity_volatilities);
    canonical(stored.bond_future_volatilities);
    return stored;
}

/// Compares two documents as records, naming the first difference.
std::string
compare_documents(const curve_document& lhs, const curve_document& rhs, const std::string& path) {
    const auto difference = ores::ore::xml::compare_records(
        *ores::ore::xml::generated::curve_configuration::curve_configuration_document().root,
        &lhs,
        &rhs);
    if (!difference)
        return {};
    return path + ": " + difference->path + ": " + difference->detail;
}

conventions load_conventions(const std::string& path) {
    return conventions_mapper::read(file::read_content(ores::testing::project_root::resolve(path)),
                                    path);
}

const std::string equity_example = "external/ore/examples/Input/curveconfig.xml";
const std::string products_example = "external/ore/examples/Products/Input/curveconfig.xml";

// A corpus document without its yield and commodity curves, so only the
// inflation swap and CDS conventions have to be written first.
curve_document equity_and_securities() {
    auto d = load_document(equity_example);
    d.yield_curves.yield_curve.clear();
    d.commodity_curves.commodity_curve.clear();
    // Clearing a list leaves the element that held it stated, and a document
    // that states an empty element is what the writing produces, so the
    // document is read back before it is compared with the writing.
    return curve_configuration_projection::read(curve_configuration_projection::write(d));
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
    const auto mapped = split_conventions(load_conventions(example + "conventions.xml"));
    write_missing(ctx,
                  deposit_convention_repository(),
                  rows_of<ores::refdata::domain::deposit_convention>(mapped, "deposit"));
    write_missing(ctx,
                  ois_convention_repository(),
                  rows_of<ores::refdata::domain::ois_convention>(mapped, "ois"));
    write_missing(ctx,
                  average_ois_convention_repository(),
                  rows_of<ores::refdata::domain::average_ois_convention>(mapped, "average_ois"));
}

// The inflation and default curves of the equity example name inflation swap
// and CDS conventions, which the example's own conventions file defines.
void write_input_conventions(const ores::database::context& ctx) {
    const auto mapped =
        split_conventions(load_conventions("external/ore/examples/Input/conventions.xml"));
    write_missing(
        ctx,
        inflation_swap_convention_repository(),
        rows_of<ores::refdata::domain::inflation_swap_convention>(mapped, "inflation_swap"));
    write_missing(ctx,
                  cds_convention_repository(),
                  rows_of<ores::refdata::domain::cds_convention>(mapped, "cds"));
}

// A curve configuration belongs to a party, so every document a case writes is
// stamped with one; the cases that need two parties make their own.
void write(const ores::database::context& ctx,
           ores::refdata::messaging::curve_configuration_document m) {
    static const auto owner = boost::uuids::random_generator()();
    ores::database::domain::assign_party(m, owner);
    ores::refdata::service::curve_configuration_document_service(ctx).save(std::move(m));
}

ores::refdata::messaging::curve_configuration_document
read_back(const ores::database::context& ctx,
          const ores::refdata::messaging::curve_configuration_document& written) {
    return ores::refdata::service::curve_configuration_document_service(ctx).get(written.config.id);
}

}

TEST_CASE("curve_configuration_roundtrip_through_the_database",
          "[curveconfig][database][roundtrip]") {
    auto lg(ores::logging::make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "curve_configuration_roundtrip_through_the_database";
    ores::testing::scoped_database_helper h;
    write_conventions(h.context());

    const auto original = load_document(example + "curveconfig.xml");
    const auto mapped = curve_configuration_projection::map(original);
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

    const auto exported = curve_configuration_projection::read(
        curve_configuration_projection::write(curve_configuration_projection::reverse(back)));
    const auto difference = compare_documents(original, exported, example);
    INFO(difference);
    CHECK(difference.empty());
}

TEST_CASE("a segment naming a convention the tenant does not hold is refused",
          "[curveconfig][database][roundtrip]") {
    auto lg(ores::logging::make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "a segment naming a convention the tenant does not hold is refused";
    ores::testing::scoped_database_helper h;
    write_conventions(h.context());

    auto mapped = curve_configuration_projection::map(load_document(example + "curveconfig.xml"));
    REQUIRE(!mapped.segments.empty());
    mapped.segments.front().conventions = "NO-SUCH-CONVENTION";

    curve_configuration_repository().write(h.context(), mapped.config);
    curve_definition_repository().write(h.context(), mapped.definitions);
    CHECK_THROWS(curve_segment_repository().write(h.context(), mapped.segments.front()));
}

TEST_CASE("a yield curve naming a day counter ORE does not spell is refused",
          "[curveconfig][database][roundtrip]") {
    auto lg(ores::logging::make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "a yield curve naming a day counter ORE does not spell is refused";
    ores::testing::scoped_database_helper h;

    auto mapped = curve_configuration_projection::map(load_document(example + "curveconfig.xml"));
    REQUIRE(!mapped.yield_curves.empty());
    mapped.yield_curves.front().day_counter = "Actual/365 Fixed";

    curve_configuration_repository().write(h.context(), mapped.config);
    curve_definition_repository().write(h.context(), mapped.definitions);
    CHECK_THROWS(yield_curve_config_repository().write(h.context(), mapped.yield_curves.front()));
}

TEST_CASE("a segment of a type the vocabulary does not hold is refused",
          "[curveconfig][database][roundtrip]") {
    auto lg(ores::logging::make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "a segment of a type the vocabulary does not hold is refused";
    ores::testing::scoped_database_helper h;
    write_conventions(h.context());

    auto mapped = curve_configuration_projection::map(load_document(example + "curveconfig.xml"));
    REQUIRE(!mapped.segments.empty());
    mapped.segments.front().segment_type = "Nonexistent";

    curve_configuration_repository().write(h.context(), mapped.config);
    curve_definition_repository().write(h.context(), mapped.definitions);
    CHECK_THROWS(curve_segment_repository().write(h.context(), mapped.segments.front()));
}

TEST_CASE("default, equity and inflation curves and securities round trip through the database",
          "[curveconfig][database][roundtrip]") {
    auto lg(ores::logging::make_logger(test_suite));
    BOOST_LOG_SEV(lg, info)
        << "default, equity and inflation curves and securities round trip through the database";
    ores::testing::scoped_database_helper h;
    write_input_conventions(h.context());

    const auto original = equity_and_securities();
    const auto mapped = curve_configuration_projection::map(original);
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

    const auto exported = curve_configuration_projection::read(
        curve_configuration_projection::write(curve_configuration_projection::reverse(back)));
    /*
     * The store keeps the business day convention, not the spelling the
     * document used, so the document the store received is the one it gives
     * back. Every other field round trips as it was written.
     */
    const auto expected =
        curve_configuration_projection::read(curve_configuration_projection::write(
            curve_configuration_projection::reverse(as_stored(mapped))));
    const auto difference = compare_documents(expected, exported, equity_example);
    INFO(difference);
    CHECK(difference.empty());
}

TEST_CASE("equity and commodity volatility configurations and the report configuration round trip "
          "through the database",
          "[curveconfig][database][roundtrip]") {
    auto lg(ores::logging::make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "equity and commodity volatility round trip";
    ores::testing::scoped_database_helper h;

    const auto c = load_conventions("external/ore/examples/Products/Input/conventions.xml");
    write_missing(h.context(),
                  commodity_future_convention_repository(),
                  rows_of<ores::refdata::domain::commodity_future_convention>(split_conventions(c),
                                                                              "commodity_future"));

    const auto full = load_document(products_example);
    curve_document original;
    original.equity_volatilities = full.equity_volatilities;
    keep(original, "EquityVolatilities");
    original.commodity_volatilities = full.commodity_volatilities;
    keep(original, "CommodityVolatilities");
    original.report_configuration = full.report_configuration;
    keep(original, "ReportConfiguration");
    const auto mapped = curve_configuration_projection::map(original);
    REQUIRE(
        std::ranges::any_of(mapped.volatility_configs, [](const auto& v) { return v.is_wrapped; }));
    REQUIRE(!mapped.global_reports.empty());

    write(h.context(), mapped);
    const auto back = read_back(h.context(), mapped);
    CHECK(back.volatility_configs.size() == mapped.volatility_configs.size());
    CHECK(back.quotes.size() == mapped.quotes.size());
    CHECK(back.global_reports.size() == mapped.global_reports.size());

    const auto exported = curve_configuration_projection::read(
        curve_configuration_projection::write(curve_configuration_projection::reverse(back)));
    const auto difference = compare_documents(original, exported, products_example);
    INFO(difference);
    CHECK(difference.empty());
}

TEST_CASE("swaption and cap and floor proxies and smiles round trip through the database",
          "[curveconfig][database][roundtrip]") {
    auto lg(ores::logging::make_logger(test_suite));
    BOOST_LOG_SEV(lg, info)
        << "swaption and cap and floor proxies and smiles round trip through the database";
    for (const std::string path :
         {"external/ore/examples/Legacy/Example_63/Input/curveconfig.xml",
          "external/ore/examples/CurveBuilding/Input/curveconfig_sabr.xml"}) {
        INFO(path);
        ores::testing::scoped_database_helper h;

        const auto full = load_document(path);
        curve_document original;
        original.swaption_volatilities = full.swaption_volatilities;
        keep(original, "SwaptionVolatilities");
        original.cap_floor_volatilities = full.cap_floor_volatilities;
        keep(original, "CapFloorVolatilities");
        const auto mapped = curve_configuration_projection::map(original);
        // A proxy is a part row now, so its presence is the part's, not a flag.
        const bool proxies = !mapped.cap_floor_proxy_configs.empty();
        CHECK((proxies || !mapped.parametric_smiles.empty()));

        write(h.context(), mapped);
        const auto back = read_back(h.context(), mapped);
        CHECK(back.parametric_smiles.size() == mapped.parametric_smiles.size());
        CHECK(back.parametric_smile_parameters.size() == mapped.parametric_smile_parameters.size());

        const auto exported = curve_configuration_projection::read(
            curve_configuration_projection::write(curve_configuration_projection::reverse(back)));
        const auto expected =
            curve_configuration_projection::read(curve_configuration_projection::write(
                curve_configuration_projection::reverse(as_stored(mapped))));
        const auto difference = compare_documents(expected, exported, path);
        INFO(difference);
        CHECK(difference.empty());
    }
}

TEST_CASE("a CDS volatility's terms and strike surface round trip through the database",
          "[curveconfig][database][roundtrip]") {
    auto lg(ores::logging::make_logger(test_suite));
    BOOST_LOG_SEV(lg, info)
        << "a CDS volatility's terms and strike surface round trip through the database";
    ores::testing::scoped_database_helper h;

    const auto full = load_document(products_example);
    curve_document original;
    original.cds_volatilities = full.cds_volatilities;
    keep(original, "CDSVolatilities");
    const auto mapped = curve_configuration_projection::map(original);
    REQUIRE(!mapped.cds_volatility_terms.empty());
    REQUIRE(mapped.volatility_configs.size() == 1);

    write(h.context(), mapped);
    const auto back = read_back(h.context(), mapped);
    CHECK(back.cds_volatility_terms.size() == mapped.cds_volatility_terms.size());
    CHECK(back.volatility_configs.size() == 1);

    const auto exported = curve_configuration_projection::read(
        curve_configuration_projection::write(curve_configuration_projection::reverse(back)));
    const auto difference = compare_documents(original, exported, products_example);
    INFO(difference);
    CHECK(difference.empty());
}

TEST_CASE("an FX volatility's report configuration round trips through the database",
          "[curveconfig][database][roundtrip]") {
    auto lg(ores::logging::make_logger(test_suite));
    BOOST_LOG_SEV(lg, info)
        << "an FX volatility's report configuration round trips through the database";
    ores::testing::scoped_database_helper h;
    write_input_conventions(h.context());

    // A volatility entry's report states its own columns, which the document
    // writes under the entry.
    const auto original = curve_configuration_projection::read(R"(<CurveConfiguration>
  <FXVolatilities>
    <FXVolatility>
      <CurveId>EURUSD</CurveId>
      <CurveDescription>A report</CurveDescription>
      <Dimension>Smile</Dimension>
      <Report>
        <ReportOnDeltaGrid>Y</ReportOnDeltaGrid>
        <Deltas>10P, ATM, 10C</Deltas>
        <Expiries>1M, 1Y</Expiries>
      </Report>
    </FXVolatility>
  </FXVolatilities>
</CurveConfiguration>)");
    const auto mapped = curve_configuration_projection::map(original);
    REQUIRE(mapped.report_configurations.size() == 1);

    write(h.context(), mapped);
    const auto back = read_back(h.context(), mapped);
    REQUIRE(back.report_configurations.size() == 1);
    CHECK(back.report_configurations.front().deltas == "10P, ATM, 10C");

    const auto exported = curve_configuration_projection::read(
        curve_configuration_projection::write(curve_configuration_projection::reverse(back)));
    const auto difference = compare_documents(original, exported, equity_example);
    INFO(difference);
    CHECK(difference.empty());
}

TEST_CASE("an equity curve naming a calendar ORE does not accept is refused",
          "[curveconfig][database][roundtrip]") {
    auto lg(ores::logging::make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "an equity curve naming a calendar ORE does not accept is refused";
    ores::testing::scoped_database_helper h;

    auto mapped = curve_configuration_projection::map(equity_and_securities());
    REQUIRE(!mapped.equity_curves.empty());
    mapped.equity_curves.front().calendar = "JoinHolidays(TARGET, Atlantis)";

    curve_configuration_repository().write(h.context(), mapped.config);
    curve_definition_repository().write(h.context(), mapped.definitions);
    CHECK_THROWS(equity_curve_config_repository().write(h.context(), mapped.equity_curves.front()));
}

TEST_CASE("an equity curve naming a joined calendar ORE accepts is stored",
          "[curveconfig][database][roundtrip]") {
    auto lg(ores::logging::make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "an equity curve naming a joined calendar ORE accepts is stored";
    ores::testing::scoped_database_helper h;

    auto mapped = curve_configuration_projection::map(equity_and_securities());
    REQUIRE(!mapped.equity_curves.empty());
    mapped.equity_curves.front().calendar =
        "JoinHolidays(TARGET, US settlement, XNYS, CUSTOM_DESK)";

    curve_configuration_repository().write(h.context(), mapped.config);
    curve_definition_repository().write(h.context(), mapped.definitions);
    CHECK_NOTHROW(
        equity_curve_config_repository().write(h.context(), mapped.equity_curves.front()));
}

TEST_CASE("a party sees only its own curve configuration", "[curveconfig][database][roundtrip]") {
    auto lg(ores::logging::make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "a party sees only its own curve configuration";
    ores::testing::scoped_database_helper h;
    auto parties = ores::testing::make_two_parties(h);
    auto mapped = curve_configuration_projection::map(equity_and_securities());
    ores::database::domain::assign_party(mapped, parties.a);

    curve_configuration_repository repo;
    repo.write(parties.a_context, mapped.config);

    const auto owns = [&](const auto& rows) {
        return std::ranges::any_of(rows, [&](const auto& r) { return r.id == mapped.config.id; });
    };
    CHECK(owns(repo.read_latest(parties.a_context)));
    CHECK_FALSE(owns(repo.read_latest(parties.b_context)));
}
