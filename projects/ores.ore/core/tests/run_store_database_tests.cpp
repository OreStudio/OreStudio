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
#include "conventions_rows.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/conventions_package.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/todays_market_projection.hpp"
#include "ores.ore.core/store/run_store.hpp"
#include "ores.ore.core/xml/record_diff.hpp"
#include "ores.ore.core/xml/roundtrip_harness.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.refdata.core/repository/convention_repository.hpp"
#include "ores.refdata.core/repository/deposit_convention_repository.hpp"
#include "ores.refdata.core/repository/swap_convention_repository.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_long_short_repository.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_pay_receive_repository.hpp"
#include "ores.refdata.core/service/conventions_bundle.hpp"
#include "ores.reporting.api/generators/report_definition_generator.hpp"
#include "ores.reporting.core/repository/report_configuration_repository.hpp"
#include "ores.reporting.core/repository/report_definition_repository.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "party_fixture.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <filesystem>
#include <map>
#include <set>
#include <string>
#include <string_view>

namespace {

const std::string tags("[ore][store][database]");

using namespace ores::ore;
using ores::ore::tests::rows_of;
using ores::platform::filesystem::file;

const std::string example = "external/ore/examples/ORE-Python/Notebooks/Example_1/Input/";

store::input_files example_input() {
    store::input_files files;
    const auto dir = ores::testing::project_root::resolve(example);
    for (const auto& entry : std::filesystem::directory_iterator(dir))
        if (entry.is_regular_file())
            files[entry.path().filename().string()] = file::read_content(entry.path());
    return files;
}

// A binding names a report definition that must exist, so each case writes a
// risk report definition owned by the party.
boost::uuids::uuid make_definition(ores::testing::scoped_database_helper& h,
                                   const boost::uuids::uuid& party,
                                   const ores::database::context& ctx) {
    auto gen = ores::testing::make_generation_context(h);
    auto d = ores::reporting::generators::generate_synthetic_report_definition(gen);
    d.change_reason_code = "system.test";
    d.report_type = "risk";
    d.party_id = party;
    ores::reporting::repository::report_definition_repository().write(ctx, d);
    return d.id;
}

boost::uuids::uuid make_definition(ores::testing::scoped_database_helper& h,
                                   const ores::ore::tests::two_parties& parties) {
    return make_definition(h, parties.a, parties.a_context);
}

template <typename Document>
Document parse(const std::string& content) {
    Document d;
    domain::load_data(content, d);
    return d;
}

template <typename Document>
std::string difference(const store::input_files& original,
                       const store::input_files& exported,
                       const std::string& name) {
    const auto it = exported.find(name);
    if (it == exported.end())
        return name + " was not exported";
    return xml::parsed_text_difference(
        parse<Document>(original.at(name)), parse<Document>(it->second), example + name);
}

// Today's market is compared by the table, as the family is read through it.
std::string market_difference(const store::input_files& original,
                              const store::input_files& exported,
                              const std::string& name) {
    const auto it = exported.find(name);
    if (it == exported.end())
        return name + " was not exported";
    using projection = domain::todays_market_projection;
    const auto left = projection::read(original.at(name), example + name);
    const auto right = projection::read(it->second, name);
    const auto found = xml::compare_records(
        *xml::generated::todays_market::todays_market_document().root, &left, &right);
    return found ? example + name + ": " + found->path + ": " + found->detail : std::string();
}

std::map<std::string, std::string> parameters_of(const domain::parameterListType& list) {
    std::map<std::string, std::string> out;
    for (const auto& p : list.Parameter)
        out[std::string(p.name)] = static_cast<const std::string&>(p);
    return out;
}

std::string type_of(const domain::analyticsType_Analytic_t& a) {
    return a.type ? std::string(*a.type) : std::string();
}

template <typename Rows>
std::set<std::string> ids_of(const Rows& rows) {
    std::set<std::string> out;
    for (const auto& r : rows)
        out.insert(r.id);
    return out;
}

}

TEST_CASE("a run's configuration imports and exports as the same files", tags) {
    ores::testing::scoped_database_helper h;
    const auto parties = ores::ore::tests::make_two_parties(h);
    const auto definition = make_definition(h, parties);
    const auto original = example_input();

    const auto imported = store::import_run(parties.a_context, definition, "Example_1", original);
    CHECK(imported.stored == std::vector<std::string>{"ore.xml",
                                                      "conventions.xml",
                                                      "curveconfig.xml",
                                                      "todaysmarket.xml",
                                                      "pricingengine.xml"});
    CHECK(std::ranges::find(imported.not_stored, "portfolio.xml") != imported.not_stored.end());
    CHECK(imported.fx_conventions_skipped == std::vector<std::string>{"EUR-GBP-FX-CONVENTIONS"});

    const auto exported = store::export_run(parties.a_context, definition);
    CHECK(exported.size() == imported.stored.size());

    CHECK(difference<domain::pricingengines>(original, exported, "pricingengine.xml").empty());
    CHECK(market_difference(original, exported, "todaysmarket.xml").empty());
    CHECK(difference<domain::curveconfiguration>(original, exported, "curveconfig.xml").empty());

    const auto run = parse<domain::ore>(original.at("ore.xml"));
    const auto exported_run = parse<domain::ore>(exported.at("ore.xml"));
    // The export names every file it hands the packaging step, the run's own
    // data files included. The portfolio, the market data and the fixings belong
    // to no configuration, but the packaging step places the gathered market
    // data and fixings where the run document says they are, so a document that
    // named none would leave them out of the package. The names come from the
    // file type catalogue rather than from the document, which is why the
    // example's names come back as the catalogue spells them.
    CHECK(parameters_of(exported_run.Setup) == parameters_of(run.Setup));
    REQUIRE(exported_run.Markets);
    CHECK(parameters_of(*exported_run.Markets) == parameters_of(*run.Markets));
    REQUIRE(exported_run.Analytics.Analytic.size() == run.Analytics.Analytic.size());
    for (std::size_t i = 0; i < run.Analytics.Analytic.size(); ++i) {
        INFO("analytic " << i);
        CHECK(type_of(exported_run.Analytics.Analytic[i]) == type_of(run.Analytics.Analytic[i]));
        CHECK(parameters_of(exported_run.Analytics.Analytic[i]) ==
              parameters_of(run.Analytics.Analytic[i]));
    }

    // The export holds every convention the party sees, so it may hold more
    // than the document did, but never less. FX conventions are not stored yet.
    const auto conventions =
        domain::split_conventions(parse<domain::conventions>(original.at("conventions.xml")));
    const auto exported_conventions =
        domain::split_conventions(parse<domain::conventions>(exported.at("conventions.xml")));
    const auto holds_all = []<typename Row>(const domain::conventions_package& written,
                                            const domain::conventions_package& read,
                                            std::string_view family) {
        const auto read_ids = ids_of(rows_of<Row>(read, family));
        const auto rows = rows_of<Row>(written, family);
        return std::ranges::all_of(rows, [&](const auto& r) { return read_ids.contains(r.id); });
    };
    using namespace ores::refdata::domain;
    CHECK(holds_all.operator()<deposit_convention>(conventions, exported_conventions, "deposit"));
    CHECK(holds_all.operator()<swap_convention>(conventions, exported_conventions, "swap"));
    CHECK(holds_all.operator()<ois_convention>(conventions, exported_conventions, "ois"));
    CHECK(holds_all.operator()<fra_convention>(conventions, exported_conventions, "fra"));
    CHECK(holds_all.operator()<zero_convention>(conventions, exported_conventions, "zero"));
    CHECK(holds_all.operator()<swap_index_convention>(
        conventions, exported_conventions, "swap_index"));
    CHECK(holds_all.operator()<cds_convention>(conventions, exported_conventions, "cds"));

    std::set<std::string> slots;
    for (const auto& b : ores::reporting::repository::report_configuration_repository().read_latest(
             parties.a_context))
        if (b.report_definition_id == definition)
            slots.insert(b.configuration_type_code);
    CHECK(slots == std::set<std::string>{"conventions",
                                         "curve_configuration",
                                         "pricing_engines",
                                         "risk_report_config",
                                         "todays_market"});
}

TEST_CASE("a run import installs the conventions the sample names", tags) {
    ores::testing::scoped_database_helper h;
    const auto parties = ores::ore::tests::make_two_parties(h);
    store::import_run(parties.a_context, make_definition(h, parties), "Example_1", example_input());

    // The rows the import stores, read from the tables rather than through the
    // service, so the count survives the service being replaced.
    ores::refdata::repository::convention_repository conventions;
    const auto central = conventions.read_latest(parties.a_context);
    CHECK(central.size() == 1);
    REQUIRE_FALSE(central.empty());
    CHECK(central.front().kind_code == "TENOR_BASIS_SWAP");
    CHECK(central.front().party_id == parties.a);

    ores::refdata::repository::tenor_basis_swap_pay_receive_repository pay_receive;
    ores::refdata::repository::tenor_basis_swap_long_short_repository long_short;
    const auto pay = pay_receive.read_latest(parties.a_context);
    const auto receive = long_short.read_latest(parties.a_context);
    // The document names one leg form, so exactly one of the two tables holds a
    // row, and that row names the central row the identity resolved to.
    CHECK(pay.size() + receive.size() == 1);
    if (!pay.empty())
        CHECK(pay.front().convention_id == central.front().id);
    else {
        REQUIRE_FALSE(receive.empty());
        CHECK(receive.front().convention_id == central.front().id);
    }

    ores::refdata::repository::deposit_convention_repository deposits;
    ores::refdata::repository::swap_convention_repository swaps;
    CHECK_FALSE(deposits.read_latest(parties.a_context).empty());
    CHECK_FALSE(swaps.read_latest(parties.a_context).empty());
}

TEST_CASE("another party cannot export a run it does not own", tags) {
    ores::testing::scoped_database_helper h;
    const auto parties = ores::ore::tests::make_two_parties(h);
    const auto definition = make_definition(h, parties);
    store::import_run(parties.a_context, definition, "Example_1", example_input());
    CHECK_THROWS(store::export_run(parties.b_context, definition));
}

TEST_CASE("a party imports the same run twice", tags) {
    ores::testing::scoped_database_helper h;
    const auto parties = ores::ore::tests::make_two_parties(h);
    const auto files = example_input();
    store::import_run(parties.a_context, make_definition(h, parties), "first", files);
    const auto second =
        store::import_run(parties.a_context, make_definition(h, parties), "second", files);
    CHECK(second.stored.size() == 5);

    // The input names one tenor basis swap, and the second import must resolve
    // it to the convention the first wrote by its stated key rather than add a
    // second one under a fresh key. The part is resolved the same way, so the
    // convention keeps one part rather than gaining a second.
    const auto held = ores::refdata::service::conventions_bundle_service(parties.a_context).read();
    const auto rows_held = [&held](const std::string& family) {
        return std::ranges::count_if(held.rows,
                                     [&family](const auto& row) { return row.family == family; });
    };
    CHECK(rows_held("convention") == 1);
    CHECK(rows_held("tenor_basis_swap_pay_receive") + rows_held("tenor_basis_swap_long_short") ==
          1);
}

TEST_CASE("an import naming a file the input lacks is refused before it writes", tags) {
    ores::testing::scoped_database_helper h;
    const auto parties = ores::ore::tests::make_two_parties(h);
    const auto definition = make_definition(h, parties);
    auto files = example_input();
    files.erase("curveconfig.xml");
    CHECK_THROWS_WITH(
        store::import_run(parties.a_context, definition, "Example_1", files),
        "The run document names curveconfig.xml as its curve_configuration file, which the input "
        "does not hold.");
    const auto bindings =
        ores::reporting::repository::report_configuration_repository().read_latest(
            parties.a_context);
    CHECK(std::ranges::none_of(
        bindings, [&](const auto& b) { return b.report_definition_id == definition; }));
}

TEST_CASE("a second import into the same report definition is refused", tags) {
    ores::testing::scoped_database_helper h;
    const auto parties = ores::ore::tests::make_two_parties(h);
    const auto definition = make_definition(h, parties);
    const auto files = example_input();
    store::import_run(parties.a_context, definition, "first", files);
    CHECK_THROWS_WITH(store::import_run(parties.a_context, definition, "second", files),
                      "The report definition already binds a risk report configuration; import "
                      "into a new definition.");
}

TEST_CASE("an export from a session with no party reads the definition's party", tags) {
    ores::testing::scoped_database_helper h;
    const auto parties = ores::ore::tests::make_two_parties(h);
    const auto files = example_input();
    const auto definition = make_definition(h, parties);
    store::import_run(parties.a_context, definition, "Example_1", files);
    store::import_run(
        parties.b_context, make_definition(h, parties.b, parties.b_context), "Example_1", files);

    const auto tenant_only = h.context().with_tenant(h.tenant_id(), "");
    CHECK(store::export_run(tenant_only, definition) ==
          store::export_run(parties.a_context, definition));
}

TEST_CASE("the archive puts the run document in Input and the rest in the run's input path",
          "[ore][store]") {
    auto files = example_input();
    auto layout = store::archive_layout(files);
    CHECK(layout.contains("Input/ore.xml"));
    CHECK(layout.contains("Input/curveconfig.xml"));
    CHECK(layout.size() == files.size());

    auto& run = files.at("ore.xml");
    const std::string from = "<Parameter name=\"inputPath\">Input</Parameter>";
    REQUIRE(run.find(from) != std::string::npos);
    run.replace(
        run.find(from), from.size(), "<Parameter name=\"inputPath\">./Input/Dim</Parameter>");
    layout = store::archive_layout(files);
    CHECK(layout.contains("Input/ore.xml"));
    CHECK(layout.contains("Input/Dim/curveconfig.xml"));
}

TEST_CASE("the archive refuses a path that leaves the package", "[ore][store]") {
    const auto with_input_path = [](const std::string& input_path) {
        auto files = example_input();
        auto& run = files.at("ore.xml");
        const std::string from = "<Parameter name=\"inputPath\">Input</Parameter>";
        run.replace(run.find(from),
                    from.size(),
                    "<Parameter name=\"inputPath\">" + input_path + "</Parameter>");
        return files;
    };
    CHECK_THROWS_AS(store::archive_layout(with_input_path("../../etc")), std::invalid_argument);
    CHECK_THROWS_AS(store::archive_layout(with_input_path("/tmp/x")), std::invalid_argument);
    CHECK_THROWS_AS(store::archive_layout(with_input_path("Input/../..")), std::invalid_argument);

    auto files = example_input();
    files["../evil.xml"] = "x";
    CHECK_THROWS_AS(store::archive_layout(files), std::invalid_argument);

    CHECK(store::archive_layout(with_input_path("./Input//Dim/"))
              .contains("Input/Dim/curveconfig.xml"));
}

TEST_CASE("the run document names the data files the package places", "[ore][store]") {
    const auto places = store::declared_data_files(example_input());
    CHECK(places.market_data == "Input/market.txt");
    CHECK(places.fixings == "Input/fixings.txt");
    CHECK(places.portfolio == "Input/portfolio.xml");

    // The names move with the run document's input path, because the engine
    // reads them from there.
    auto files = example_input();
    auto& run = files.at("ore.xml");
    const std::string from = "<Parameter name=\"inputPath\">Input</Parameter>";
    REQUIRE(run.find(from) != std::string::npos);
    run.replace(
        run.find(from), from.size(), "<Parameter name=\"inputPath\">./Input/Dim</Parameter>");
    CHECK(store::declared_data_files(files).portfolio == "Input/Dim/portfolio.xml");

    // A run document naming no portfolio leaves the slot empty, so the caller
    // sees the gap rather than a name of our own choosing.
    auto without = example_input();
    auto& doc = without.at("ore.xml");
    const std::string line = "<Parameter name=\"portfolioFile\">portfolio.xml</Parameter>";
    REQUIRE(doc.find(line) != std::string::npos);
    doc.erase(doc.find(line), line.size());
    CHECK(store::declared_data_files(without).portfolio.empty());
}

TEST_CASE("the run document refuses a data file that leaves the package", "[ore][store]") {
    auto files = example_input();
    auto& run = files.at("ore.xml");
    const std::string line = "<Parameter name=\"portfolioFile\">portfolio.xml</Parameter>";
    REQUIRE(run.find(line) != std::string::npos);
    run.replace(run.find(line),
                line.size(),
                "<Parameter name=\"portfolioFile\">../portfolio.xml</Parameter>");
    CHECK_THROWS_AS(store::declared_data_files(files), std::invalid_argument);
}

TEST_CASE("an import whose document does not parse is refused before it writes", tags) {
    ores::testing::scoped_database_helper h;
    const auto parties = ores::ore::tests::make_two_parties(h);
    const auto definition = make_definition(h, parties);
    auto files = example_input();
    files["curveconfig.xml"] = "<CurveConfiguration><not closed";
    CHECK_THROWS(store::import_run(parties.a_context, definition, "Example_1", files));
    const auto bindings =
        ores::reporting::repository::report_configuration_repository().read_latest(
            parties.a_context);
    CHECK(std::ranges::none_of(
        bindings, [&](const auto& b) { return b.report_definition_id == definition; }));
}
