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
#include "ores.ore.core/store/run_store.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/xml/roundtrip_harness.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.reporting.api/generators/report_definition_generator.hpp"
#include "ores.reporting.core/repository/report_configuration_repository.hpp"
#include "ores.reporting.core/repository/report_definition_repository.hpp"
#include "ores.reporting.core/repository/report_run_setup_repository.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "party_fixture.hpp"
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <filesystem>
#include <map>
#include <set>
#include <string>

namespace {

const std::string tags("[ore][store][database]");

using namespace ores::ore;
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
    CHECK(difference<domain::todaysmarket>(original, exported, "todaysmarket.xml").empty());
    CHECK(difference<domain::curveconfiguration>(original, exported, "curveconfig.xml").empty());

    const auto run = parse<domain::ore>(original.at("ore.xml"));
    const auto exported_run = parse<domain::ore>(exported.at("ore.xml"));
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
        domain::conventions_mapper::map(parse<domain::conventions>(original.at("conventions.xml")));
    const auto exported_conventions =
        domain::conventions_mapper::map(parse<domain::conventions>(exported.at("conventions.xml")));
    const auto holds_all = [](const auto& written, const auto& read) {
        const auto read_ids = ids_of(read);
        return std::ranges::all_of(written, [&](const auto& r) { return read_ids.contains(r.id); });
    };
    CHECK(holds_all(conventions.deposit, exported_conventions.deposit));
    CHECK(holds_all(conventions.swap, exported_conventions.swap));
    CHECK(holds_all(conventions.ois, exported_conventions.ois));
    CHECK(holds_all(conventions.fra, exported_conventions.fra));
    CHECK(holds_all(conventions.zero, exported_conventions.zero));
    CHECK(holds_all(conventions.swap_index, exported_conventions.swap_index));
    CHECK(holds_all(conventions.cds, exported_conventions.cds));

    std::set<std::string> slots;
    for (const auto& b : ores::reporting::repository::report_configuration_repository().read_latest(
             parties.a_context))
        if (b.report_definition_id == definition)
            slots.insert(b.configuration_type_code);
    CHECK(slots == std::set<std::string>{
                       "conventions", "curve_configuration", "pricing_engines", "todays_market"});
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
    const auto setups =
        ores::reporting::repository::report_run_setup_repository().read_latest(parties.a_context);
    CHECK(std::ranges::none_of(
        setups, [&](const auto& s) { return s.report_definition_id == definition; }));
}

TEST_CASE("a second import into the same report definition is refused", tags) {
    ores::testing::scoped_database_helper h;
    const auto parties = ores::ore::tests::make_two_parties(h);
    const auto definition = make_definition(h, parties);
    const auto files = example_input();
    store::import_run(parties.a_context, definition, "first", files);
    CHECK_THROWS_WITH(
        store::import_run(parties.a_context, definition, "second", files),
        "The report definition already holds a run document; import into a new definition.");
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
