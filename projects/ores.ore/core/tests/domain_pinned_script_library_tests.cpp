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
#include "ores.ore.core/domain/pinned_script_library.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <map>
#include <optional>
#include <pugixml.hpp>
#include <string>
#include <vector>

namespace {

using ores::ore::domain::pin_script_library;
using ores::trading::domain::instrument_batch;
using ores::trading::domain::scripted_instrument;
using ores::trading::messaging::export_script_library_response;

const auto world = ores::utility::uuid::tenant_id::system().to_uuid();
const auto acme = boost::uuids::string_generator()("00000000-0000-0000-0000-0000000ac3e1");

/// A trade of the library script @p name, at @p version held by @p tenant.
scripted_instrument library_trade(const std::string& trade,
                                  const std::string& name,
                                  std::optional<int> version,
                                  const boost::uuids::uuid& tenant = world,
                                  std::optional<std::string> element = std::nullopt) {
    scripted_instrument r;
    r.identity.trade_id = boost::uuids::string_generator()(trade);
    r.script_name = name;
    r.data_element = element;
    if (version) {
        r.script_tenant_id = tenant;
        r.script_version = version;
    }
    return r;
}

const std::string t1 = "00000000-0000-0000-0000-000000000001";
const std::string t2 = "00000000-0000-0000-0000-000000000002";
const std::string t3 = "00000000-0000-0000-0000-000000000003";

std::map<std::string, std::string> names_of(const instrument_batch& batch) {
    std::map<std::string, std::string> out;
    for (const auto& r : batch.scripted_instruments)
        out[boost::uuids::to_string(r.identity.trade_id)] = r.script_name;
    return out;
}

/// The rows trading holds for one version: the script and its default variant.
void add_version(export_script_library_response& rows,
                 const boost::uuids::uuid& tenant,
                 const std::string& name,
                 int version,
                 const std::string& code) {
    const auto holder = *ores::utility::uuid::tenant_id::from_uuid(tenant);
    rows.scripts.push_back({.version = version, .tenant_id = holder, .name = name});
    rows.variants.push_back({.tenant_id = holder,
                             .script_name = name,
                             .purpose = "default",
                             .code = code,
                             .npv_variable = "Option",
                             .script_version = version});
}

/// The code of each script the written library holds, by the name it holds it under.
std::map<std::string, std::string> codes_of(const std::string& written) {
    pugi::xml_document document;
    REQUIRE(document.load_string(written.c_str()));
    std::map<std::string, std::string> out;
    for (const auto script : document.child("ScriptLibrary").children("Script"))
        out[script.child_value("Name")] = script.child("Script").child("Code").text().as_string();
    return out;
}

}

TEST_CASE("one version of a script is written under its own name", "[ore][script_library][pin]") {
    instrument_batch batch;
    batch.scripted_instruments.push_back(library_trade(t1, "Accumulator01", 3));
    batch.scripted_instruments.push_back(library_trade(t2, "Accumulator01", 3));

    const auto plan = pin_script_library(batch);

    CHECK(plan.refusals.empty());
    REQUIRE(plan.entries.size() == 1u);
    CHECK(plan.entries[0].library_name == "Accumulator01");
    CHECK(plan.entries[0].pin.version == 3);
    CHECK(plan.entries[0].pin.tenant_id == world);
    CHECK(names_of(batch)[t1] == "Accumulator01");
}

TEST_CASE("two versions of a script are each written under a name of their own",
          "[ore][script_library][pin]") {
    instrument_batch batch;
    batch.scripted_instruments.push_back(library_trade(t1, "Accumulator01", 3));
    batch.scripted_instruments.push_back(library_trade(t2, "Accumulator01", 4));
    batch.scripted_instruments.push_back(library_trade(t3, "Accumulator01", 1, acme));

    const auto plan = pin_script_library(batch);
    CHECK(plan.refusals.empty());
    REQUIRE(plan.entries.size() == 3u);

    const auto names = names_of(batch);
    CHECK(names.at(t1) != names.at(t2));
    CHECK(names.at(t1) != names.at(t3));
    CHECK(names.at(t1).starts_with("Accumulator01."));

    export_script_library_response rows;
    add_version(rows, world, "Accumulator01", 3, "Option = 3;");
    add_version(rows, world, "Accumulator01", 4, "Option = 4;");
    add_version(rows, acme, "Accumulator01", 1, "Option = 1;");
    add_version(rows, world, "Accumulator01", 2, "Option = 2;");

    const auto codes = codes_of(ores::ore::domain::write_pinned_script_library(plan, rows));
    CHECK(codes.size() == 3u);
    CHECK(codes.at(names.at(t1)) == "Option = 3;");
    CHECK(codes.at(names.at(t2)) == "Option = 4;");
    CHECK(codes.at(names.at(t3)) == "Option = 1;");
}

TEST_CASE("the version a product element names keeps the script's own name",
          "[ore][script_library][pin]") {
    instrument_batch batch;
    batch.scripted_instruments.push_back(
        library_trade(t1, "Accumulator01", 3, world, "Accumulator01Data"));
    batch.scripted_instruments.push_back(library_trade(t2, "Accumulator01", 4));

    const auto plan = pin_script_library(batch);

    CHECK(plan.refusals.empty());
    const auto names = names_of(batch);
    CHECK(names.at(t1) == "Accumulator01");
    CHECK(names.at(t2) != "Accumulator01");
    CHECK(names.at(t2).starts_with("Accumulator01."));
}

TEST_CASE("product elements that name two versions of one script are refused",
          "[ore][script_library][pin]") {
    instrument_batch batch;
    batch.scripted_instruments.push_back(
        library_trade(t1, "Accumulator01", 3, world, "Accumulator01Data"));
    batch.scripted_instruments.push_back(
        library_trade(t2, "Accumulator01", 4, world, "Accumulator01Data"));

    const auto plan = pin_script_library(batch);

    REQUIRE(plan.refusals.size() == 1u);
    CHECK_THAT(plan.refusals[0], Catch::Matchers::ContainsSubstring(t1));
    CHECK_THAT(plan.refusals[0], Catch::Matchers::ContainsSubstring(t2));
    CHECK_THAT(plan.refusals[0], Catch::Matchers::ContainsSubstring("Accumulator01Data"));
}

TEST_CASE("a script no library held when the trade was booked is refused",
          "[ore][script_library][pin]") {
    instrument_batch batch;
    batch.scripted_instruments.push_back(library_trade(t1, "Autocallable", std::nullopt));

    const auto plan = pin_script_library(batch);

    CHECK(plan.entries.empty());
    REQUIRE(plan.refusals.size() == 1u);
    CHECK_THAT(plan.refusals[0], Catch::Matchers::ContainsSubstring("Autocallable"));
    CHECK_THAT(plan.refusals[0], Catch::Matchers::ContainsSubstring(t1));
}

TEST_CASE("an inline script needs no library and states no script name",
          "[ore][script_library][pin]") {
    instrument_batch batch;
    auto inline_trade = library_trade(t1, "MyPayoff", std::nullopt);
    inline_trade.script_body = "Option = PAY(1, Expiry, Expiry, PayCcy);";
    batch.scripted_instruments.push_back(inline_trade);

    const auto plan = pin_script_library(batch);

    CHECK(plan.entries.empty());
    CHECK(plan.refusals.empty());
    CHECK(names_of(batch).at(t1).empty());
}

TEST_CASE("one version number held by two tenants is two scripts", "[ore][script_library][pin]") {
    instrument_batch batch;
    batch.scripted_instruments.push_back(library_trade(t1, "Accumulator01", 1));
    batch.scripted_instruments.push_back(library_trade(t2, "Accumulator01", 1, acme));

    const auto plan = pin_script_library(batch);

    CHECK(plan.refusals.empty());
    CHECK(plan.entries.size() == 2u);
    const auto names = names_of(batch);
    CHECK(names.at(t1) != names.at(t2));
}

TEST_CASE("a version is refused when its qualified name is another script's name",
          "[ore][script_library][pin]") {
    const auto taken = "Accumulator01." + boost::uuids::to_string(world) + ".v3";
    instrument_batch batch;
    batch.scripted_instruments.push_back(library_trade(t1, "Accumulator01", 3));
    batch.scripted_instruments.push_back(library_trade(t2, "Accumulator01", 4));
    batch.scripted_instruments.push_back(library_trade(t3, taken, 1, acme));

    const auto plan = pin_script_library(batch);

    REQUIRE(plan.refusals.size() == 1u);
    CHECK_THAT(plan.refusals[0], Catch::Matchers::ContainsSubstring(taken));
}
