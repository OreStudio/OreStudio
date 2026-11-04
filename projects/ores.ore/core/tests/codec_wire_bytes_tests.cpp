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
#include "ores.ore.api/messaging/currency_config_protocol.hpp"
#include "ores.ore.api/messaging/ore_document_protocol.hpp"
#include "ores.ore.api/messaging/ore_import_protocol.hpp"
#include "ores.ore.api/messaging/pricing_engines_protocol.hpp"
#include "ores.ore.api/messaging/run_configuration_protocol.hpp"
#include "ores.ore.core/planner/import_choices.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/codec/wire_traits.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <span>
#include <string>

namespace {

using namespace ores::ore::messaging;
using ores::ore::planner::import_choices;

const std::string tags("[codec]");

template <typename T>
std::string their_msgpack(const T& v) {
    const auto bytes = rfl::msgpack::write(v);
    return std::string(bytes.begin(), bytes.end());
}

std::span<const std::byte> bytes_of(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

/**
 * @brief The generated codec writes the bytes reflect-cpp wrote, in both
 * formats, and reads them back to the same bytes.
 */
template <typename T>
void same_bytes_as_reflect_cpp(const T& v) {
    using codec = ores::utility::codec::wire_traits<T>;
    CHECK(codec::is_generated);

    const auto json = codec::write_json(v);
    CHECK(json == rfl::json::write(v));
    CHECK(codec::write_json(codec::read_json(json)) == json);

    const auto msgpack = codec::write_msgpack(v);
    CHECK(msgpack == their_msgpack(v));
    CHECK(codec::write_msgpack(codec::read_msgpack(bytes_of(msgpack))) == msgpack);
}

template <typename... T>
void every_default_has_the_same_bytes() {
    (same_bytes_as_reflect_cpp(T{}), ...);
}

boost::uuids::uuid an_id(char last) {
    std::string text = "3f2504e0-4f89-11d3-9a0c-0305e82c330";
    text += last;
    return boost::uuids::string_generator{}(text);
}

import_choices some_choices() {
    import_choices c;
    c.currency_mode = ores::ore::planner::currency_import_mode::all;
    c.portfolio_reimport_mode = ores::ore::planner::reimport_mode::create_new_versions;
    c.parent_portfolio_name = "Imports";
    c.create_parent_portfolio = false;
    c.scan_exclusions = {"Output", "Logs"};
    c.hierarchy_strip = {"Input", "Examples"};
    c.defaults.trade_date = "2026-11-02";
    c.defaults.activity_type_code = "NEW";
    c.defaults.default_counterparty_id = an_id('1');
    c.existing_target_book_id = an_id('2');
    c.sandbox_id = an_id('3');
    c.official = false;
    c.party_id = an_id('4');
    c.aggregation_ccy = "EUR";
    c.functional_currency = "GBP";
    c.purpose_type = "External";
    return c;
}

}

TEST_CASE("every ore message's default value has reflect-cpp's bytes", tags) {
    every_default_has_the_same_bytes<export_currency_config_request,
                                     export_currency_config_response,
                                     currency_import_row,
                                     currency_import_issue,
                                     preview_currency_config_import_request,
                                     preview_currency_config_import_response,
                                     currency_import_choice,
                                     import_currency_config_request,
                                     currency_import_error,
                                     import_currency_config_response,
                                     ore_document,
                                     ore_import_item_error,
                                     ore_import_request,
                                     ore_import_response,
                                     ore_import_execute_request,
                                     ore_import_execute_result,
                                     ore_import_rollback_request,
                                     pricing_engines_export_problem,
                                     export_pricing_engines_request,
                                     export_pricing_engines_response,
                                     pricing_engines_import_product,
                                     pricing_engines_import_row,
                                     pricing_engines_import_issue,
                                     preview_pricing_engines_import_request,
                                     preview_pricing_engines_import_response,
                                     import_pricing_engines_request,
                                     pricing_engines_import_error,
                                     import_pricing_engines_response,
                                     run_input_file,
                                     saved_document,
                                     import_run_configuration_request,
                                     import_run_configuration_response,
                                     run_configuration_import_execute_request,
                                     run_configuration_import_execute_result,
                                     run_configuration_import_rollback_request,
                                     export_run_configuration_request,
                                     export_run_configuration_response>();
}

TEST_CASE("the ore import messages have reflect-cpp's bytes", tags) {
    const ore_import_item_error error{"a.xml", "T1", "no leg"};
    same_bytes_as_reflect_cpp(ore_import_response{true, "ok", {error}, "c1", "w1"});
    same_bytes_as_reflect_cpp(ore_import_execute_request{"r1", "{}", "c1", "t1"});
    same_bytes_as_reflect_cpp(ore_import_execute_result{
        false, "partial", "c1", {error}, {"EUR"}, {"P1"}, {"B1", "B2"}, {"T1"}});
    same_bytes_as_reflect_cpp(
        ore_import_rollback_request{"c1", "t1", {"EUR"}, {"P1"}, {"B1"}, {"T1", "T2"}});
}

TEST_CASE("the run configuration messages have reflect-cpp's bytes", tags) {
    const run_input_file file{"ore.xml", "<ORE/>"};
    const saved_document saved{"conventions", "3f2504e0-4f89-11d3-9a0c-0305e82c3301"};
    same_bytes_as_reflect_cpp(
        run_configuration_import_execute_request{"d1", "Run", {file, file}, "c1", "t1"});
    same_bytes_as_reflect_cpp(run_configuration_import_execute_result{
        true, "ok", {"a"}, {"b"}, {"c"}, {"d"}, {"e"}, "d1", true, {saved}});
    same_bytes_as_reflect_cpp(
        run_configuration_import_rollback_request{"c1", "t1", "d1", true, {saved, saved}});
    same_bytes_as_reflect_cpp(export_run_configuration_response{true, "ok", {file}});
}

TEST_CASE("the currency and pricing engine messages have reflect-cpp's bytes", tags) {
    same_bytes_as_reflect_cpp(preview_currency_config_import_response{true,
                                                                      "ok",
                                                                      "CurrencyConfig",
                                                                      3,
                                                                      2,
                                                                      1,
                                                                      1,
                                                                      0,
                                                                      {{"EUR", "Euro", "held", ""}},
                                                                      {{"k", "s", "d"}}});
    same_bytes_as_reflect_cpp(
        import_currency_config_request{"<x/>", {{"EUR", "keep"}}, "rc", "why"});
    same_bytes_as_reflect_cpp(
        import_currency_config_response{true, "ok", 1, 2, 3, 4, {{"USD", "bad"}}});
    same_bytes_as_reflect_cpp(
        export_currency_config_response{true, "ok", 1, {{"currencies.xml", "<x/>"}}});
    same_bytes_as_reflect_cpp(
        preview_pricing_engines_import_response{true,
                                                "ok",
                                                "PricingEngines",
                                                2,
                                                5,
                                                {{"Swap", "DK", "DE", 2}},
                                                {"g1"},
                                                "target",
                                                1,
                                                1,
                                                0,
                                                0,
                                                {{"k", "e", "c", "o", "n", "w"}},
                                                {{"k", "s", "d"}}});
    same_bytes_as_reflect_cpp(
        import_pricing_engines_request{"<x/>", "id", "name", "variant", {"r"}, "rc", "why"});
    same_bytes_as_reflect_cpp(
        import_pricing_engines_response{true, "ok", "id", "name", 1, 2, 3, 4, {{"k", "bad"}}});
    same_bytes_as_reflect_cpp(export_pricing_engines_response{
        true, "ok", 1, {{"pricingengine.xml", "<x/>"}}, {{"code", "msg"}}});
}

TEST_CASE("the import choices have reflect-cpp's bytes", tags) {
    namespace object = ores::utility::codec::object;
    for (const auto& choices : {import_choices{}, some_choices()}) {
        const auto json = object::write_json(choices);
        CHECK(json == rfl::json::write(choices));
        CHECK(object::write_json(object::read_json<import_choices>(json)) == json);

        const auto msgpack = object::write_msgpack(choices);
        CHECK(msgpack == their_msgpack(choices));
        CHECK(object::write_msgpack(object::read_msgpack<import_choices>(bytes_of(msgpack))) ==
              msgpack);
    }
}

TEST_CASE("the import choices refuse a mode they do not know", tags) {
    auto json = ores::utility::codec::object::write_json(some_choices());
    const std::string known = R"("currency_mode":"all")";
    const auto at = json.find(known);
    REQUIRE(at != std::string::npos);
    json.replace(at, known.size(), R"("currency_mode":"some")");
    CHECK_THROWS_WITH(ores::utility::codec::object::read_json<import_choices>(json),
                      Catch::Matchers::ContainsSubstring("a currency import mode"));
    CHECK_FALSE(rfl::json::read<import_choices>(json));
}
