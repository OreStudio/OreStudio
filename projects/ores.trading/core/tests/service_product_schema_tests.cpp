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
#include "ores.ore.core/repository/ore_parameter_repository.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.trading.core/repository/product_type_repository.hpp"
#include "ores.trading.core/repository/script_input_code_repository.hpp"
#include "ores.trading.core/repository/script_input_repository.hpp"
#include "ores.trading.core/repository/script_repository.hpp"
#include "ores.trading.core/repository/script_twin_input_repository.hpp"
#include "ores.trading.core/service/product_schema_service.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <catch2/catch_test_macros.hpp>
#include <algorithm>
#include <string>
#include <vector>

using namespace ores::trading;
using ores::utility::uuid::tenant_id;

namespace {

ores::ore::domain::ore_parameter field(std::string type, std::string name, int position,
                                       std::string domain, std::string xpath = {}) {
    ores::ore::domain::ore_parameter p;
    p.scope = "instrument";
    p.subtype = std::move(type);
    p.name = std::move(name);
    p.position = position;
    p.ore_parameter_value_domain_code = std::move(domain);
    p.xpath = xpath.empty() ? "/" + p.subtype + "/" + p.name : std::move(xpath);
    return p;
}

/// A swap whose legs repeat, a leg that nests itself as a cycle, and a type no
/// product reaches.
service::instrument_fields swap_fields() {
    service::instrument_fields f;
    f["trade"] = {field("trade", "FxForwardData", 0, "record:fxForwardData",
                        "/Trade[TradeType='FxForward']/FxForwardData"),
                  field("trade", "SwapData", 1, "record:swapData",
                        "/Trade[TradeType='Swap']/SwapData")};
    f["swapData"] = {field("swapData", "Settlement", 1, "string"),
                     field("swapData", "LegData", 0, "list:legData")};
    f["legData"] = {field("legData", "Payer", 0, "boolean"),
                    field("legData", "Nested", 1, "record:legData"),
                    field("legData", "Rates", 2, "list:double")};
    f["fxForwardData"] = {field("fxForwardData", "ValueDate", 0, "date")};
    return f;
}

domain::product_type product(std::string code, int order, std::string construction = "native") {
    domain::product_type p;
    p.code = code;
    p.name = code + " name";
    p.construction = std::move(construction);
    p.display_order = order;
    return p;
}

domain::script script(std::string name, tenant_id tenant, int version, std::string tag) {
    domain::script s;
    s.name = std::move(name);
    s.tenant_id = tenant;
    s.version = version;
    s.product_tag = std::move(tag);
    return s;
}

domain::script_input input(std::string script, tenant_id tenant, int version, int seq,
                           std::string name) {
    domain::script_input i;
    i.script_name = std::move(script);
    i.tenant_id = tenant;
    i.script_version = version;
    i.sequence_number = seq;
    i.input_name = std::move(name);
    return i;
}

domain::script_input_code code(std::string script, tenant_id tenant, int version, int seq,
                               std::string input, int value, std::string label) {
    domain::script_input_code c;
    c.script_name = std::move(script);
    c.tenant_id = tenant;
    c.script_version = version;
    c.sequence_number = seq;
    c.input_name = std::move(input);
    c.code = value;
    c.label = std::move(label);
    return c;
}

domain::script_twin_input twin(std::string trade_type, std::string script) {
    domain::script_twin_input t;
    t.trade_type_code = std::move(trade_type);
    t.script_name = std::move(script);
    return t;
}

std::vector<std::string> names_of(const std::vector<messaging::product_field>& fields) {
    std::vector<std::string> out;
    for (const auto& f : fields)
        out.push_back(f.type_name + "/" + f.name);
    return out;
}

}

TEST_CASE("a native product is described from its element down, each type once in schema order",
          "[product_schema]") {
    service::product_catalogue catalogue;
    catalogue.product_types = {product("Swap", 1)};

    const auto out = service::describe_product("Swap", catalogue, swap_fields());

    REQUIRE(out.success);
    CHECK(out.element == "SwapData");
    CHECK(names_of(out.fields) == std::vector<std::string>{"trade/SwapData", "swapData/LegData",
                                                           "swapData/Settlement", "legData/Payer",
                                                           "legData/Nested", "legData/Rates"});
    REQUIRE(out.forms.size() == 1);
    CHECK(out.forms[0].kind == "native");
    CHECK(out.inputs.empty());
}

TEST_CASE("a twinned product has a scripted form with its script's inputs and codes",
          "[product_schema]") {
    const auto system = tenant_id::system();
    service::product_catalogue catalogue;
    catalogue.product_types = {product("Swap", 1)};
    catalogue.scripts = {script("SwapScript", system, 2, "SingleAssetOption")};
    catalogue.twins = {twin("Swap", "SwapScript"), twin("Swap", "SwapScript")};
    catalogue.inputs = {input("SwapScript", system, 2, 2, "PutCall"),
                        input("SwapScript", system, 2, 1, "Notional"),
                        input("SwapScript", system, 1, 1, "OldInput")};
    catalogue.codes = {code("SwapScript", system, 2, 2, "PutCall", -1, "put"),
                       code("SwapScript", system, 2, 1, "PutCall", 1, "call")};

    const auto out = service::describe_product("Swap", catalogue, swap_fields());

    REQUIRE(out.success);
    REQUIRE(out.forms.size() == 2);
    CHECK(out.forms[1].kind == "scripted");
    CHECK(out.forms[1].script_name == "SwapScript");
    CHECK(out.forms[1].product_tag == "SingleAssetOption");
    REQUIRE(out.inputs.size() == 2);
    CHECK(out.inputs[0].input_name == "Notional");
    REQUIRE(out.codes.size() == 2);
    CHECK(out.codes[0].label == "call");
}

TEST_CASE("a script no product type names is a script-only product, the tenant's own first",
          "[product_schema]") {
    const auto system = tenant_id::system();
    const auto own = tenant_id::from_string("00000000-0000-0000-0000-0000000000a1").value();
    service::product_catalogue catalogue;
    catalogue.scripts = {script("Corridor", own, 3, "OwnTag"),
                         script("Corridor", system, 7, "LibraryTag")};
    catalogue.inputs = {input("Corridor", system, 7, 1, "LibraryInput"),
                        input("Corridor", own, 3, 1, "OwnInput")};

    const auto out = service::describe_product("Corridor", catalogue, swap_fields());

    REQUIRE(out.success);
    CHECK(out.product.construction == "script");
    CHECK(out.element.empty());
    CHECK(out.fields.empty());
    REQUIRE(out.forms.size() == 1);
    CHECK(out.forms[0].product_tag == "OwnTag");
    REQUIRE(out.inputs.size() == 1);
    CHECK(out.inputs[0].input_name == "OwnInput");
}

TEST_CASE("a product with nothing to describe is refused, naming it", "[product_schema]") {
    service::product_catalogue catalogue;
    auto cdo = product("Cdo", 1);
    cdo.instrument_table = "credit_instrument";
    catalogue.product_types = {cdo};

    const auto unknown = service::describe_product("Nothing", catalogue, swap_fields());
    CHECK_FALSE(unknown.success);
    CHECK(unknown.message.find("Nothing") != std::string::npos);

    const auto empty = service::describe_product("Cdo", catalogue, swap_fields());
    CHECK_FALSE(empty.success);
    CHECK(empty.message.find("Cdo") != std::string::npos);
    CHECK(empty.message.find("no element in ORE's schema") != std::string::npos);
    CHECK(empty.forms.empty());
}

TEST_CASE("a product whose fields name a type the cache holds nothing for is refused, naming it",
          "[product_schema]") {
    service::product_catalogue catalogue;
    catalogue.product_types = {product("Swap", 1)};
    auto fields = swap_fields();
    fields.erase("legData");

    const auto out = service::describe_product("Swap", catalogue, fields);

    CHECK_FALSE(out.success);
    CHECK(out.fields.empty());
    CHECK(out.message.find("legData") != std::string::npos);
}

TEST_CASE("a product type ORE cannot build is refused with that reason", "[product_schema]") {
    service::product_catalogue catalogue;
    auto unbuildable = product("CommodityPairwiseVarianceSwap", 1);
    unbuildable.instrument_table = "";
    catalogue.product_types = {unbuildable};

    const auto out = service::describe_product("CommodityPairwiseVarianceSwap", catalogue,
                                               swap_fields());

    CHECK_FALSE(out.success);
    CHECK(out.message ==
          "ORE has no trade builder for the product type CommodityPairwiseVarianceSwap: the "
          "catalogue routes it to no instrument table, so it is booked only as a ScriptedTrade "
          "over a library script.");
}

TEST_CASE("the products are the product types in display order, then the scripts no type twins",
          "[product_schema]") {
    const auto system = tenant_id::system();
    service::product_catalogue catalogue;
    catalogue.product_types = {product("Swap", 2), product("FxForward", 1)};
    catalogue.scripts = {script("Twin", system, 1, ""), script("Alone", system, 1, ""),
                         script("Swap", system, 1, "")};
    catalogue.twins = {twin("Swap", "Twin")};

    const auto out = service::list_products(catalogue);

    REQUIRE(out.success);
    std::vector<std::string> codes;
    for (const auto& p : out.products)
        codes.push_back(p.code + ":" + p.construction);
    CHECK(codes == std::vector<std::string>{"FxForward:native", "Swap:native", "Alone:script"});
}

TEST_CASE("every product the seeded catalogue lists is described, but one ORE cannot build",
          "[product_schema][database]") {
    ores::testing::scoped_database_helper h;
    const auto system_ctx = h.context().with_tenant(tenant_id::system(), h.db_user());

    service::product_catalogue catalogue{
        .product_types = repository::product_type_repository().read_latest(system_ctx),
        .scripts = repository::script_repository().read_latest(system_ctx),
        .twins = repository::script_twin_input_repository().read_latest(system_ctx),
        .inputs = repository::script_input_repository().read_latest(system_ctx),
        .codes = repository::script_input_code_repository().read_latest(system_ctx)};
    service::instrument_fields fields;
    for (auto& p : ores::ore::repository::ore_parameter_repository().read_latest(system_ctx))
        if (p.scope == "instrument")
            fields[p.subtype].push_back(std::move(p));

    const auto products = service::list_products(catalogue).products;
    REQUIRE(products.size() > catalogue.product_types.size());
    std::vector<std::string> refused;
    for (const auto& p : products) {
        const auto out = service::describe_product(p.code, catalogue, fields);
        const auto type = std::ranges::find(catalogue.product_types, p.code,
                                            &domain::product_type::code);
        const bool unbuildable = type != catalogue.product_types.end() &&
                                 type->instrument_table.empty() &&
                                 out.message.starts_with("ORE has no trade builder");
        if (!out.success && !unbuildable)
            refused.push_back(p.code + ": " + out.message);
    }
    INFO("refused: " << refused.size());
    for (const auto& r : refused)
        UNSCOPED_INFO(r);
    CHECK(refused.empty());
}
