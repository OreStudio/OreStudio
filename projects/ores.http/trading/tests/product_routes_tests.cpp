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
#include "ores.http/routes/trading/product_routes.hpp"
#include "ores.trading.api/messaging/product_schema_operation_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::routes::trading::product_routes;
namespace schema = ores::trading::messaging;

TEST_CASE("the product list names each product by code, name and construction",
          "[product_routes]") {
    schema::list_products_response resp;
    resp.success = true;
    resp.products = {{.code = "Swap", .name = "Swap", .construction = "native"},
                     {.code = "Corridor", .name = "Corridor", .construction = "script"}};

    CHECK(product_routes::list_json(resp) ==
          R"({"rows":[{"code":"Swap","name":"Swap","construction":"native"},)"
          R"({"code":"Corridor","name":"Corridor","construction":"script"}]})");
}

TEST_CASE("a description names its fields, inputs, codes and forms in the BFF's shape",
          "[product_routes]") {
    schema::describe_product_response resp;
    resp.success = true;
    resp.product = {.code = "Swap", .name = "Swap", .construction = "native"};
    resp.element = "SwapData";
    resp.fields = {{.type_name = "trade",
                    .name = "SwapData",
                    .position = 3,
                    .label = "Swap data",
                    .help = "h",
                    .description = "d",
                    .value_domain = "record:swapData",
                    .is_required = true,
                    .default_text = "None.",
                    .format = "A swap data record.",
                    .xpath = "/Trade[TradeType='Swap']/SwapData"}};
    ores::trading::domain::script_input input;
    input.script_name = "SwapScript";
    input.input_name = "PutCall";
    input.data_type = "Number";
    resp.inputs = {input};
    ores::trading::domain::script_input_code code;
    code.script_name = "SwapScript";
    code.input_name = "PutCall";
    code.code = -1;
    code.label = "put";
    resp.codes = {code};
    resp.forms = {{.kind = "native"},
                  {.kind = "scripted", .script_name = "SwapScript", .product_tag = "Tag"}};

    const auto text = product_routes::describe_json(resp);

    CHECK(text.starts_with(
        R"({"product":{"code":"Swap","name":"Swap","construction":"native"},"element":"SwapData",)"));
    CHECK(text.find(R"("value_domain":"record:swapData","is_required":true)") != std::string::npos);
    CHECK(text.find(R"("inputs":[{"script_name":"SwapScript","name":"PutCall","data_type":)"
                    R"("Number","is_array":false,"value_rule":"","description":""}])") !=
          std::string::npos);
    CHECK(text.find(R"("codes":[{"script_name":"SwapScript","input_name":"PutCall","code":-1,)"
                    R"("label":"put"}])") != std::string::npos);
    CHECK(
        text.ends_with(R"("forms":[{"kind":"native","script_name":"","product_tag":""},)"
                       R"({"kind":"scripted","script_name":"SwapScript","product_tag":"Tag"}]})"));
}
