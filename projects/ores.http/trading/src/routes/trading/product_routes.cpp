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
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <string>
#include <vector>

namespace ores::http::routes::trading {

namespace {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace schema = ores::trading::messaging;

std::string summary_json(const schema::product_summary& p) {
    return "{\"code\":" + json::write(p.code) + ",\"name\":" + json::write(p.name) +
           ",\"construction\":" + json::write(p.construction) + "}";
}

std::string code_of(const http_request& req) {
    const auto code = rr::trimmed(req.get_path_param("code"));
    if (code.empty() || rr::characters_of(code) > 128)
        rr::invalid("A product is addressed by its code, of 1 to 128 characters.");
    return code;
}

}

std::string product_routes::list_json(const schema::list_products_response& response) {
    std::vector<std::string> rows;
    for (const auto& p : response.products)
        rows.push_back(summary_json(p));
    return "{\"rows\":" + rr::array_of(rows) + "}";
}

std::string product_routes::describe_json(const schema::describe_product_response& response) {
    std::vector<std::string> fields;
    for (const auto& f : response.fields)
        fields.push_back(
            "{\"type_name\":" + json::write(f.type_name) + ",\"name\":" + json::write(f.name) +
            ",\"position\":" + json::write(f.position) + ",\"label\":" + json::write(f.label) +
            ",\"help\":" + json::write(f.help) + ",\"description\":" +
            json::write(f.description) + ",\"value_domain\":" + json::write(f.value_domain) +
            ",\"is_required\":" + json::write(f.is_required) + ",\"default_text\":" +
            json::write(f.default_text) + ",\"format\":" + json::write(f.format) +
            ",\"xpath\":" + json::write(f.xpath) + "}");
    std::vector<std::string> inputs;
    for (const auto& i : response.inputs)
        inputs.push_back("{\"script_name\":" + json::write(i.script_name) +
                         ",\"name\":" + json::write(i.input_name) +
                         ",\"data_type\":" + json::write(i.data_type) +
                         ",\"is_array\":" + json::write(i.is_array) +
                         ",\"value_rule\":" + json::write(i.value_rule.value_or("")) +
                         ",\"description\":" + json::write(i.description.value_or("")) + "}");
    std::vector<std::string> codes;
    for (const auto& c : response.codes)
        codes.push_back("{\"script_name\":" + json::write(c.script_name) +
                        ",\"input_name\":" + json::write(c.input_name) +
                        ",\"code\":" + json::write(c.code) + ",\"label\":" +
                        json::write(c.label) + "}");
    std::vector<std::string> forms;
    for (const auto& f : response.forms)
        forms.push_back("{\"kind\":" + json::write(f.kind) + ",\"script_name\":" +
                        json::write(f.script_name) + ",\"product_tag\":" +
                        json::write(f.product_tag) + "}");
    return "{\"product\":" + summary_json(response.product) +
           ",\"element\":" + json::write(response.element) +
           ",\"fields\":" + rr::array_of(fields) + ",\"inputs\":" + rr::array_of(inputs) +
           ",\"codes\":" + rr::array_of(codes) + ",\"forms\":" + rr::array_of(forms) + "}";
}

void product_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    ores::nats::service::nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"trading"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/trading/products").catalogue("web_get_trading_products", "trading", "products", false)
            .summary("The products a screen can book")
            .description("Every product type, then every library script no product type "
                         "twins, each by its code, name and construction.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<schema::list_products_request>([&] {
                    const auto resp = forwarding::forward(req, session, schema::list_products_request{});
                    if (!resp.success)
                        return http_response::internal_error(resp.message);
                    return http_response::json(list_json(resp));
                });
            }));

    add(router->get("/api/trading/products/{code}").catalogue("web_get_trading_products_code", "trading", "products", false)
            .summary("One product's fields and forms")
            .description("The fields of the product's element and of every schema type they "
                         "reach, the inputs and codes of the scripts its forms run, and the "
                         "forms. A code no product has is not found; a product with no fields to "
                         "describe is unprocessable; both state the service's reason.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<schema::describe_product_request>([&] {
                    schema::describe_product_request msg;
                    msg.code = code_of(req);
                    const auto resp = forwarding::forward(req, session, msg);
                    if (resp.refusal_code == "not_found")
                        return http_response::not_found(resp.message);
                    if (resp.refusal_code == "not_describable")
                        return http_response::error(http_status::unprocessable_content,
                                                    resp.message);
                    if (!resp.success)
                        return http_response::internal_error(resp.message);
                    return http_response::json(describe_json(resp));
                });
            }));
}

}
