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
#include "ores.http/routes/reporting/pricing_engine_routes.hpp"
#include "ores.analytics.api/messaging/pricing_model_config_protocol.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/domain/http_status.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.core/routes/history_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/reporting/engine_settings_rows.hpp"
#include "ores.http.api/net/json_shape.hpp"
#include "ores.http/routes/reporting/pricing_engine_rows.hpp"
#include "ores.http/routes/reporting/pricing_engine_shapes.hpp"
#include "ores.ore.api/messaging/pricing_engines_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::reporting {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using pricing_engine_shapes::summary_json_of;
using ores::http::net::shape::member;
using ores::http::net::shape::object;
using ores::nats::service::nats_client;

/// One record whose versions the history tab draws, and the name it states.
struct history_entity final {
    std::string type;
    std::string id;
    std::string name;
};

/// The record kinds a document's timeline draws.
constexpr std::string_view config_entity_type = "ores.analytics.pricing_model_config";
constexpr std::string_view product_entity_type = "ores.analytics.pricing_model_product";
constexpr std::string_view parameter_entity_type = "ores.analytics.pricing_model_product_parameter";

/// Every record of a document, the configuration first.
///
/// A save writes the three together and a person reads one timeline, so they
/// are read together. A parameter is named by the product it belongs to and its
/// own name, because its own name repeats across products.
std::vector<history_entity> entities_of(const pricing_document_row& document) {
    std::vector<history_entity> entities{
        {std::string(config_entity_type), document.id, document.name}};
    std::map<std::string, std::string> type_of_product;
    for (const auto& product : document.products) {
        type_of_product[product.id] = product.type_code;
        entities.push_back({std::string(product_entity_type), product.id, product.type_code});
    }
    for (const auto& parameter : document.parameters) {
        std::string name;
        if (parameter.product_id) {
            const auto product = type_of_product.find(*parameter.product_id);
            if (product != type_of_product.end())
                name = product->second;
        }
        if (!name.empty())
            name += " · ";
        entities.push_back(
            {std::string(parameter_entity_type), parameter.id, name + parameter.name});
    }
    return entities;
}

/// Every version of every record of a document, the configuration's first.
std::vector<std::string> history_rows_of(const http_request& req,
                                         nats_client& session,
                                         const pricing_document_row& document) {
    std::vector<std::string> rows;
    for (const auto& entity : entities_of(document)) {
        const auto versions =
            ores::http::routes::history_routes::read_versions(req, session, entity.type, entity.id);
        for (const auto& version : versions) {
            auto members = std::vector<std::string>{member("entityType", json::write(entity.type)),
                                                    member("entityId", json::write(entity.id)),
                                                    member("entityName", json::write(entity.name))};
            const auto stated = ores::http::routes::history_routes::version_members(version);
            members.insert(members.end(), stated.begin(), stated.end());
            rows.push_back(object(members));
        }
    }
    return rows;
}

/// One byte written as a percent-encoded octet, in upper-case hex.
void append_encoded(unsigned char c, std::string& out) {
    static constexpr char digits[] = "0123456789ABCDEF";
    out += '%';
    out += digits[c >> 4];
    out += digits[c & 0x0F];
}

/**
 * @brief The header that offers one file to the browser, under the name the ORE
 * service stated.
 *
 * The name comes from the reporting file types table rather than from this
 * server, so it is quoted and stripped of the characters that would end the
 * value, and nothing else about it is rewritten. A header is ASCII, so a name
 * with a byte outside it states both a plain name and its encoded form, and the
 * browser saves the file under its own name.
 */
std::string ore_document_disposition_of(std::string_view name) {
    const auto strippable = [](unsigned char c) {
        return c < 0x20 || c == 0x7F || c == '"' || c == '\\' || c == '/';
    };
    std::string safe;
    std::string ascii;
    for (const char raw : name) {
        const auto c = static_cast<unsigned char>(raw);
        if (strippable(c))
            continue;
        safe += raw;
        if (c < 0x7F)
            ascii += raw;
    }
    if (!safe.empty() && safe == ascii)
        return "attachment; filename=\"" + safe + "\"";

    std::string encoded;
    for (const char raw : name) {
        const auto c = static_cast<unsigned char>(raw);
        const auto plain = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
                           (c >= '0' && c <= '9') || c == '-' || c == '_' || c == '.' || c == '~';
        if (plain)
            encoded += raw;
        else
            append_encoded(c, encoded);
    }
    return "attachment; filename=\"" + (ascii.empty() ? std::string("document") : ascii) +
           "\"; filename*=UTF-8''" + encoded;
}

}

void pricing_engine_routes::register_read_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    ores::nats::service::nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"reporting"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/reporting/pricing-engines")
            .summary("The pricing engine configurations a party holds")
            .description("One row per configuration: the party that holds it, the version, the "
                         "variant, and how many products and parameters its document holds.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<ores::analytics::messaging::list_pricing_model_configs_request>(
                    [&] {
                        rr::refuse_unknown_params(req, {});
                        const auto configs = pricing_engine_rows::read_configs(req, session);
                        const auto counts = pricing_engine_rows::read_counts(req, session);
                        const auto parties = engine_settings_rows::read_party_names(req, session);
                        std::vector<std::string> rows;
                        rows.reserve(configs.size());
                        for (const auto& config : configs)
                            rows.push_back(summary_json_of(config, counts, parties));
                        return http_response::json(object({member("rows", rr::array_of(rows))}));
                    });
            }));

    add(router->get("/api/reporting/pricing-engines/{id}")
            .summary("One pricing engine document and the reports that bind it")
            .description("The configuration, its products and its parameters as one document, "
                         "and the reports that bind it through its reporting configuration.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<ores::analytics::messaging::list_pricing_model_configs_request>(
                    [&] {
                        rr::refuse_unknown_params(req, {});
                        const auto raw = req.get_path_param("id");
                        if (!rr::is_uuid(raw))
                            rr::invalid("A pricing engine is named by its id.");
                        const auto id = boost::uuids::string_generator()(raw);
                        const auto document = pricing_engine_rows::read_document(req, session, id);
                        const auto parties = engine_settings_rows::read_party_names(req, session);
                        const auto reports = pricing_engine_rows::bound_reports_json_of(
                            req, session, document.configuration_id);
                        return http_response::json(
                            pricing_engine_shapes::document_json_of(document, parties, reports));
                    });
            }));

    add(router->get("/api/reporting/pricing-engines/{id}/history")
            .summary("Every version of a pricing engine document")
            .description("The versions of the configuration, of every product it holds and of "
                         "every parameter, newest first within each record, so one save reads as "
                         "one timeline.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<ores::analytics::messaging::list_pricing_model_configs_request>(
                    [&] {
                        rr::refuse_unknown_params(req, {});
                        const auto raw = req.get_path_param("id");
                        if (!rr::is_uuid(raw))
                            rr::invalid("A pricing engine is named by its id.");
                        const auto document = pricing_engine_rows::read_document(
                            req, session, boost::uuids::string_generator()(raw));
                        return http_response::json(object({member(
                            "versions", rr::array_of(history_rows_of(req, session, document)))}));
                    });
            }));

    add(router->get("/api/reporting/pricing-engines/{id}/ore-document")
            .summary("The ORE document of one pricing engine configuration")
            .description("The ORE service writes the file and names it, and this route offers it "
                         "to the browser under that name. The service reads the document by the "
                         "reporting configuration the record fills, so the two ids are joined "
                         "here rather than sent to the screen.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<ores::ore::messaging::export_pricing_engines_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto raw = req.get_path_param("id");
                    if (!rr::is_uuid(raw))
                        rr::invalid("A pricing engine is named by its id.");
                    const auto document = pricing_engine_rows::read_document(
                        req, session, boost::uuids::string_generator()(raw));
                    ores::ore::messaging::export_pricing_engines_request ask;
                    ask.configuration_id = document.configuration_id;
                    const auto exported = forwarding::forward(req, session, ask);
                    if (!exported.success || exported.files.empty())
                        rr::invalid(exported.message.empty() ?
                                        "The pricing engines document could not be written." :
                                        exported.message);
                    if (exported.files.size() > 1)
                        rr::refuse(http_status::bad_gateway,
                                   "internal",
                                   "The export answered more than one file.");
                    const auto& file = exported.files.front();
                    http_response answer;
                    answer.content_type = "application/xml";
                    answer.body = file.content;
                    answer.set_header("content-disposition",
                                      ore_document_disposition_of(file.name));
                    return answer;
                });
            }));
}

}
