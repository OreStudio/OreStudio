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
#include "ores.http/routes/reporting/pricing_engine_writes.hpp"
#include "ores.analytics.api/messaging/configuration_document_protocol.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/domain/http_status.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/reporting/engine_settings_rows.hpp"
#include "ores.http/routes/reporting/pricing_engine_rows.hpp"
#include "ores.http/routes/reporting/pricing_engine_shapes.hpp"
#include "ores.http/routes/reporting/reporting_configuration_rows.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::http::routes::reporting {

/**
 * @brief One product as a screen states it.
 */
struct pricing_product_write final {
    std::string id;
    int version{};
    std::string type_code;
    std::string model;
    std::string engine;
    int position{};
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<pricing_product_write>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"id", &pricing_product_write::id},
                                            field{"version", &pricing_product_write::version},
                                            field{"typeCode", &pricing_product_write::type_code},
                                            field{"model", &pricing_product_write::model},
                                            field{"engine", &pricing_product_write::engine},
                                            field{"position", &pricing_product_write::position}};
}

/**
 * @brief One parameter as a screen states it.
 */
struct pricing_parameter_write final {
    std::string id;
    int version{};
    std::optional<std::string> product_id;
    std::string scope;
    std::string name;
    std::string value;
    int position{};
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<pricing_parameter_write>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{
        field{"id", &pricing_parameter_write::id},
        field{"version", &pricing_parameter_write::version},
        field{"productId", &pricing_parameter_write::product_id},
        field{"scope", &pricing_parameter_write::scope},
        field{"name", &pricing_parameter_write::name},
        field{"value", &pricing_parameter_write::value},
        field{"position", &pricing_parameter_write::position}};
}

/**
 * @brief A document as a screen states it.
 *
 * The version is the one the screen read, and only a change states one: a create
 * writes the first version of everything it holds.
 */
struct pricing_document_write final {
    std::string name;
    std::string description;
    std::string variant;
    std::vector<pricing_product_write> products;
    std::vector<pricing_parameter_write> parameters;
    ores::http::net::record_routes::stated_intent intent;
    std::optional<int> version;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<pricing_document_write>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{
        field{"name", &pricing_document_write::name},
        field{"description", &pricing_document_write::description},
        field{"variant", &pricing_document_write::variant},
        field{"products", &pricing_document_write::products},
        field{"parameters", &pricing_document_write::parameters},
        field{"intent", &pricing_document_write::intent},
        field{"version", &pricing_document_write::version}};
}

namespace {

namespace forwarding = ores::http::forwarding;
namespace analytics_messaging = ores::analytics::messaging;
namespace rr = ores::http::net::record_routes;
namespace configuration_rows = reporting_configuration_rows;
namespace shapes = pricing_engine_shapes;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::utility::domain::change_intent;
using ores::utility::uuid::tenant_id;
using ores::nats::service::nats_client;

auto& pricing_write_lg() {
    static auto instance = ores::logging::make_logger("ores.http.reporting.pricing_engine_writes");
    return instance;
}

/// The reporting configuration type a pricing engine document is registered as.
constexpr std::string_view pricing_configuration_type = "pricing_engines";

/// The component that owns the configuration a pricing engine document fills.
constexpr std::string_view pricing_owning_component = "ores.analytics";

/// How many products and parameters one document may hold, as the BFF stated it.
constexpr std::size_t max_products = 500;
constexpr std::size_t max_parameters = 5000;

/// The sentence a body of this unit states when it is not the shape the form sends.
constexpr std::string_view malformed_document_write =
    "A write states the document's name, its products, its parameters and the reason it changes.";

/// The party every row carries until the service stamps the session's own.
const boost::uuids::uuid no_party{};

/// The time every row carries until the service stamps its own clock.
const std::chrono::system_clock::time_point no_time{};

/// The tenant the caller's token states, refused when it states none.
tenant_id tenant_of(const http_request& req) {
    const std::optional<std::string> stated =
        req.authenticated_user ? req.authenticated_user->tenant_id : std::nullopt;
    if (!stated || stated->empty())
        rr::refuse(http_status::internal_server_error,
                   "internal-error",
                   "The caller's session states no tenant, so there is nobody to write for.");
    auto tenant = tenant_id::from_string(*stated);
    if (!tenant)
        rr::refuse(http_status::internal_server_error, "internal-error", tenant.error());
    return *tenant;
}

/// The document as the form states it, with each member the form leaves out at its default.
pricing_document_write document_of(const http_request& req, std::string_view extra_member) {
    const auto sentence = std::string(malformed_document_write);
    if (extra_member.empty())
        return rr::lenient_body_of<pricing_document_write>(
            req, {"name", "description", "variant", "products", "parameters", "intent"}, sentence);
    return rr::lenient_body_of<pricing_document_write>(
        req,
        {"name", "description", "variant", "products", "parameters", "intent", extra_member},
        sentence);
}

/// A document the screen may write, refused when it states anything the tables cannot hold.
void check_document(const pricing_document_write& body) {
    const auto name = rr::trimmed(body.name);
    if (name.empty() || rr::characters_of(name) > 200 ||
        rr::characters_of(body.description) > 2000 ||
        rr::characters_of(rr::trimmed(body.variant)) > 200)
        rr::invalid(std::string(malformed_document_write));
    if (body.products.size() > max_products || body.parameters.size() > max_parameters)
        rr::invalid(std::string(malformed_document_write));
    for (const auto& product : body.products) {
        const auto type_code = rr::trimmed(product.type_code);
        if (!rr::is_uuid(product.id) || product.version < 0 || product.position < 0 ||
            type_code.empty() || rr::characters_of(type_code) > 200 ||
            rr::characters_of(rr::trimmed(product.model)) > 200 ||
            rr::characters_of(rr::trimmed(product.engine)) > 200)
            rr::invalid(std::string(malformed_document_write));
    }
    for (const auto& parameter : body.parameters) {
        const auto scope = rr::trimmed(parameter.scope);
        const auto parameter_name = rr::trimmed(parameter.name);
        if (!rr::is_uuid(parameter.id) || parameter.version < 0 || parameter.position < 0 ||
            (parameter.product_id && !rr::is_uuid(*parameter.product_id)) || scope.empty() ||
            rr::characters_of(scope) > 50 || parameter_name.empty() ||
            rr::characters_of(parameter_name) > 200 || rr::characters_of(parameter.value) > 2000)
            rr::invalid(std::string(malformed_document_write));
    }
}

/**
 * @brief The document as the three tables' rows.
 *
 * The party and the time are the service's to stamp: the session that saves a
 * document acts for a party, and the store writes the time. The tenant is not,
 * because the tenant type refuses the nil id and a row must state one.
 */
analytics_messaging::pricing_engines_document rows_of(const pricing_document_write& body,
                                                      const boost::uuids::uuid& id,
                                                      int version,
                                                      const boost::uuids::uuid& configuration_id,
                                                      const tenant_id& tenant,
                                                      const change_intent& intent) {
    const auto stamp_common = [&](auto& row) {
        row.tenant_id = tenant;
        row.party_id = no_party;
        row.modified_by = std::string();
        row.performed_by = std::string();
        row.change_reason_code = intent.reason_code;
        row.change_commentary = intent.commentary;
        row.recorded_at = no_time;
    };

    analytics_messaging::pricing_engines_document document;
    auto& config = document.config;
    stamp_common(config);
    config.version = version;
    config.id = id;
    config.name = rr::trimmed(body.name);
    config.description = body.description;
    config.config_variant = rr::trimmed(body.variant);
    config.configuration_id = configuration_id;

    document.products.reserve(body.products.size());
    for (const auto& product : body.products) {
        auto& row = document.products.emplace_back();
        stamp_common(row);
        row.version = product.version;
        row.id = boost::uuids::string_generator()(product.id);
        row.pricing_model_config_id = id;
        row.pricing_engine_type_code = rr::trimmed(product.type_code);
        row.model = rr::trimmed(product.model);
        row.engine = rr::trimmed(product.engine);
        row.position = product.position;
    }

    document.parameters.reserve(body.parameters.size());
    for (const auto& parameter : body.parameters) {
        auto& row = document.parameters.emplace_back();
        stamp_common(row);
        row.version = parameter.version;
        row.id = boost::uuids::string_generator()(parameter.id);
        row.pricing_model_config_id = id;
        if (parameter.product_id)
            row.pricing_model_product_id = boost::uuids::string_generator()(*parameter.product_id);
        row.parameter_scope = rr::trimmed(parameter.scope);
        row.parameter_name = rr::trimmed(parameter.name);
        row.parameter_value = parameter.value;
        row.position = parameter.position;
    }
    return document;
}

/// Saves the document, or refuses with the sentence the service stated.
void save_document(const http_request& req,
                   nats_client& session,
                   analytics_messaging::pricing_engines_document document) {
    analytics_messaging::save_pricing_engines_document_request msg;
    msg.document = std::move(document);
    const auto reply = forwarding::forward(req, session, msg);
    if (!reply.success) {
        BOOST_LOG_SEV(pricing_write_lg(), ores::logging::debug)
            << "The pricing engines service refused the document: " << reply.message;
        rr::invalid(reply.message.empty() ? "The pricing engines document could not be written." :
                                            reply.message);
    }
}

/// One document as the screen reads it, with the reports that bind it.
http_response written_document_answer(const http_request& req,
                                      nats_client& session,
                                      const boost::uuids::uuid& id,
                                      http_status status) {
    const auto document = pricing_engine_rows::read_document(req, session, id);
    const auto parties = engine_settings_rows::read_party_names(req, session);
    const auto reports =
        pricing_engine_rows::bound_reports_json_of(req, session, document.configuration_id);
    return http_response::json(shapes::document_json_of(document, parties, reports), status);
}

/// The id a path states.
boost::uuids::uuid document_id_of(const http_request& req) {
    const auto raw = req.get_path_param("id");
    if (!rr::is_uuid(raw))
        rr::invalid("A pricing engine is named by its id.");
    return boost::uuids::string_generator()(raw);
}

}

void pricing_engine_writes::register_write_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"reporting"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->post("/api/reporting/pricing-engines")
            .summary("Create a pricing engine document and the header a report binds it through")
            .description("The reporting configuration is written first and the document second, "
                         "and a refused document removes the configuration again, so a create "
                         "the service refuses leaves nothing behind.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<analytics_messaging::save_pricing_engines_document_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto body = document_of(req, "");
                    check_document(body);
                    const auto intent = configuration_rows::intent_or_none(body.intent);
                    const auto id = boost::uuids::random_generator()();
                    const auto configuration_id =
                        configuration_rows::register_configuration(req,
                                                                   session,
                                                                   rr::trimmed(body.name),
                                                                   pricing_configuration_type,
                                                                   pricing_owning_component,
                                                                   intent);
                    try {
                        save_document(
                            req,
                            session,
                            rows_of(body, id, 0, configuration_id, tenant_of(req), intent));
                    } catch (...) {
                        configuration_rows::remove_configuration(
                            req, session, rr::trimmed(body.name), intent);
                        throw;
                    }
                    return written_document_answer(req, session, id, http_status::created);
                });
            }));

    add(router->put("/api/reporting/pricing-engines/{id}")
            .summary("Change a pricing engine document against the version the screen read")
            .description("The party and the configuration the record fills travel from the record "
                         "just read, so a change cannot move a document to another party. A "
                         "product or a parameter the document omits is removed.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<analytics_messaging::save_pricing_engines_document_request>([&] {
                    const auto id = document_id_of(req);
                    const auto body = document_of(req, "version");
                    if (!body.version || *body.version < 1)
                        rr::invalid(std::string(malformed_document_write));
                    check_document(body);
                    const auto stored = pricing_engine_rows::read_document(req, session, id);
                    save_document(req,
                                  session,
                                  rows_of(body,
                                          id,
                                          *body.version,
                                          boost::uuids::string_generator()(stored.configuration_id),
                                          tenant_of(req),
                                          configuration_rows::intent_or_none(body.intent)));
                    return written_document_answer(req, session, id, http_status::ok);
                });
            }));

    add(router->delete_("/api/reporting/pricing-engines/{id}")
            .summary("Remove a pricing engine document")
            .description("The document is removed through the service that owns it, and a "
                         "service that says it did not is a refusal that carries its sentence. "
                         "The reporting configuration the document filled stays, because the "
                         "report that binds it is named by it.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<analytics_messaging::delete_pricing_engines_document_request>(
                    [&] {
                        const auto id = document_id_of(req);
                        const auto stored = pricing_engine_rows::read_document(req, session, id);
                        analytics_messaging::delete_pricing_engines_document_request msg;
                        msg.configuration_id = stored.configuration_id;
                        const auto deleted = forwarding::forward(req, session, msg);
                        if (!deleted.success)
                            rr::invalid(deleted.message.empty() ?
                                            "The pricing engines document could not be removed." :
                                            deleted.message);
                        return rr::no_content();
                    });
            }));
}

}
