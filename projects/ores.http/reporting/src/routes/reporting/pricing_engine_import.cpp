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
#include "ores.http/routes/reporting/pricing_engine_import.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/reporting/pricing_engine_rows.hpp"
#include "ores.http/routes/reporting/reporting_configuration_rows.hpp"
#include "ores.ore.api/messaging/pricing_engines_protocol.hpp"
#include <boost/uuid/string_generator.hpp>
#include <optional>
#include <string>
#include <vector>

namespace ores::http::routes::reporting {

/**
 * @brief The preview's body: the file's text and the configuration to compare
 * it against, or nothing for a configuration the file would create.
 */
struct pricing_preview_write final {
    std::string content;
    std::optional<std::string> configuration_id;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<pricing_preview_write>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{
        field{"content", &pricing_preview_write::content},
        field{"configurationId", &pricing_preview_write::configuration_id}};
}

/**
 * @brief The import's body: the same file, the target, the removed rows the
 * person kept, and the reason the write is theirs.
 */
struct pricing_import_write final {
    std::string content;
    std::optional<std::string> configuration_id;
    std::string new_name;
    std::string new_variant;
    std::vector<std::string> removals_kept;
    ores::http::net::record_routes::stated_intent intent;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<pricing_import_write>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{
        field{"content", &pricing_import_write::content},
        field{"configurationId", &pricing_import_write::configuration_id},
        field{"newName", &pricing_import_write::new_name},
        field{"newVariant", &pricing_import_write::new_variant},
        field{"removalsKept", &pricing_import_write::removals_kept},
        field{"intent", &pricing_import_write::intent}};
}

namespace {

namespace forwarding = ores::http::forwarding;
namespace ore_messaging = ores::ore::messaging;
namespace rr = ores::http::net::record_routes;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;

/// The largest document the ORE service reads, in bytes.
constexpr std::uint64_t max_document_bytes = 4 * 1024 * 1024;

/// The sentence a body of this unit states when it is not the shape the form sends.
constexpr std::string_view malformed_import_write =
    "An import states the document's text and the configuration it changes.";

/// The configuration the service reads a document by, or nothing for one the file would create.
std::string
target_of(const http_request& req, nats_client& session, const std::optional<std::string>& asked) {
    if (!asked || asked->empty())
        return std::string();
    if (!rr::is_uuid(*asked))
        rr::invalid("An import names its configuration by its id.");
    return pricing_engine_rows::read_document(
               req, session, boost::uuids::string_generator()(*asked))
        .configuration_id;
}

}

void pricing_engine_import::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"reporting"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->post("/api/reporting/pricing-engines/ore-import/preview")
            .summary("What importing an ORE pricing engines document would change")
            .description("The ORE service parses the file and says what importing it would do. "
                         "Nothing is written, and the answer is the service's own words, a "
                         "refusal included, because the person reads the reason on the step.")
            .max_body(2 * max_document_bytes)
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<ore_messaging::preview_pricing_engines_import_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto body = rr::lenient_body_of<pricing_preview_write>(
                        req, {"content", "configurationId"}, std::string(malformed_import_write));
                    if (body.content.empty())
                        rr::invalid(std::string(malformed_import_write));
                    ore_messaging::preview_pricing_engines_import_request msg;
                    msg.content = body.content;
                    msg.target_configuration_id = target_of(req, session, body.configuration_id);
                    return forwarding::respond(req, session, msg);
                });
            }));

    add(router->post("/api/reporting/pricing-engines/ore-import")
            .summary("Import an ORE pricing engines document")
            .description("The ORE service writes the document through the save the screen itself "
                         "uses, so the validation and the history are the screen's, and answers "
                         "with what it saved and the rows it could not write.")
            .max_body(2 * max_document_bytes)
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<ore_messaging::import_pricing_engines_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto body = rr::lenient_body_of<pricing_import_write>(
                        req,
                        {"content",
                         "configurationId",
                         "newName",
                         "newVariant",
                         "removalsKept",
                         "intent"},
                        std::string(malformed_import_write));
                    const auto new_name = rr::trimmed(body.new_name);
                    const auto new_variant = rr::trimmed(body.new_variant);
                    if (body.content.empty() || rr::characters_of(new_name) > 200 ||
                        rr::characters_of(new_variant) > 200)
                        rr::invalid(std::string(malformed_import_write));
                    ore_messaging::import_pricing_engines_request msg;
                    msg.content = body.content;
                    msg.target_configuration_id = target_of(req, session, body.configuration_id);
                    msg.new_configuration_name = new_name;
                    msg.new_configuration_variant = new_variant;
                    msg.removals_kept = body.removals_kept;
                    const auto intent = reporting_configuration_rows::intent_or_none(body.intent);
                    msg.reason_code = intent.reason_code;
                    msg.commentary = intent.commentary;
                    return forwarding::respond(req, session, msg);
                });
            }));
}

}
