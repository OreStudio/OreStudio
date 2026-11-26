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
#include "ores.http/routes/vault/vault_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.nats/service/request_helpers.hpp"
#include "ores.security/crypto/key_file.hpp"
#include "ores.security/crypto/sealed_box.hpp"
#include "ores.utility/convert/base64_converter.hpp"
#include "ores.vault.api/messaging/custody_context.hpp"
#include "ores.vault.api/messaging/secret_operations_protocol.hpp"
#include <boost/json.hpp>
#include <string>
#include <utility>
#include <vector>

namespace ores::http::routes::vault {

using namespace ores::logging;
using ores::security::crypto::key_file;
using ores::security::crypto::sealed_box;
using ores::utility::convert::base64_converter;
using ores::vault::messaging::custody_context;

namespace {

/// The credential the deployment places for the gateway's own key pair.
constexpr std::string_view gateway_credential = "gateway.transport.key";

/// Reads one string member of a JSON body, refusing a body that lacks it.
[[nodiscard]] std::string field(const boost::json::object& body, std::string_view name) {
    const auto found = body.find(name);
    if (found == body.end() || !found->value().is_string()) {
        throw std::runtime_error("The body names no " + std::string(name));
    }
    return std::string(found->value().as_string());
}

}

std::shared_ptr<const vault_routes::gateway_keys> vault_routes::load_keys() {
    const auto directory = key_file::credential_directory();
    if (!directory.has_value()) {
        ORES_LOG_SEV(lg(), error) << "The gateway was started without "
                                  << key_file::directory_variable;
        throw std::runtime_error("The gateway has no key material: the unit was started without " +
                                 std::string(key_file::directory_variable));
    }

    auto keys = std::make_shared<gateway_keys>();
    keys->private_key = key_file::read(*directory / gateway_credential, sealed_box::KEY_LEN);
    keys->public_key = sealed_box::public_key_of(keys->private_key);
    ORES_LOG_SEV(lg(), info) << "Read the gateway's transport key from " << *directory;
    return keys;
}

void vault_routes::register_routes(std::shared_ptr<ores::http::net::router> router,
                                   std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                   ores::nats::service::nats_client& session,
                                   std::shared_ptr<const gateway_keys> keys) {
    ORES_LOG_SEV(lg(), info) << "Registering vault routes";

    auto write_route = router->post("/api/v1/vault/secrets")
                           .summary("Store a secret")
                           .description("Seals the value to the vault and forwards the write.")
                           .tags({"vault"})
                           .entity_catalogue("vault", "secrets", "write")
                           .json_body()
                           .auth_required()
                           .handler([&session, keys](const ores::http::domain::http_request& req) {
                               return handle_write(req, session, *keys);
                           });
    const auto write_built = write_route.build();
    router->add_route(write_built);
    registry->register_route(write_built);

    auto reveal_route =
        router->get("/api/v1/vault/secrets/{name}/value")
            .summary("Reveal one secret")
            .description("Resolves as this process, opens the answer, and records the person.")
            .tags({"vault"})
            .entity_catalogue("vault", "secrets", "resolve")
            .no_body()
            .auth_required()
            .handler([&session, keys](const ores::http::domain::http_request& req) {
                return handle_reveal(req, session, *keys);
            });
    const auto reveal_built = reveal_route.build();
    router->add_route(reveal_built);
    registry->register_route(reveal_built);
}

boost::asio::awaitable<ores::http::domain::http_response>
vault_routes::handle_write(const ores::http::domain::http_request& req,
                           ores::nats::service::nats_client& session,
                           const gateway_keys& keys) {
    using namespace ores::http::forwarding;

    try {
        const auto body = boost::json::parse(req.body).as_object();
        const auto name = field(body, "name");
        const auto owner = field(body, "owner");
        const auto value = field(body, "value");

        // The vault publishes the key a caller seals to. A write cannot happen
        // without it, and it is not a secret, so a miss is a bad request and
        // not an authorisation failure.
        const auto published =
            authenticated_request_and_decode<ores::vault::messaging::vault_public_key_response>(
                session,
                ores::vault::messaging::vault_public_key_request::nats_subject,
                ores::vault::messaging::vault_public_key_request{});
        if (!published.has_value() || !published->success) {
            co_return ores::http::domain::http_response::internal_error(
                "The vault published no transport key");
        }

        const auto raw_key = base64_converter::convert(published->public_key);
        const auto sealed =
            sealed_box::seal(std::vector<unsigned char>(raw_key.begin(), raw_key.end()),
                             value,
                             custody_context::write(name, owner));

        ores::vault::messaging::write_secret_request request;
        request.name = name;
        request.owner = owner;
        request.sealed_value = sealed;

        // The person's own token travels with the request, so the vault checks
        // the permission they hold and records them as the actor.
        co_return respond(req, session, request);
    } catch (const std::exception& e) {
        ORES_LOG_SEV(lg(), warn) << "Write refused: " << e.what();
        co_return ores::http::domain::http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<ores::http::domain::http_response>
vault_routes::handle_reveal(const ores::http::domain::http_request& req,
                            ores::nats::service::nats_client& session,
                            const gateway_keys& keys) {
    try {
        const auto name = req.get_path_param("name");
        if (name.empty()) {
            co_return ores::http::domain::http_response::bad_request("The path names no secret");
        }

        // The vault seals only to a consumer it knows, so the gateway states
        // its own public key first. The row is about this process: the vault
        // takes the name from the token, so this can only ever register the
        // caller. Repeating it is harmless, and a failed registration is left
        // for the resolve to report.
        ores::vault::messaging::register_consumer_request registration;
        registration.public_key = base64_converter::convert(keys.public_key);
        const auto registered = ores::nats::service::authenticated_request_and_decode<
            ores::vault::messaging::register_consumer_response>(
            session, ores::vault::messaging::register_consumer_request::nats_subject, registration);
        if (!registered.has_value() || !registered->success) {
            ORES_LOG_SEV(lg(), warn) << "The gateway could not register its key with the vault";
        }

        // The gateway resolves as itself: the vault seals to the consumer its
        // own token names, which is the only consumer holding a key here. The
        // person's permission is the vault's to check on the write and the
        // gateway's log records them on the read.
        const auto resolved = ores::nats::service::authenticated_request_and_decode<
            ores::vault::messaging::resolve_secret_response>(
            session,
            ores::vault::messaging::resolve_secret_request::nats_subject,
            ores::vault::messaging::resolve_secret_request{.name = name});
        if (!resolved.has_value()) {
            co_return ores::http::domain::http_response::internal_error(
                "The vault answered nothing");
        }
        if (!resolved->success) {
            co_return ores::http::domain::http_response::not_found(resolved->message);
        }

        const auto value = sealed_box::open(
            keys.private_key,
            resolved->sealed_value,
            custody_context::delivery(resolved->subject, resolved->name, resolved->key_version));

        boost::json::object answer_body;
        answer_body["name"] = resolved->name;
        answer_body["key_version"] = resolved->key_version;
        answer_body["value"] = value;
        auto answer = ores::http::domain::http_response::json(boost::json::serialize(answer_body));
        // A value a person asked for is never cached by anything between here
        // and the screen.
        answer.headers["Cache-Control"] = "no-store";
        co_return answer;
    } catch (const std::exception& e) {
        ORES_LOG_SEV(lg(), warn) << "Reveal refused: " << e.what();
        co_return ores::http::domain::http_response::bad_request(e.what());
    }
}

}
