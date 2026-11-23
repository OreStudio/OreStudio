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
#ifndef ORES_TRADING_CORE_MESSAGING_PRODUCT_SCHEMA_HANDLER_HPP
#define ORES_TRADING_CORE_MESSAGING_PRODUCT_SCHEMA_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.trading.api/messaging/product_schema_operation_protocol.hpp"
#include "ores.trading.core/repository/product_type_repository.hpp"
#include "ores.trading.core/repository/script_input_code_repository.hpp"
#include "ores.trading.core/repository/script_input_repository.hpp"
#include "ores.trading.core/repository/script_repository.hpp"
#include "ores.trading.core/repository/script_twin_input_repository.hpp"
#include "ores.trading.core/service/product_schema_service.hpp"
#include <functional>
#include <optional>
#include <utility>

namespace ores::trading::messaging {

namespace {
inline auto& product_schema_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.trading.messaging.product_schema_handler");
    return instance;
}
} // namespace

/**
 * @brief Hand-written NATS handler for a product's description.
 *
 * The products, scripts and inputs are trading's own rows, read in the
 * caller's context. The instrument fields are ores.ore's, read through the
 * source the registrar gives, which reads the cache of ores.ore's parameters,
 * so the handler reads no table of ores.ore.
 */
class product_schema_handler {
public:
    using fields_source = std::function<service::instrument_fields()>;

    product_schema_handler(ores::nats::service::client& nats, const ores::database::context& ctx,
                           std::optional<ores::security::jwt::jwt_authenticator> verifier,
                           fields_source fields)
        : nats_(nats)
        , ctx_(ctx)
        , verifier_(std::move(verifier))
        , fields_(std::move(fields)) {}

    /**
     * @brief Serves trading.v1.ops.describe_product.
     */
    void describe_product(ores::nats::message msg) {
        serve<describe_product_request, describe_product_response>(
            msg, [this](const auto& ctx, const describe_product_request& req) {
                return service::describe_product(req.code, catalogue(ctx), fields_());
            });
    }

    /**
     * @brief Serves trading.v1.ops.list_products.
     */
    void list_products(ores::nats::message msg) {
        serve<list_products_request, list_products_response>(
            msg, [this](const auto& ctx, const list_products_request&) {
                return service::list_products(catalogue(ctx));
            });
    }

private:
    service::product_catalogue catalogue(const ores::database::context& ctx) const {
        return {.product_types = repository::product_type_repository().read_latest(ctx),
                .scripts = repository::script_repository().read_latest(ctx),
                .twins = repository::script_twin_input_repository().read_latest(ctx),
                .inputs = repository::script_input_repository().read_latest(ctx),
                .codes = repository::script_input_code_repository().read_latest(ctx)};
    }

    template <typename Request, typename Response, typename Serve>
    void serve(ores::nats::message& msg, Serve&& serve_request) {
        using ores::service::messaging::decode;
        using ores::service::messaging::error_reply;
        using ores::service::messaging::has_permission;
        using ores::service::messaging::reply;
        using namespace ores::logging;

        ORES_LOG_SEV(product_schema_handler_lg(), debug) << "Handling " << msg.subject;
        auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx) {
            error_reply(nats_, msg, ctx.error());
            return;
        }
        if (!has_permission(*ctx, "trading::product_types:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        Response resp;
        try {
            if (auto req = decode<Request>(msg))
                resp = serve_request(*ctx, *req);
            else
                resp.message = "Invalid request on " + msg.subject + ".";
        } catch (const std::exception& e) {
            ORES_LOG_SEV(product_schema_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    fields_source fields_;
};

}

#endif
