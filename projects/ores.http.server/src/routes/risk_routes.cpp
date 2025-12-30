/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.http.server/routes/risk_routes.hpp"

#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.risk/domain/currency_json.hpp"
#include "ores.risk/messaging/currency_protocol.hpp"
#include "ores.risk/messaging/currency_history_protocol.hpp"
#include "ores.risk/service/currency_service.hpp"

namespace ores::http_server::routes {

using namespace ores::telemetry::log;
using namespace ores::http::domain;
namespace asio = boost::asio;

risk_routes::risk_routes(database::context ctx,
    std::shared_ptr<comms::service::auth_session_service> sessions)
    : ctx_(std::move(ctx))
    , sessions_(std::move(sessions)) {
    BOOST_LOG_SEV(lg(), debug) << "Risk routes initialized";
}

void risk_routes::register_routes(std::shared_ptr<http::net::router> router,
    std::shared_ptr<http::openapi::endpoint_registry> registry) {

    BOOST_LOG_SEV(lg(), info) << "Registering Risk routes";

    auto get_currencies = router->get("/api/v1/currencies")
        .summary("Get currencies")
        .description("Retrieve currencies with pagination")
        .tags({"currencies"})
        .auth_required()
        .handler([this](const http_request& req) { return handle_get_currencies(req); });
    router->add_route(get_currencies.build());
    registry->register_route(get_currencies.build());

    auto save_currency = router->post("/api/v1/currencies")
        .summary("Save currency")
        .description("Create or update a currency")
        .tags({"currencies"})
        .auth_required()
        .roles({"admin"})
        .handler([this](const http_request& req) { return handle_save_currency(req); });
    router->add_route(save_currency.build());
    registry->register_route(save_currency.build());

    auto delete_currencies = router->delete_("/api/v1/currencies")
        .summary("Delete currencies")
        .description("Delete one or more currencies")
        .tags({"currencies"})
        .auth_required()
        .roles({"admin"})
        .handler([this](const http_request& req) { return handle_delete_currencies(req); });
    router->add_route(delete_currencies.build());
    registry->register_route(delete_currencies.build());

    auto currency_history = router->get("/api/v1/currencies/{code}/history")
        .summary("Get currency history")
        .description("Retrieve version history for a currency")
        .tags({"currencies"})
        .auth_required()
        .handler([this](const http_request& req) { return handle_get_currency_history(req); });
    router->add_route(currency_history.build());
    registry->register_route(currency_history.build());

    BOOST_LOG_SEV(lg(), info) << "Risk routes registered: 4 endpoints";
}

asio::awaitable<http_response> risk_routes::handle_get_currencies(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling get currencies request";

    try {
        std::uint32_t offset = 0;
        std::uint32_t limit = 100;

        auto offset_str = req.get_query_param("offset");
        auto limit_str = req.get_query_param("limit");

        if (!offset_str.empty()) offset = std::stoul(offset_str);
        if (!limit_str.empty()) limit = std::stoul(limit_str);

        risk::service::currency_service service(ctx_);
        auto currencies = service.list_currencies(offset, limit);
        auto total = service.count_currencies();

        risk::messaging::get_currencies_response resp;
        resp.currencies = currencies;
        resp.total_available_count = total;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Get currencies error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> risk_routes::handle_save_currency(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling save currency request";

    try {
        auto save_req = rfl::json::read<risk::messaging::save_currency_request>(req.body);
        if (!save_req) {
            co_return http_response::bad_request("Invalid request body");
        }

        risk::service::currency_service service(ctx_);
        service.save_currency(save_req->currency);

        risk::messaging::save_currency_response resp;
        resp.success = true;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Save currency error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> risk_routes::handle_delete_currencies(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling delete currencies request";

    try {
        auto delete_req = rfl::json::read<risk::messaging::delete_currency_request>(req.body);
        if (!delete_req) {
            co_return http_response::bad_request("Invalid request body");
        }

        risk::service::currency_service service(ctx_);
        std::vector<risk::messaging::delete_currency_result> results;

        for (const auto& code : delete_req->iso_codes) {
            bool success = service.delete_currency(code);
            results.push_back({code, success, success ? "" : "Failed to delete"});
        }

        risk::messaging::delete_currency_response resp;
        resp.results = results;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Delete currencies error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> risk_routes::handle_get_currency_history(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling get currency history request";

    try {
        auto code = req.get_path_param("code");
        if (code.empty()) {
            co_return http_response::bad_request("Currency code required");
        }

        risk::service::currency_service service(ctx_);
        auto history_records = service.get_currency_history(code);

        risk::messaging::get_currency_history_response resp;
        resp.success = true;
        resp.history.iso_code = code;
        // Note: Would need to convert currencies to currency_version objects
        // For now, leaving history empty until currency_version conversion is added

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Get currency history error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

}
