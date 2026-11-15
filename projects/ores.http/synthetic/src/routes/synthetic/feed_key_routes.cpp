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
#include "ores.http/routes/synthetic/feed_key_routes.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.synthetic.api/domain/fx_spot_generation_config_codec.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config_codec.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/ir_curve_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/market_data_generation_config_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <exception>
#include <string>
#include <utility>
#include <vector>

namespace ores::http::routes::synthetic {

using namespace logging;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace messaging = ores::synthetic::messaging;
namespace rr = ores::http::net::record_routes;

namespace {

auto& feed_key_lg() {
    static auto instance = make_logger("ores.http.routes.synthetic.feed_key_routes");
    return instance;
}

/**
 * @brief One row of a list by its id, or a 404 that names the resource.
 */
template <typename Request, typename Filter, typename Rows>
auto feed_key_one(const http_request& req,
                  nats_client& session,
                  const boost::uuids::uuid& id,
                  const std::string& resource,
                  Rows Request::response_type::*rows) {
    Request msg;
    msg.offset = 0;
    msg.limit = 1;
    msg.order.field = "";
    msg.order.descending = false;
    msg.filter = Filter{};
    msg.filter->id_one_of = std::vector<boost::uuids::uuid>{id};
    const auto reply = ores::http::forwarding::forward(req, session, msg);
    rr::require_ok(reply.result);
    const auto& found = reply.*rows;
    if (found.empty())
        rr::refuse(http_status::not_found,
                   "not-found",
                   "There is no " + resource + " row " + boost::uuids::to_string(id) + ".");
    return found.front();
}

/**
 * @brief The feed's row with its provider, whether it runs, and where its run started.
 */
template <typename Request, typename Filter, typename Rows>
boost::asio::awaitable<http_response> feed_key_read(const http_request& req,
                                                    nats_client& session,
                                                    std::string resource,
                                                    Rows Request::response_type::*rows) {
    try {
        const auto id = rr::read<boost::uuids::uuid>(
            rr::code(req.get_path_param("key"), "A row is named by a code of 1 to 100 characters."),
            "key");
        const auto feed = feed_key_one<Request, Filter>(req, session, id, resource, rows);
        const auto collection = feed_key_one<messaging::list_market_data_generation_configs_request,
                                             messaging::market_data_generation_configs_filter>(
            req,
            session,
            feed.config_id,
            "collections",
            &messaging::list_market_data_generation_configs_response::
                market_data_generation_configs);

        const auto running =
            ores::http::forwarding::forward(req, session, messaging::list_feeds_request{});
        if (!running.success)
            rr::refuse(
                http_status::bad_gateway, "upstream-unavailable", std::string(rr::unanswered));
        const auto* run = static_cast<const messaging::running_feed*>(nullptr);
        for (const auto& candidate : running.running_feeds) {
            if (candidate.source_name == feed.source_name) {
                run = &candidate;
                break;
            }
        }

        auto row = ores::utility::codec::json::write(feed);
        row.pop_back();
        row += ",\"provider_code\":" + ores::utility::codec::json::write(collection.provider_code);
        row += std::string(",\"running\":") + (run != nullptr ? "true" : "false");
        row += ",\"start\":" + (run != nullptr && run->start ?
                                    ores::utility::codec::json::write(*run->start) :
                                    std::string("null"));
        row += "}";
        co_return http_response::json("{\"row\":" + row + "}");
    } catch (const rr::refusal& refusal) {
        co_return rr::answer(refusal);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(feed_key_lg(), error) << e.what();
        co_return http_response::api_error(
            http_status::bad_gateway, "internal", std::string(rr::unanswered));
    }
}

}

boost::asio::awaitable<http_response> feed_key_routes::handle_fx_feed(const http_request& req,
                                                                      nats_client& session) {
    return feed_key_read<messaging::list_fx_spot_generation_configs_request,
                         messaging::fx_spot_generation_configs_filter>(
        req,
        session,
        "fx-feeds",
        &messaging::list_fx_spot_generation_configs_response::fx_spot_generation_configs);
}

boost::asio::awaitable<http_response> feed_key_routes::handle_ir_curve_feed(const http_request& req,
                                                                            nats_client& session) {
    return feed_key_read<messaging::list_ir_curve_generation_configs_request,
                         messaging::ir_curve_generation_configs_filter>(
        req,
        session,
        "ir-curve-feeds",
        &messaging::list_ir_curve_generation_configs_response::ir_curve_generation_configs);
}

void feed_key_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    BOOST_LOG_SEV(lg(), info) << "Registering the feed key reads";

    auto fx_route =
        router->get("/api/synthetic/fx-feeds/key/{key}")
            .summary("Read one FX feed for a screen")
            .description("The feed's row with its provider, whether it runs, and where its run "
                         "started.")
            .tags({"synthetic"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_fx_feed(req, session); });
    const auto fx_built = fx_route.build();
    router->add_route(fx_built);
    registry->register_route(fx_built);

    auto ir_route = router->get("/api/synthetic/ir-curve-feeds/key/{key}")
                        .summary("Read one IR curve feed for a screen")
                        .description("The feed's row with its provider, whether it runs, and "
                                     "where its run started.")
                        .tags({"synthetic"})
                        .auth_required()
                        .handler([&session](const http_request& req) {
                            return handle_ir_curve_feed(req, session);
                        });
    const auto ir_built = ir_route.build();
    router->add_route(ir_built);
    registry->register_route(ir_built);
}

}
