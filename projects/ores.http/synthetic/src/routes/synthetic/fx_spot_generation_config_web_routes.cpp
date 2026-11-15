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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_http_route_web_records.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.http/routes/synthetic/fx_spot_generation_config_web_routes.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.synthetic.api/domain/fx_spot_generation_config_codec.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstdint>
#include <exception>
#include <optional>
#include <string>
#include <string_view>
#include <tuple>
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

/// How many rows a whole read asks for at a time.
constexpr std::uint32_t fx_spot_generation_config_web_page_size = 1000;

/// One row as the service sends it, with the version the screens read on every row.
std::string
fx_spot_generation_config_web_row(const ores::synthetic::domain::fx_spot_generation_config& row) {
    auto text = ores::utility::codec::json::write(row);
    return text;
}

std::string fx_spot_generation_config_web_rows(
    const std::vector<ores::synthetic::domain::fx_spot_generation_config>& rows) {
    std::string out = "[";
    for (std::size_t i = 0; i < rows.size(); ++i) {
        if (i != 0)
            out += ",";
        out += fx_spot_generation_config_web_row(rows[i]);
    }
    return out + "]";
}

/// A page request that states no order and no filter.
messaging::list_fx_spot_generation_configs_request
fx_spot_generation_config_web_page(std::uint32_t offset, std::uint32_t limit) {
    messaging::list_fx_spot_generation_configs_request msg;
    msg.offset = offset;
    msg.limit = limit;
    msg.order.field = "";
    msg.order.descending = false;
    return msg;
}

using fx_spot_generation_config_web_save_body =
    rr::save_body<messaging::fx_spot_generation_config_write>;

/**
 * @brief The key of one row that a removal names.
 *
 * A function, not a lambda in the handler: the handler is a coroutine, and g++
 * refuses a lambda kept in a coroutine frame (-Wsubobject-linkage).
 */
boost::uuids::uuid fx_spot_generation_config_web_key_of(const std::string& raw) {
    return rr::read<boost::uuids::uuid>(
        rr::code(raw, "A row is named by a code of 1 to 100 characters."), "key");
}

/**
 * @brief Why this session may not write the resource, or nothing when it may.
 *
 * A catalogue every tenant shares is written only from the system tenant, so a
 * tenant's write would not change every tenant's rows.
 */
std::optional<std::string>
fx_spot_generation_config_web_read_only_for([[maybe_unused]] const http_request& req) {
    return std::nullopt;
}

/// Refuse a write this session may not make.
void fx_spot_generation_config_web_require_writable(const http_request& req) {
    if (const auto because = fx_spot_generation_config_web_read_only_for(req))
        rr::refuse(http_status::forbidden,
                   "forbidden",
                   std::string("fx-feeds is read only here.") +
                       (because->empty() ? "" : " " + *because));
}

/// The body with the model's default for each write member it leaves out.
std::string fx_spot_generation_config_web_with_defaults(const std::string& body) {
    return rr::with_write_defaults(body,
                                   {
                                       rr::write_default{"price_source", R"json("fixed")json"},
                                       rr::write_default{"gmm_initial_price", R"json(0.0)json"},
                                       rr::write_default{"ticks_per_hour", R"json(0)json"},
                                       rr::write_default{"process_type", R"json("geometric")json"},
                                       rr::write_default{"enabled", R"json(false)json"},
                                       rr::write_default{"auto_start", R"json(false)json"},
                                       rr::write_default{"asset_class_code", R"json("fx")json"},
                                       rr::write_default{"arrival_mode", R"json("periodic")json"},
                                       rr::write_default{"branching_ratio", R"json(0.0)json"},
                                       rr::write_default{"decay", R"json(1.0)json"},
                                       rr::write_default{"sensitivity", R"json(1.0)json"},
                                       rr::write_default{"variance_model", R"json("constant")json"},
                                   });
}

}

std::string fx_spot_generation_config_web_routes::catalogue_entry(const http_request& req) {
    const auto because = fx_spot_generation_config_web_read_only_for(req);
    return std::string(
               R"json({"key":"fx-feeds","entityType":"ores.synthetic.fx_spot_generation_config","keyField":"id","writable":)json") +
           (because ? "false" : "true") + R"json(,"readOnlyBecause":)json" +
           (because ? ores::utility::codec::json::write(*because) : std::string("null")) +
           R"json(,"search":true,"sortable":["base_currency_code","quote_currency_code","source_name","oresmd_uri","ticks_per_hour","arrival_mode"],"readPermission":"synthetic::fx_spot_generation_configs:read","writePermission":"synthetic::fx_spot_generation_configs:write","deletePermission":"synthetic::fx_spot_generation_configs:delete"})json";
}

boost::asio::awaitable<http_response>
fx_spot_generation_config_web_routes::handle_list(const http_request& req, nats_client& session) {
    try {
        if (req.query_params.find("limit") == req.query_params.end()) {
            std::vector<ores::synthetic::domain::fx_spot_generation_config> rows;
            // A service may cap a page below the size asked for, so the read
            // advances by the rows that arrived and ends on an empty page, or on
            // a short one once it holds the total.
            for (std::uint32_t offset = 0;;) {
                const auto reply = ores::http::forwarding::forward(
                    req,
                    session,
                    fx_spot_generation_config_web_page(offset,
                                                       fx_spot_generation_config_web_page_size));
                rr::require_ok(reply.result);
                const auto& got = reply.fx_spot_generation_configs;
                rows.insert(rows.end(), got.begin(), got.end());
                offset += static_cast<std::uint32_t>(got.size());
                if (got.empty() || (got.size() < fx_spot_generation_config_web_page_size &&
                                    rows.size() >= reply.total))
                    break;
            }
            co_return http_response::json("{\"rows\":" + fx_spot_generation_config_web_rows(rows) +
                                          "}");
        }

        const std::string page_sentence = "A page names an offset and a limit of 1 to 1000.";
        const auto raw_offset = req.get_query_param("offset");
        std::uint32_t offset = 0;
        std::uint32_t limit = 0;
        try {
            offset = raw_offset.empty() ? 0 : boost::lexical_cast<std::uint32_t>(raw_offset);
            limit = boost::lexical_cast<std::uint32_t>(req.get_query_param("limit"));
        } catch (const std::exception&) {
            rr::invalid(page_sentence);
        }
        if (limit < 1 || limit > 1000)
            rr::invalid(page_sentence);

        auto msg = fx_spot_generation_config_web_page(offset, limit);
        const auto sort = req.get_query_param("sort");
        if (!sort.empty()) {
            constexpr std::string_view sortable[] = {"base_currency_code",
                                                     "quote_currency_code",
                                                     "source_name",
                                                     "oresmd_uri",
                                                     "ticks_per_hour",
                                                     "arrival_mode",
                                                     ""};
            if (std::find(std::begin(sortable), std::end(sortable) - 1, sort) ==
                std::end(sortable) - 1)
                rr::invalid("fx-feeds cannot be ordered by " + sort + ".");
            msg.order.field = sort;
        }
        const auto descending = req.get_query_param("descending");
        if (!descending.empty() && descending != "true" && descending != "false")
            rr::invalid(page_sentence);
        msg.order.descending = descending == "true";

        const auto search = req.get_query_param("search");
        if (!search.empty()) {
            msg.filter = messaging::fx_spot_generation_configs_filter{};
            msg.filter->search = search;
        }

        for (const auto& [name, value] : req.query_params) {
            if (!name.starts_with("f."))
                continue;
            const auto member = name.substr(2);
            constexpr std::string_view filters[] = {""};
            if (std::find(std::begin(filters), std::end(filters) - 1, member) ==
                std::end(filters) - 1)
                rr::invalid("fx-feeds cannot be filtered by " + member + ".");
        }

        const auto reply = ores::http::forwarding::forward(req, session, msg);
        rr::require_ok(reply.result);
        co_return http_response::json(
            "{\"rows\":" + fx_spot_generation_config_web_rows(reply.fx_spot_generation_configs) +
            ",\"total\":" + std::to_string(reply.total) + "}");
    } catch (const rr::refusal& refusal) {
        co_return rr::answer(refusal);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << e.what();
        co_return http_response::api_error(
            http_status::bad_gateway, "internal", std::string(rr::unanswered));
    }
}

boost::asio::awaitable<http_response>
fx_spot_generation_config_web_routes::handle_put(const http_request& req, nats_client& session) {
    try {
        fx_spot_generation_config_web_require_writable(req);
        const auto body = rr::read_body<fx_spot_generation_config_web_save_body>(
            fx_spot_generation_config_web_with_defaults(req.body));
        if (!body || body->changes.empty() || body->changes.size() > 1000 ||
            !rr::has_reason(body->intent.reason_code) || body->intent.commentary.size() > 2000)
            rr::invalid("A save names one to a thousand rows, each with the version it was read "
                        "at, and a reason.");
        messaging::put_many_fx_spot_generation_configs_request msg;
        std::vector<decltype(body->changes.front().write.id)> keys;
        for (const auto& change : body->changes) {
            auto& item = msg.changes.emplace_back();
            item.write = change.write;
            item.precondition.kind =
                change.version ? ores::utility::domain::precondition_kind::must_match_version :
                                 ores::utility::domain::precondition_kind::must_not_exist;
            item.precondition.version = change.version;
            keys.push_back(change.write.id);
        }
        std::sort(keys.begin(), keys.end());
        if (std::adjacent_find(keys.begin(), keys.end()) != keys.end())
            rr::invalid("A save names each row once.");
        msg.intent = body->intent;
        const auto reply = ores::http::forwarding::forward(req, session, msg);
        rr::require_ok(reply.result);
        co_return http_response::json(
            "{\"rows\":" + fx_spot_generation_config_web_rows(reply.fx_spot_generation_configs) +
            "}");
    } catch (const rr::refusal& refusal) {
        co_return rr::answer(refusal);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << e.what();
        co_return http_response::api_error(
            http_status::bad_gateway, "internal", std::string(rr::unanswered));
    }
}

boost::asio::awaitable<http_response>
fx_spot_generation_config_web_routes::handle_delete(const http_request& req, nats_client& session) {
    try {
        fx_spot_generation_config_web_require_writable(req);
        const auto body = rr::read_body<rr::remove_body>(req.body);
        if (!body || body->removals.empty() || body->removals.size() > 1000 ||
            !rr::has_reason(body->intent.reason_code) || body->intent.commentary.size() > 2000)
            rr::invalid("A removal names one to a thousand rows by id, and a reason.");
        std::vector<std::string> named;
        for (const auto& removal : body->removals)
            named.push_back(removal.key);
        std::sort(named.begin(), named.end());
        if (std::adjacent_find(named.begin(), named.end()) != named.end())
            rr::invalid("A removal names each row once.");
        if (body->removals.size() == 1) {
            const auto& removal = body->removals.front();
            messaging::delete_fx_spot_generation_config_request msg;
            msg.removal.key.id = fx_spot_generation_config_web_key_of(removal.key);
            msg.removal.precondition.kind =
                removal.version ? ores::utility::domain::precondition_kind::must_match_version :
                                  ores::utility::domain::precondition_kind::any;
            msg.removal.precondition.version = removal.version;
            msg.intent.reason_code = body->intent.reason_code;
            msg.intent.commentary = body->intent.commentary;
            const auto reply = ores::http::forwarding::forward(req, session, msg);
            rr::require_ok(reply.result);
            co_return rr::no_content();
        }
        for (const auto& removal : body->removals) {
            if (removal.version)
                rr::invalid("A removal of many rows is unconditional: it names no version. "
                            "Remove one row to name its version.");
        }
        messaging::delete_many_fx_spot_generation_configs_request msg;
        for (const auto& removal : body->removals) {
            auto& item = msg.removals.emplace_back();
            item.key.id = fx_spot_generation_config_web_key_of(removal.key);
            item.precondition.kind = ores::utility::domain::precondition_kind::any;
        }
        msg.intent = body->intent;
        const auto reply = ores::http::forwarding::forward(req, session, msg);
        rr::require_ok(reply.result);
        co_return rr::no_content();
    } catch (const rr::refusal& refusal) {
        co_return rr::answer(refusal);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << e.what();
        co_return http_response::api_error(
            http_status::bad_gateway, "internal", std::string(rr::unanswered));
    }
}

void fx_spot_generation_config_web_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    BOOST_LOG_SEV(lg(), info) << "Registering fx-feeds record routes";

    auto list_route =
        router->get("/api/synthetic/fx-feeds")
            .summary("List fx-feeds for a screen")
            .description("One page with its total, or every row when no limit is named.")
            .tags({"synthetic"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_list(req, session); });
    const auto list_built = list_route.build();
    router->add_route(list_built);
    registry->register_route(list_built);

    auto put_route =
        router->put("/api/synthetic/fx-feeds")
            .summary("Write one fx-feeds record for a screen")
            .description("A new row with no version, else a correction of the version read.")
            .tags({"synthetic"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_put(req, session); });
    const auto put_built = put_route.build();
    router->add_route(put_built);
    registry->register_route(put_built);

    auto remove_route =
        router->delete_("/api/synthetic/fx-feeds")
            .summary("Remove one fx-feeds record for a screen")
            .description("Removes the row the body's key names.")
            .tags({"synthetic"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_delete(req, session); });
    const auto remove_built = remove_route.build();
    router->add_route(remove_built);
    registry->register_route(remove_built);
}

}
