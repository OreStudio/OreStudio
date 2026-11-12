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
#include "ores.http/routes/synthetic/fx_vol_surface_feed_contributor_web_routes.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.synthetic.api/domain/fx_vol_surface_feed_contributor_codec.hpp"
#include "ores.synthetic.api/messaging/fx_vol_surface_feed_contributor_protocol.hpp"
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
constexpr std::uint32_t fx_vol_surface_feed_contributor_web_page_size = 1000;

/// One row as the service sends it, with the version the screens read on every row.
std::string fx_vol_surface_feed_contributor_web_row(
    const ores::synthetic::domain::fx_vol_surface_feed_contributor& row) {
    auto text = ores::utility::codec::json::write(row);
    return text;
}

std::string fx_vol_surface_feed_contributor_web_rows(
    const std::vector<ores::synthetic::domain::fx_vol_surface_feed_contributor>& rows) {
    std::string out = "[";
    for (std::size_t i = 0; i < rows.size(); ++i) {
        if (i != 0)
            out += ",";
        out += fx_vol_surface_feed_contributor_web_row(rows[i]);
    }
    return out + "]";
}

/// A page request that states no order and no filter.
messaging::list_fx_vol_surface_feed_contributors_request
fx_vol_surface_feed_contributor_web_page(std::uint32_t offset, std::uint32_t limit) {
    messaging::list_fx_vol_surface_feed_contributors_request msg;
    msg.offset = offset;
    msg.limit = limit;
    msg.order.field = "";
    msg.order.descending = false;
    return msg;
}

using fx_vol_surface_feed_contributor_web_save_body =
    rr::save_body<messaging::fx_vol_surface_feed_contributor_write>;

/**
 * @brief The key of one row that a removal names.
 *
 * A function, not a lambda in the handler: the handler is a coroutine, and g++
 * refuses a lambda kept in a coroutine frame (-Wsubobject-linkage).
 */
boost::uuids::uuid fx_vol_surface_feed_contributor_web_key_of(const std::string& raw) {
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
fx_vol_surface_feed_contributor_web_read_only_for([[maybe_unused]] const http_request& req) {
    const auto& user = req.authenticated_user;
    if (!user || !user->tenant_id || *user->tenant_id != rr::system_tenant_id)
        return std::string(
            "Every tenant shares this catalogue, so only the system tenant changes it.");
    return std::nullopt;
}

/// Refuse a write this session may not make.
void fx_vol_surface_feed_contributor_web_require_writable(const http_request& req) {
    if (const auto because = fx_vol_surface_feed_contributor_web_read_only_for(req))
        rr::refuse(http_status::forbidden,
                   "forbidden",
                   std::string("fx-vol-surface-feed-contributors is read only here.") +
                       (because->empty() ? "" : " " + *because));
}

/// The body with the model's default for each write member it leaves out.
std::string fx_vol_surface_feed_contributor_web_with_defaults(const std::string& body) {
    return body;
}

}

std::string fx_vol_surface_feed_contributor_web_routes::catalogue_entry(const http_request& req) {
    const auto because = fx_vol_surface_feed_contributor_web_read_only_for(req);
    return std::string(
               R"json({"key":"fx-vol-surface-feed-contributors","entityType":"ores.synthetic.fx_vol_surface_feed_contributor","keyField":"id","writable":)json") +
           (because ? "false" : "true") + R"json(,"readOnlyBecause":)json" +
           (because ? ores::utility::codec::json::write(*because) : std::string("null")) +
           R"json(,"search":false,"sortable":[],"readPermission":"synthetic::fx_vol_surface_feed_contributors:read","writePermission":"synthetic::fx_vol_surface_feed_contributors:write","deletePermission":"synthetic::fx_vol_surface_feed_contributors:delete"})json";
}

boost::asio::awaitable<http_response>
fx_vol_surface_feed_contributor_web_routes::handle_list(const http_request& req,
                                                        nats_client& session) {
    try {
        if (req.query_params.find("limit") == req.query_params.end()) {
            std::vector<ores::synthetic::domain::fx_vol_surface_feed_contributor> rows;
            // A service may cap a page below the size asked for, so the read
            // advances by the rows that arrived and ends on an empty page, or on
            // a short one once it holds the total.
            for (std::uint32_t offset = 0;;) {
                const auto reply = ores::http::forwarding::forward(
                    req,
                    session,
                    fx_vol_surface_feed_contributor_web_page(
                        offset, fx_vol_surface_feed_contributor_web_page_size));
                rr::require_ok(reply.result);
                const auto& got = reply.fx_vol_surface_feed_contributors;
                rows.insert(rows.end(), got.begin(), got.end());
                offset += static_cast<std::uint32_t>(got.size());
                if (got.empty() || (got.size() < fx_vol_surface_feed_contributor_web_page_size &&
                                    rows.size() >= reply.total))
                    break;
            }
            co_return http_response::json(
                "{\"rows\":" + fx_vol_surface_feed_contributor_web_rows(rows) + "}");
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

        auto msg = fx_vol_surface_feed_contributor_web_page(offset, limit);
        const auto sort = req.get_query_param("sort");
        if (!sort.empty()) {
            constexpr std::string_view sortable[] = {""};
            if (std::find(std::begin(sortable), std::end(sortable) - 1, sort) ==
                std::end(sortable) - 1)
                rr::invalid("fx-vol-surface-feed-contributors cannot be ordered by " + sort + ".");
            msg.order.field = sort;
        }
        const auto descending = req.get_query_param("descending");
        if (!descending.empty() && descending != "true" && descending != "false")
            rr::invalid(page_sentence);
        msg.order.descending = descending == "true";

        const auto search = req.get_query_param("search");
        if (!search.empty())
            rr::invalid("fx-vol-surface-feed-contributors cannot be searched.");

        for (const auto& [name, value] : req.query_params) {
            if (!name.starts_with("f."))
                continue;
            const auto member = name.substr(2);
            constexpr std::string_view filters[] = {"feed_id", "contributor_id", ""};
            if (std::find(std::begin(filters), std::end(filters) - 1, member) ==
                std::end(filters) - 1)
                rr::invalid("fx-vol-surface-feed-contributors cannot be filtered by " + member +
                            ".");
        }
        if (const auto raw = req.get_query_param("f.feed_id"); !raw.empty()) {
            if (!msg.filter)
                msg.filter = messaging::fx_vol_surface_feed_contributors_filter{};
            msg.filter->feed_id = rr::read<boost::uuids::uuid>(raw, "f.feed_id");
        }
        if (const auto raw = req.get_query_param("f.contributor_id"); !raw.empty()) {
            if (!msg.filter)
                msg.filter = messaging::fx_vol_surface_feed_contributors_filter{};
            msg.filter->contributor_id = rr::read<boost::uuids::uuid>(raw, "f.contributor_id");
        }

        const auto reply = ores::http::forwarding::forward(req, session, msg);
        rr::require_ok(reply.result);
        co_return http_response::json(
            "{\"rows\":" +
            fx_vol_surface_feed_contributor_web_rows(reply.fx_vol_surface_feed_contributors) +
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
fx_vol_surface_feed_contributor_web_routes::handle_key(const http_request& req,
                                                       nats_client& session) {
    try {
        const auto key = rr::code(req.get_path_param("key"),
                                  "A record is named by a code of 1 to 100 characters.");
        auto msg = fx_vol_surface_feed_contributor_web_page(0, 1);
        msg.filter = messaging::fx_vol_surface_feed_contributors_filter{};
        msg.filter->id_one_of =
            std::vector<boost::uuids::uuid>{rr::read<boost::uuids::uuid>(key, "key")};
        const auto reply = ores::http::forwarding::forward(req, session, msg);
        rr::require_ok(reply.result);
        if (reply.fx_vol_surface_feed_contributors.empty())
            rr::refuse(http_status::not_found,
                       "not-found",
                       "There is no fx-vol-surface-feed-contributors row " + key + ".");
        co_return http_response::json("{\"row\":" +
                                      fx_vol_surface_feed_contributor_web_row(
                                          reply.fx_vol_surface_feed_contributors.front()) +
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
fx_vol_surface_feed_contributor_web_routes::handle_by(const http_request& req,
                                                      nats_client& session) {
    try {
        const auto parent = rr::code(req.get_path_param("parent"),
                                     "A parent is named by a code of 1 to 100 characters.");
        std::vector<ores::synthetic::domain::fx_vol_surface_feed_contributor> rows;
        for (std::uint32_t offset = 0;;) {
            auto msg = fx_vol_surface_feed_contributor_web_page(
                offset, fx_vol_surface_feed_contributor_web_page_size);
            msg.filter = messaging::fx_vol_surface_feed_contributors_filter{};
            msg.filter->feed_id = rr::read<boost::uuids::uuid>(parent, "parent");
            const auto reply = ores::http::forwarding::forward(req, session, msg);
            rr::require_ok(reply.result);
            const auto& got = reply.fx_vol_surface_feed_contributors;
            rows.insert(rows.end(), got.begin(), got.end());
            offset += static_cast<std::uint32_t>(got.size());
            if (got.empty() || (got.size() < fx_vol_surface_feed_contributor_web_page_size &&
                                rows.size() >= reply.total))
                break;
        }
        co_return http_response::json(
            "{\"rows\":" + fx_vol_surface_feed_contributor_web_rows(rows) + "}");
    } catch (const rr::refusal& refusal) {
        co_return rr::answer(refusal);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << e.what();
        co_return http_response::api_error(
            http_status::bad_gateway, "internal", std::string(rr::unanswered));
    }
}

boost::asio::awaitable<http_response>
fx_vol_surface_feed_contributor_web_routes::handle_put(const http_request& req,
                                                       nats_client& session) {
    try {
        fx_vol_surface_feed_contributor_web_require_writable(req);
        const auto body = rr::read_body<fx_vol_surface_feed_contributor_web_save_body>(
            fx_vol_surface_feed_contributor_web_with_defaults(req.body));
        if (!body || body->changes.empty() || body->changes.size() > 1000 ||
            !rr::has_reason(body->intent.reason_code) || body->intent.commentary.size() > 2000)
            rr::invalid("A save names one to a thousand rows, each with the version it was read "
                        "at, and a reason.");
        messaging::put_many_fx_vol_surface_feed_contributors_request msg;
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
            "{\"rows\":" +
            fx_vol_surface_feed_contributor_web_rows(reply.fx_vol_surface_feed_contributors) + "}");
    } catch (const rr::refusal& refusal) {
        co_return rr::answer(refusal);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << e.what();
        co_return http_response::api_error(
            http_status::bad_gateway, "internal", std::string(rr::unanswered));
    }
}

boost::asio::awaitable<http_response>
fx_vol_surface_feed_contributor_web_routes::handle_delete(const http_request& req,
                                                          nats_client& session) {
    try {
        fx_vol_surface_feed_contributor_web_require_writable(req);
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
            messaging::delete_fx_vol_surface_feed_contributor_request msg;
            msg.removal.key.id = fx_vol_surface_feed_contributor_web_key_of(removal.key);
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
        messaging::delete_many_fx_vol_surface_feed_contributors_request msg;
        for (const auto& removal : body->removals) {
            auto& item = msg.removals.emplace_back();
            item.key.id = fx_vol_surface_feed_contributor_web_key_of(removal.key);
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

void fx_vol_surface_feed_contributor_web_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    BOOST_LOG_SEV(lg(), info) << "Registering fx-vol-surface-feed-contributors record routes";

    auto list_route =
        router->get("/api/synthetic/fx-vol-surface-feed-contributors")
            .summary("List fx-vol-surface-feed-contributors for a screen")
            .description("One page with its total, or every row when no limit is named.")
            .tags({"synthetic"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_list(req, session); });
    const auto list_built = list_route.build();
    router->add_route(list_built);
    registry->register_route(list_built);

    auto key_route =
        router->get("/api/synthetic/fx-vol-surface-feed-contributors/key/{key}")
            .summary("Read one fx-vol-surface-feed-contributors record for a screen")
            .description("One row, named by its key.")
            .tags({"synthetic"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_key(req, session); });
    const auto key_built = key_route.build();
    router->add_route(key_built);
    registry->register_route(key_built);

    auto by_route =
        router->get("/api/synthetic/fx-vol-surface-feed-contributors/by/{parent}")
            .summary("List one parent's fx-vol-surface-feed-contributors for a screen")
            .description("Every row of one parent.")
            .tags({"synthetic"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_by(req, session); });
    const auto by_built = by_route.build();
    router->add_route(by_built);
    registry->register_route(by_built);

    auto put_route =
        router->put("/api/synthetic/fx-vol-surface-feed-contributors")
            .summary("Write one fx-vol-surface-feed-contributors record for a screen")
            .description("A new row with no version, else a correction of the version read.")
            .tags({"synthetic"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_put(req, session); });
    const auto put_built = put_route.build();
    router->add_route(put_built);
    registry->register_route(put_built);

    auto remove_route =
        router->delete_("/api/synthetic/fx-vol-surface-feed-contributors")
            .summary("Remove one fx-vol-surface-feed-contributors record for a screen")
            .description("Removes the row the body's key names.")
            .tags({"synthetic"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_delete(req, session); });
    const auto remove_built = remove_route.build();
    router->add_route(remove_built);
    registry->register_route(remove_built);
}

}
