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
#include "ores.http/routes/refdata/currency_web_routes.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/session_expired_error.hpp"
#include "ores.nats/service/timeouts.hpp"
#include "ores.refdata.api/domain/currency_codec.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
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

namespace ores::http::routes::refdata {

using namespace logging;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace messaging = ores::refdata::messaging;
namespace rr = ores::http::net::record_routes;

namespace {

/// The unit's logger, for the detail a refusal does not show the browser.
auto& currency_web_lg() {
    static auto instance = make_logger("ores.http.routes.refdata.currency_web_routes");
    return instance;
}

/// How many rows a whole read asks for at a time.
constexpr std::uint32_t currency_web_page_size = 1000;

/**
 * @brief Forward one canonical request with the caller's token, and decode the reply.
 *
 * A refusal the service states in its reply header becomes the status it
 * names. An expired session is the caller's to renew.
 */
template <typename Request>
typename Request::response_type
currency_web_call(const http_request& req, nats_client& session, const Request& msg) {
    auto delegated = session.with_delegation(req.get_bearer_token().value_or(std::string{}));
    try {
        const auto reply =
            delegated.authenticated_request(msg.nats_subject,
                                            ores::nats::default_wire_codec().encode(msg),
                                            ores::nats::service::default_request_timeout);
        if (const auto it = reply.headers.find(std::string(ores::nats::headers::x_error));
            it != reply.headers.end()) {
            if (it->second == "forbidden")
                rr::refuse(http_status::forbidden, "forbidden", it->second);
            if (it->second == "bad_request")
                rr::invalid(it->second);
            if (it->second == "unauthorized" || it->second == "token_expired" ||
                it->second == "max_session_exceeded")
                rr::refuse(http_status::unauthorized, "not-authenticated", it->second);
            BOOST_LOG_SEV(currency_web_lg(), warn) << "Unknown service error: " << it->second;
            rr::refuse(
                http_status::bad_gateway, "upstream-unavailable", std::string(rr::unanswered));
        }
        auto decoded =
            ores::nats::default_wire_codec().decode<typename Request::response_type>(reply.data);
        if (!decoded) {
            BOOST_LOG_SEV(currency_web_lg(), error)
                << "Failed to decode response: " << decoded.error().what();
            rr::refuse(http_status::bad_gateway, "internal", std::string(rr::unanswered));
        }
        return std::move(*decoded);
    } catch (const ores::nats::service::session_expired_error& e) {
        rr::refuse(http_status::unauthorized, "session-expired", e.what());
    }
}

/// One row as the service sends it, with the version the screens read on every row.
std::string currency_web_row(const ores::refdata::domain::currency& row) {
    auto text = ores::utility::codec::json::write(row);
    return text;
}

std::string currency_web_rows(const std::vector<ores::refdata::domain::currency>& rows) {
    std::string out = "[";
    for (std::size_t i = 0; i < rows.size(); ++i) {
        if (i != 0)
            out += ",";
        out += currency_web_row(rows[i]);
    }
    return out + "]";
}

/// A page request that states no order and no filter.
messaging::list_currencies_request currency_web_page(std::uint32_t offset, std::uint32_t limit) {
    messaging::list_currencies_request msg;
    msg.offset = offset;
    msg.limit = limit;
    msg.order.field = "";
    msg.order.descending = false;
    return msg;
}

using currency_web_write_body = rr::write_body<messaging::currency_write>;
using currency_web_removal_body = rr::removal_body<messaging::currency_key>;

/**
 * @brief Why this session may not write the resource, or nothing when it may.
 *
 * A catalogue every tenant shares is written only from the system tenant, so a
 * tenant's write would not change every tenant's rows.
 */
std::optional<std::string> currency_web_read_only_for([[maybe_unused]] const http_request& req) {
    return std::nullopt;
}

/// Refuse a write this session may not make.
void currency_web_require_writable(const http_request& req) {
    if (const auto because = currency_web_read_only_for(req))
        rr::refuse(http_status::forbidden,
                   "forbidden",
                   std::string("currencies is read only here.") +
                       (because->empty() ? "" : " " + *because));
}

/// The body with the model's default for each write member it leaves out.
std::string currency_web_with_defaults(const std::string& body) {
    return rr::with_write_defaults(body,
                                   {
                                       rr::write_default{"fractions_per_unit", R"json(0)json"},
                                       rr::write_default{"rounding_precision", R"json(0)json"},
                                       rr::write_default{"spot_days", R"json(0)json"},
                                       rr::write_default{"base_precedence", R"json(0)json"},
                                   });
}

}

std::string currency_web_routes::catalogue_entry(const http_request& req) {
    const auto because = currency_web_read_only_for(req);
    return std::string(
               R"json({"key":"currencies","entityType":"ores.refdata.currency","keyFields":["iso_code"],"versioned":true,"writable":)json") +
           (because ? "false" : "true") +
           R"json(,"search":true,"sortable":["iso_code","name","monetary_nature","market_tier"],"events":{"component":"refdata","entity":"currencies"},"writePermission":"refdata::currencies:write","deletePermission":"refdata::currencies:delete"})json";
}

boost::asio::awaitable<http_response> currency_web_routes::handle_list(const http_request& req,
                                                                       nats_client& session) {
    try {
        if (req.query_params.find("limit") == req.query_params.end()) {
            std::vector<ores::refdata::domain::currency> rows;
            // A service may cap a page below the size asked for, so the read
            // advances by the rows that arrived and ends on an empty page, or on
            // a short one once it holds the total.
            for (std::uint32_t offset = 0;;) {
                const auto reply = currency_web_call(
                    req, session, currency_web_page(offset, currency_web_page_size));
                rr::require_ok(reply.result);
                const auto& got = reply.currencies;
                rows.insert(rows.end(), got.begin(), got.end());
                offset += static_cast<std::uint32_t>(got.size());
                if (got.empty() ||
                    (got.size() < currency_web_page_size && rows.size() >= reply.total))
                    break;
            }
            co_return http_response::json("{\"rows\":" + currency_web_rows(rows) + "}");
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

        auto msg = currency_web_page(offset, limit);
        const auto sort = req.get_query_param("sort");
        if (!sort.empty()) {
            constexpr std::string_view sortable[] = {
                "iso_code", "name", "monetary_nature", "market_tier", ""};
            if (std::find(std::begin(sortable), std::end(sortable) - 1, sort) ==
                std::end(sortable) - 1)
                rr::invalid("currencies cannot be ordered by " + sort + ".");
            msg.order.field = sort;
        }
        const auto descending = req.get_query_param("descending");
        if (!descending.empty() && descending != "true" && descending != "false")
            rr::invalid(page_sentence);
        msg.order.descending = descending == "true";

        const auto search = req.get_query_param("search");
        if (!search.empty()) {
            msg.filter = messaging::currencies_filter{};
            msg.filter->search = search;
        }

        for (const auto& [name, value] : req.query_params) {
            if (!name.starts_with("f."))
                continue;
            const auto member = name.substr(2);
            constexpr std::string_view filters[] = {""};
            if (std::find(std::begin(filters), std::end(filters) - 1, member) ==
                std::end(filters) - 1)
                rr::invalid("currencies cannot be filtered by " + member + ".");
        }

        const auto reply = currency_web_call(req, session, msg);
        rr::require_ok(reply.result);
        co_return http_response::json("{\"rows\":" + currency_web_rows(reply.currencies) +
                                      ",\"total\":" + std::to_string(reply.total) + "}");
    } catch (const rr::refusal& refusal) {
        co_return rr::answer(refusal);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << e.what();
        co_return http_response::api_error(
            http_status::bad_gateway, "internal", std::string(rr::unanswered));
    }
}

boost::asio::awaitable<http_response> currency_web_routes::handle_key(const http_request& req,
                                                                      nats_client& session) {
    try {
        const auto key = rr::code(req.get_path_param("key"),
                                  "A record is named by a code of 1 to 100 characters.");
        auto msg = currency_web_page(0, 1);
        msg.filter = messaging::currencies_filter{};
        msg.filter->iso_code_one_of = std::vector<std::string>{rr::read<std::string>(key, "key")};
        const auto reply = currency_web_call(req, session, msg);
        rr::require_ok(reply.result);
        if (reply.currencies.empty())
            rr::refuse(
                http_status::not_found, "not-found", "There is no currencies record " + key + ".");
        co_return http_response::json("{\"row\":" + currency_web_row(reply.currencies.front()) +
                                      "}");
    } catch (const rr::refusal& refusal) {
        co_return rr::answer(refusal);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << e.what();
        co_return http_response::api_error(
            http_status::bad_gateway, "internal", std::string(rr::unanswered));
    }
}

boost::asio::awaitable<http_response> currency_web_routes::handle_put(const http_request& req,
                                                                      nats_client& session) {
    try {
        currency_web_require_writable(req);
        const auto body =
            rr::read_body<currency_web_write_body>(currency_web_with_defaults(req.body));
        if (!body || !rr::has_reason(body->reasonCode) || body->commentary.size() > 2000)
            rr::invalid("A write needs the row, the version it was read at, and a reason.");
        messaging::put_currency_request msg;
        msg.change.write = body->write;
        msg.change.precondition.kind =
            body->version ? ores::utility::domain::precondition_kind::must_match_version :
                            ores::utility::domain::precondition_kind::must_not_exist;
        msg.change.precondition.version = body->version;
        msg.intent.reason_code = body->reasonCode;
        msg.intent.commentary = body->commentary;
        const auto reply = currency_web_call(req, session, msg);
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

boost::asio::awaitable<http_response> currency_web_routes::handle_delete(const http_request& req,
                                                                         nats_client& session) {
    try {
        currency_web_require_writable(req);
        const auto body = rr::read_body<currency_web_removal_body>(req.body);
        if (!body || !rr::has_reason(body->reasonCode) || body->commentary.size() > 2000)
            rr::invalid("A removal names the row by iso_code, and a reason.");
        messaging::delete_currency_request msg;
        msg.removal.key = body->key;
        msg.removal.precondition.kind =
            body->version ? ores::utility::domain::precondition_kind::must_match_version :
                            ores::utility::domain::precondition_kind::any;
        msg.removal.precondition.version = body->version;
        msg.intent.reason_code = body->reasonCode;
        msg.intent.commentary = body->commentary;
        const auto reply = currency_web_call(req, session, msg);
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

void currency_web_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    BOOST_LOG_SEV(lg(), info) << "Registering currencies record routes";

    auto list_route =
        router->get("/api/refdata/currencies")
            .summary("List currencies for a screen")
            .description("One page with its total, or every row when no limit is named.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_list(req, session); });
    const auto list_built = list_route.build();
    router->add_route(list_built);
    registry->register_route(list_built);

    auto key_route =
        router->get("/api/refdata/currencies/key/{key}")
            .summary("Read one currencies record for a screen")
            .description("One row, named by its key.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_key(req, session); });
    const auto key_built = key_route.build();
    router->add_route(key_built);
    registry->register_route(key_built);

    auto put_route =
        router->put("/api/refdata/currencies")
            .summary("Write one currencies record for a screen")
            .description("A new row with no version, else a correction of the version read.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_put(req, session); });
    const auto put_built = put_route.build();
    router->add_route(put_built);
    registry->register_route(put_built);

    auto remove_route =
        router->delete_("/api/refdata/currencies")
            .summary("Remove one currencies record for a screen")
            .description("Removes the row the body's key names.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_delete(req, session); });
    const auto remove_built = remove_route.build();
    router->add_route(remove_built);
    registry->register_route(remove_built);
}

}
