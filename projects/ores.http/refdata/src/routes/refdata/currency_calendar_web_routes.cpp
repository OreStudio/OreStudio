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
#include "ores.http/routes/refdata/currency_calendar_web_routes.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/session_expired_error.hpp"
#include "ores.nats/service/timeouts.hpp"
#include "ores.refdata.api/domain/currency_calendar_codec.hpp"
#include "ores.refdata.api/messaging/currency_calendar_protocol.hpp"
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
auto& currency_calendar_web_lg() {
    static auto instance = make_logger("ores.http.routes.refdata.currency_calendar_web_routes");
    return instance;
}

/// How many rows a whole read asks for at a time.
constexpr std::uint32_t currency_calendar_web_page_size = 1000;

/**
 * @brief Forward one canonical request with the caller's token, and decode the reply.
 *
 * A refusal the service states in its reply header becomes the status it
 * names. An expired session is the caller's to renew.
 */
template <typename Request>
typename Request::response_type
currency_calendar_web_call(const http_request& req, nats_client& session, const Request& msg) {
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
            BOOST_LOG_SEV(currency_calendar_web_lg(), warn)
                << "Unknown service error: " << it->second;
            rr::refuse(
                http_status::bad_gateway, "upstream-unavailable", std::string(rr::unanswered));
        }
        auto decoded =
            ores::nats::default_wire_codec().decode<typename Request::response_type>(reply.data);
        if (!decoded) {
            BOOST_LOG_SEV(currency_calendar_web_lg(), error)
                << "Failed to decode response: " << decoded.error().what();
            rr::refuse(http_status::bad_gateway, "internal", std::string(rr::unanswered));
        }
        return std::move(*decoded);
    } catch (const ores::nats::service::session_expired_error& e) {
        rr::refuse(http_status::unauthorized, "session-expired", e.what());
    }
}

/// One row as the service sends it, with the version the screens read on every row.
std::string currency_calendar_web_row(const ores::refdata::domain::currency_calendar& row) {
    auto text = ores::utility::codec::json::write(row);
    text.insert(1, text.size() > 2 ? "\"version\":0," : "\"version\":0");
    return text;
}

std::string
currency_calendar_web_rows(const std::vector<ores::refdata::domain::currency_calendar>& rows) {
    std::string out = "[";
    for (std::size_t i = 0; i < rows.size(); ++i) {
        if (i != 0)
            out += ",";
        out += currency_calendar_web_row(rows[i]);
    }
    return out + "]";
}

/// A page request that states no order and no filter.
messaging::list_currency_calendars_request currency_calendar_web_page(std::uint32_t offset,
                                                                      std::uint32_t limit) {
    messaging::list_currency_calendars_request msg;
    msg.offset = offset;
    msg.limit = limit;
    msg.order.field = "";
    msg.order.descending = false;
    return msg;
}

using currency_calendar_web_write_body = rr::write_body<messaging::currency_calendar_write>;
using currency_calendar_web_removal_body = rr::removal_body<messaging::currency_calendar_key>;

/**
 * @brief Why this session may not write the resource, or nothing when it may.
 *
 * A catalogue every tenant shares is written only from the system tenant, so a
 * tenant's write would not change every tenant's rows.
 */
std::optional<std::string>
currency_calendar_web_read_only_for([[maybe_unused]] const http_request& req) {
    return std::nullopt;
}

/// Refuse a write this session may not make.
void currency_calendar_web_require_writable(const http_request& req) {
    if (const auto because = currency_calendar_web_read_only_for(req))
        rr::refuse(http_status::forbidden,
                   "forbidden",
                   std::string("currency-calendars is read only here.") +
                       (because->empty() ? "" : " " + *because));
}

/// The body with the model's default for each write member it leaves out.
std::string currency_calendar_web_with_defaults(const std::string& body) {
    return body;
}

}

std::string currency_calendar_web_routes::catalogue_entry(const http_request& req) {
    const auto because = currency_calendar_web_read_only_for(req);
    return std::string(
               R"json({"key":"currency-calendars","entityType":"ores.refdata.currency_calendar","keyFields":["currency_iso_code","calendar_code"],"versioned":false,"writable":)json") +
           (because ? "false" : "true") +
           R"json(,"search":false,"sortable":[],"events":{"component":"refdata","entity":"currency_calendars"},"writePermission":"refdata::currency_calendars:write","deletePermission":"refdata::currency_calendars:delete"})json";
}

boost::asio::awaitable<http_response>
currency_calendar_web_routes::handle_list(const http_request& req, nats_client& session) {
    try {
        if (req.query_params.find("limit") == req.query_params.end()) {
            std::vector<ores::refdata::domain::currency_calendar> rows;
            // A service may cap a page below the size asked for, so the read
            // advances by the rows that arrived and ends on an empty page, or on
            // a short one once it holds the total.
            for (std::uint32_t offset = 0;;) {
                const auto reply = currency_calendar_web_call(
                    req,
                    session,
                    currency_calendar_web_page(offset, currency_calendar_web_page_size));
                rr::require_ok(reply.result);
                const auto& got = reply.currency_calendars;
                rows.insert(rows.end(), got.begin(), got.end());
                offset += static_cast<std::uint32_t>(got.size());
                if (got.empty() ||
                    (got.size() < currency_calendar_web_page_size && rows.size() >= reply.total))
                    break;
            }
            co_return http_response::json("{\"rows\":" + currency_calendar_web_rows(rows) + "}");
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

        auto msg = currency_calendar_web_page(offset, limit);
        const auto sort = req.get_query_param("sort");
        if (!sort.empty()) {
            constexpr std::string_view sortable[] = {""};
            if (std::find(std::begin(sortable), std::end(sortable) - 1, sort) ==
                std::end(sortable) - 1)
                rr::invalid("currency-calendars cannot be ordered by " + sort + ".");
            msg.order.field = sort;
        }
        const auto descending = req.get_query_param("descending");
        if (!descending.empty() && descending != "true" && descending != "false")
            rr::invalid(page_sentence);
        msg.order.descending = descending == "true";

        const auto search = req.get_query_param("search");
        if (!search.empty())
            rr::invalid("currency-calendars cannot be searched.");

        for (const auto& [name, value] : req.query_params) {
            if (!name.starts_with("f."))
                continue;
            const auto member = name.substr(2);
            constexpr std::string_view filters[] = {"currency_iso_code", ""};
            if (std::find(std::begin(filters), std::end(filters) - 1, member) ==
                std::end(filters) - 1)
                rr::invalid("currency-calendars cannot be filtered by " + member + ".");
        }
        if (const auto raw = req.get_query_param("f.currency_iso_code"); !raw.empty()) {
            if (!msg.filter)
                msg.filter = messaging::currency_calendars_filter{};
            msg.filter->currency_iso_code = rr::read<std::string>(raw, "f.currency_iso_code");
        }

        const auto reply = currency_calendar_web_call(req, session, msg);
        rr::require_ok(reply.result);
        co_return http_response::json(
            "{\"rows\":" + currency_calendar_web_rows(reply.currency_calendars) +
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
currency_calendar_web_routes::handle_by(const http_request& req, nats_client& session) {
    try {
        const auto parent = rr::code(req.get_path_param("parent"),
                                     "A parent is named by a code of 1 to 100 characters.");
        std::vector<ores::refdata::domain::currency_calendar> rows;
        for (std::uint32_t offset = 0;;) {
            messaging::list_by_currency_iso_code_currency_calendars_request msg;
            msg.currency_iso_code = rr::read<std::string>(parent, "parent");
            msg.scope = ores::utility::domain::scope::direct;
            msg.offset = offset;
            msg.limit = currency_calendar_web_page_size;
            msg.order.field = "";
            msg.order.descending = false;
            const auto reply = currency_calendar_web_call(req, session, msg);
            rr::require_ok(reply.result);
            const auto& got = reply.currency_calendars;
            rows.insert(rows.end(), got.begin(), got.end());
            offset += static_cast<std::uint32_t>(got.size());
            if (got.empty() ||
                (got.size() < currency_calendar_web_page_size && rows.size() >= reply.total))
                break;
        }
        co_return http_response::json("{\"rows\":" + currency_calendar_web_rows(rows) + "}");
    } catch (const rr::refusal& refusal) {
        co_return rr::answer(refusal);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << e.what();
        co_return http_response::api_error(
            http_status::bad_gateway, "internal", std::string(rr::unanswered));
    }
}

boost::asio::awaitable<http_response>
currency_calendar_web_routes::handle_put(const http_request& req, nats_client& session) {
    try {
        currency_calendar_web_require_writable(req);
        const auto body = rr::read_body<currency_calendar_web_write_body>(
            currency_calendar_web_with_defaults(req.body));
        if (!body || !rr::has_reason(body->reasonCode) || body->commentary.size() > 2000)
            rr::invalid("A write needs the row, the version it was read at, and a reason.");
        messaging::put_currency_calendar_request msg;
        msg.change.write = body->write;
        msg.change.precondition.kind =
            body->version ? ores::utility::domain::precondition_kind::must_match_version :
                            ores::utility::domain::precondition_kind::must_not_exist;
        msg.change.precondition.version = body->version;
        msg.intent.reason_code = body->reasonCode;
        msg.intent.commentary = body->commentary;
        const auto reply = currency_calendar_web_call(req, session, msg);
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

boost::asio::awaitable<http_response>
currency_calendar_web_routes::handle_delete(const http_request& req, nats_client& session) {
    try {
        currency_calendar_web_require_writable(req);
        const auto body = rr::read_body<currency_calendar_web_removal_body>(req.body);
        if (!body || !rr::has_reason(body->reasonCode) || body->commentary.size() > 2000)
            rr::invalid(
                "A removal names the row by currency_iso_code and calendar_code, and a reason.");
        messaging::delete_currency_calendar_request msg;
        msg.removal.key = body->key;
        msg.removal.precondition.kind =
            body->version ? ores::utility::domain::precondition_kind::must_match_version :
                            ores::utility::domain::precondition_kind::any;
        msg.removal.precondition.version = body->version;
        msg.intent.reason_code = body->reasonCode;
        msg.intent.commentary = body->commentary;
        const auto reply = currency_calendar_web_call(req, session, msg);
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

void currency_calendar_web_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    BOOST_LOG_SEV(lg(), info) << "Registering currency-calendars record routes";

    auto list_route =
        router->get("/api/refdata/currency-calendars")
            .summary("List currency-calendars for a screen")
            .description("One page with its total, or every row when no limit is named.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_list(req, session); });
    const auto list_built = list_route.build();
    router->add_route(list_built);
    registry->register_route(list_built);

    auto by_route =
        router->get("/api/refdata/currency-calendars/by/{parent}")
            .summary("List one parent's currency-calendars for a screen")
            .description("Every row of one parent.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_by(req, session); });
    const auto by_built = by_route.build();
    router->add_route(by_built);
    registry->register_route(by_built);

    auto put_route =
        router->put("/api/refdata/currency-calendars")
            .summary("Write one currency-calendars record for a screen")
            .description("A new row with no version, else a correction of the version read.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_put(req, session); });
    const auto put_built = put_route.build();
    router->add_route(put_built);
    registry->register_route(put_built);

    auto remove_route =
        router->delete_("/api/refdata/currency-calendars")
            .summary("Remove one currency-calendars record for a screen")
            .description("Removes the row the body's key names.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_delete(req, session); });
    const auto remove_built = remove_route.build();
    router->add_route(remove_built);
    registry->register_route(remove_built);
}

}
