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
#include "ores.http/routes/refdata/party_counterparty_web_routes.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.refdata.api/domain/party_counterparty_codec.hpp"
#include "ores.refdata.api/messaging/party_counterparty_protocol.hpp"
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

/// How many rows a whole read asks for at a time.
constexpr std::uint32_t party_counterparty_web_page_size = 1000;

/// One row as the service sends it, with the version the screens read on every row.
std::string party_counterparty_web_row(const ores::refdata::domain::party_counterparty& row) {
    auto text = ores::utility::codec::json::write(row);
    text.insert(1, text.size() > 2 ? "\"version\":0," : "\"version\":0");
    return text;
}

std::string
party_counterparty_web_rows(const std::vector<ores::refdata::domain::party_counterparty>& rows) {
    std::string out = "[";
    for (std::size_t i = 0; i < rows.size(); ++i) {
        if (i != 0)
            out += ",";
        out += party_counterparty_web_row(rows[i]);
    }
    return out + "]";
}

/// A page request that states no order and no filter.
messaging::list_party_counterparties_request party_counterparty_web_page(std::uint32_t offset,
                                                                         std::uint32_t limit) {
    messaging::list_party_counterparties_request msg;
    msg.offset = offset;
    msg.limit = limit;
    msg.order.field = "";
    msg.order.descending = false;
    return msg;
}

using party_counterparty_web_write_body = rr::write_body<messaging::party_counterparty_write>;
using party_counterparty_web_removal_body = rr::removal_body<messaging::party_counterparty_key>;

/**
 * @brief Why this session may not write the resource, or nothing when it may.
 *
 * A catalogue every tenant shares is written only from the system tenant, so a
 * tenant's write would not change every tenant's rows.
 */
std::optional<std::string>
party_counterparty_web_read_only_for([[maybe_unused]] const http_request& req) {
    return std::nullopt;
}

/// Refuse a write this session may not make.
void party_counterparty_web_require_writable(const http_request& req) {
    if (const auto because = party_counterparty_web_read_only_for(req))
        rr::refuse(http_status::forbidden,
                   "forbidden",
                   std::string("party-counterparties is read only here.") +
                       (because->empty() ? "" : " " + *because));
}

/// The body with the model's default for each write member it leaves out.
std::string party_counterparty_web_with_defaults(const std::string& body) {
    return body;
}

}

std::string party_counterparty_web_routes::catalogue_entry(const http_request& req) {
    const auto because = party_counterparty_web_read_only_for(req);
    return std::string(
               R"json({"key":"party-counterparties","entityType":"ores.refdata.party_counterparty","keyFields":["party_id","counterparty_id"],"versioned":false,"writable":)json") +
           (because ? "false" : "true") +
           R"json(,"search":false,"sortable":[],"events":{"component":"refdata","entity":"party_counterparties"},"writePermission":"refdata::party_counterparties:write","deletePermission":"refdata::party_counterparties:delete"})json";
}

boost::asio::awaitable<http_response>
party_counterparty_web_routes::handle_list(const http_request& req, nats_client& session) {
    try {
        if (req.query_params.find("limit") == req.query_params.end()) {
            std::vector<ores::refdata::domain::party_counterparty> rows;
            // A service may cap a page below the size asked for, so the read
            // advances by the rows that arrived and ends on an empty page, or on
            // a short one once it holds the total.
            for (std::uint32_t offset = 0;;) {
                const auto reply = ores::http::forwarding::forward(
                    req,
                    session,
                    party_counterparty_web_page(offset, party_counterparty_web_page_size));
                rr::require_ok(reply.result);
                const auto& got = reply.party_counterparties;
                rows.insert(rows.end(), got.begin(), got.end());
                offset += static_cast<std::uint32_t>(got.size());
                if (got.empty() ||
                    (got.size() < party_counterparty_web_page_size && rows.size() >= reply.total))
                    break;
            }
            co_return http_response::json("{\"rows\":" + party_counterparty_web_rows(rows) + "}");
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

        auto msg = party_counterparty_web_page(offset, limit);
        const auto sort = req.get_query_param("sort");
        if (!sort.empty()) {
            constexpr std::string_view sortable[] = {""};
            if (std::find(std::begin(sortable), std::end(sortable) - 1, sort) ==
                std::end(sortable) - 1)
                rr::invalid("party-counterparties cannot be ordered by " + sort + ".");
            msg.order.field = sort;
        }
        const auto descending = req.get_query_param("descending");
        if (!descending.empty() && descending != "true" && descending != "false")
            rr::invalid(page_sentence);
        msg.order.descending = descending == "true";

        const auto search = req.get_query_param("search");
        if (!search.empty())
            rr::invalid("party-counterparties cannot be searched.");

        for (const auto& [name, value] : req.query_params) {
            if (!name.starts_with("f."))
                continue;
            const auto member = name.substr(2);
            constexpr std::string_view filters[] = {"party_id", "counterparty_id", ""};
            if (std::find(std::begin(filters), std::end(filters) - 1, member) ==
                std::end(filters) - 1)
                rr::invalid("party-counterparties cannot be filtered by " + member + ".");
        }
        if (const auto raw = req.get_query_param("f.party_id"); !raw.empty()) {
            if (!msg.filter)
                msg.filter = messaging::party_counterparties_filter{};
            msg.filter->party_id = rr::read<boost::uuids::uuid>(raw, "f.party_id");
        }
        if (const auto raw = req.get_query_param("f.counterparty_id"); !raw.empty()) {
            if (!msg.filter)
                msg.filter = messaging::party_counterparties_filter{};
            msg.filter->counterparty_id = rr::read<boost::uuids::uuid>(raw, "f.counterparty_id");
        }

        const auto reply = ores::http::forwarding::forward(req, session, msg);
        rr::require_ok(reply.result);
        co_return http_response::json(
            "{\"rows\":" + party_counterparty_web_rows(reply.party_counterparties) +
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
party_counterparty_web_routes::handle_by(const http_request& req, nats_client& session) {
    try {
        const auto parent = rr::code(req.get_path_param("parent"),
                                     "A parent is named by a code of 1 to 100 characters.");
        std::vector<ores::refdata::domain::party_counterparty> rows;
        for (std::uint32_t offset = 0;;) {
            messaging::list_by_party_id_party_counterparties_request msg;
            msg.party_id = rr::read<boost::uuids::uuid>(parent, "parent");
            msg.scope = ores::utility::domain::scope::direct;
            msg.offset = offset;
            msg.limit = party_counterparty_web_page_size;
            msg.order.field = "";
            msg.order.descending = false;
            const auto reply = ores::http::forwarding::forward(req, session, msg);
            rr::require_ok(reply.result);
            const auto& got = reply.party_counterparties;
            rows.insert(rows.end(), got.begin(), got.end());
            offset += static_cast<std::uint32_t>(got.size());
            if (got.empty() ||
                (got.size() < party_counterparty_web_page_size && rows.size() >= reply.total))
                break;
        }
        co_return http_response::json("{\"rows\":" + party_counterparty_web_rows(rows) + "}");
    } catch (const rr::refusal& refusal) {
        co_return rr::answer(refusal);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << e.what();
        co_return http_response::api_error(
            http_status::bad_gateway, "internal", std::string(rr::unanswered));
    }
}

boost::asio::awaitable<http_response>
party_counterparty_web_routes::handle_put(const http_request& req, nats_client& session) {
    try {
        party_counterparty_web_require_writable(req);
        const auto body = rr::read_body<party_counterparty_web_write_body>(
            party_counterparty_web_with_defaults(req.body));
        if (!body || !rr::has_reason(body->reasonCode) || body->commentary.size() > 2000)
            rr::invalid("A write needs the row, the version it was read at, and a reason.");
        messaging::put_party_counterparty_request msg;
        msg.change.write = body->write;
        msg.change.precondition.kind =
            body->version ? ores::utility::domain::precondition_kind::must_match_version :
                            ores::utility::domain::precondition_kind::must_not_exist;
        msg.change.precondition.version = body->version;
        msg.intent.reason_code = body->reasonCode;
        msg.intent.commentary = body->commentary;
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

boost::asio::awaitable<http_response>
party_counterparty_web_routes::handle_delete(const http_request& req, nats_client& session) {
    try {
        party_counterparty_web_require_writable(req);
        const auto body = rr::read_body<party_counterparty_web_removal_body>(req.body);
        if (!body || !rr::has_reason(body->reasonCode) || body->commentary.size() > 2000)
            rr::invalid("A removal names the row by party_id and counterparty_id, and a reason.");
        messaging::delete_party_counterparty_request msg;
        msg.removal.key = body->key;
        msg.removal.precondition.kind =
            body->version ? ores::utility::domain::precondition_kind::must_match_version :
                            ores::utility::domain::precondition_kind::any;
        msg.removal.precondition.version = body->version;
        msg.intent.reason_code = body->reasonCode;
        msg.intent.commentary = body->commentary;
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

void party_counterparty_web_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    BOOST_LOG_SEV(lg(), info) << "Registering party-counterparties record routes";

    auto list_route =
        router->get("/api/refdata/party-counterparties")
            .summary("List party-counterparties for a screen")
            .description("One page with its total, or every row when no limit is named.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_list(req, session); });
    const auto list_built = list_route.build();
    router->add_route(list_built);
    registry->register_route(list_built);

    auto by_route =
        router->get("/api/refdata/party-counterparties/by/{parent}")
            .summary("List one parent's party-counterparties for a screen")
            .description("Every row of one parent.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_by(req, session); });
    const auto by_built = by_route.build();
    router->add_route(by_built);
    registry->register_route(by_built);

    auto put_route =
        router->put("/api/refdata/party-counterparties")
            .summary("Write one party-counterparties record for a screen")
            .description("A new row with no version, else a correction of the version read.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_put(req, session); });
    const auto put_built = put_route.build();
    router->add_route(put_built);
    registry->register_route(put_built);

    auto remove_route =
        router->delete_("/api/refdata/party-counterparties")
            .summary("Remove one party-counterparties record for a screen")
            .description("Removes the row the body's key names.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_delete(req, session); });
    const auto remove_built = remove_route.build();
    router->add_route(remove_built);
    registry->register_route(remove_built);
}

}
