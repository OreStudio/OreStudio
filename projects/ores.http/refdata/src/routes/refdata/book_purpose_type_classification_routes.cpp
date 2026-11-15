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
 * Template: cpp_http_route_web_classification.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.http/routes/refdata/book_purpose_type_classification_routes.hpp"
#include "ores.http.api/net/classification_routes.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/refdata/classification_routes.hpp"
#include "ores.refdata.api/domain/book_purpose_type_codec.hpp"
#include "ores.refdata.api/messaging/book_purpose_type_protocol.hpp"
#include <algorithm>
#include <exception>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace ores::http::routes::refdata {

using namespace logging;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace messaging = ores::refdata::messaging;
namespace rr = ores::http::net::record_routes;
namespace cr = ores::http::net::classification_routes;

namespace {

auto& book_purpose_type_classification_routes_lg() {
    static auto instance =
        make_logger("ores.http.routes.refdata.book_purpose_type_classification_routes");
    return instance;
}

/// One page reads any classification list whole; the lists are short.
constexpr std::uint32_t book_purpose_type_classification_page_size = 1000;

std::vector<ores::refdata::domain::book_purpose_type>
book_purpose_type_classification_rows(const http_request& req, nats_client& session) {
    messaging::list_book_purpose_types_request msg;
    msg.offset = 0;
    msg.limit = book_purpose_type_classification_page_size;
    msg.order.field = "";
    msg.order.descending = false;
    auto reply = ores::http::forwarding::forward(req, session, msg);
    rr::require_ok(reply.result);
    if (reply.total > reply.types.size())
        BOOST_LOG_SEV(book_purpose_type_classification_routes_lg(), warn)
            << "The book-purpose-type list holds " << reply.total << " rows; one page read "
            << reply.types.size();
    return std::move(reply.types);
}

/// Refuse a write when the list holds spellings an ORE document must write exactly; an editable
/// list refuses nothing.
void book_purpose_type_classification_require_editable() {}

}

std::string
book_purpose_type_classification_routes::catalogue_entry(std::optional<std::uint64_t> count) {
    return std::string(
               R"json({"key":"book-purpose-type","entityType":"ores.refdata.book_purpose_type","topic":"books","shape":"named","editable":true,"writePermission":"refdata::book_purpose_types:write","deletePermission":"refdata::book_purpose_types:delete","count":)json") +
           (count ? std::to_string(*count) : std::string("null")) + "}";
}

std::uint64_t book_purpose_type_classification_routes::count(const http_request& req,
                                                             nats_client& session) {
    messaging::list_book_purpose_types_request msg;
    msg.offset = 0;
    msg.limit = 1;
    msg.order.field = "";
    msg.order.descending = false;
    const auto reply = ores::http::forwarding::forward(req, session, msg);
    rr::require_ok(reply.result);
    return reply.total;
}

boost::asio::awaitable<http_response>
book_purpose_type_classification_routes::handle_list(const http_request& req,
                                                     nats_client& session) {
    try {
        auto rows = book_purpose_type_classification_rows(req, session);
        std::map<std::string, std::string> labels;
        try {
            labels = classification_routes::labels_of(req, session, "book_purpose_type");
        } catch (const rr::refusal& refusal) {
            BOOST_LOG_SEV(lg(), warn) << "Labels could not be read: " << refusal.message;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Labels could not be read: " << e.what();
        }
        cr::sort_rows(rows);
        std::string out = "{\"rows\":[";
        for (std::size_t i = 0; i < rows.size(); ++i) {
            if (i != 0)
                out += ",";
            out += cr::row_json(rows[i], labels);
        }
        co_return http_response::json(out + "]}");
    } catch (const rr::refusal& refusal) {
        co_return rr::answer(refusal);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << e.what();
        co_return http_response::api_error(
            http_status::bad_gateway, "internal", std::string(rr::unanswered));
    }
}

boost::asio::awaitable<http_response>
book_purpose_type_classification_routes::handle_create(const http_request& req,
                                                       nats_client& session) {
    try {
        book_purpose_type_classification_require_editable();
        const auto body = rr::read_body<cr::write_body>(req.body);
        if (!body || !body->code || !rr::has_reason(body->reasonCode) ||
            !cr::text_fits(body->name) || !cr::text_fits(body->description) ||
            !cr::text_fits(body->commentary))
            rr::invalid("A row needs a code and a change reason.");
        const auto code = rr::code(*body->code, "A row needs a code and a change reason.");
        messaging::put_book_purpose_type_request msg;
        msg.change.write = cr::write_of<messaging::book_purpose_type_write>(
            code, body->name, body->description, body->displayOrder);
        msg.change.precondition.kind = ores::utility::domain::precondition_kind::must_not_exist;
        msg.intent = cr::intent_of(body->reasonCode, body->commentary);
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
book_purpose_type_classification_routes::handle_update(const http_request& req,
                                                       nats_client& session) {
    try {
        book_purpose_type_classification_require_editable();
        const auto code = rr::code(req.get_path_param("code"),
                                   "A row is named by a code of 1 to 100 characters.");
        const auto body = rr::read_body<cr::write_body>(req.body);
        if (!body || !body->version || !rr::has_reason(body->reasonCode) ||
            !cr::text_fits(body->name) || !cr::text_fits(body->description) ||
            !cr::text_fits(body->commentary))
            rr::invalid("A correction needs the version it was read at and a reason.");
        messaging::put_book_purpose_type_request msg;
        msg.change.write = cr::write_of<messaging::book_purpose_type_write>(
            code, body->name, body->description, body->displayOrder);
        msg.change.precondition.kind = ores::utility::domain::precondition_kind::must_match_version;
        msg.change.precondition.version = body->version;
        msg.intent = cr::intent_of(body->reasonCode, body->commentary);
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
book_purpose_type_classification_routes::handle_order(const http_request& req,
                                                      nats_client& session) {
    try {
        book_purpose_type_classification_require_editable();
        const auto body = rr::read_body<cr::order_body>(req.body);
        const std::string sentence =
            "Send the rows that moved, each with its version, and a reason.";
        if (!body || body->rows.empty() || body->rows.size() > 1000 ||
            !rr::has_reason(body->reasonCode) || !cr::text_fits(body->commentary))
            rr::invalid(sentence);
        messaging::put_many_book_purpose_types_request msg;
        std::set<std::string> codes;
        for (const auto& row : body->rows) {
            if (!row.code || !row.displayOrder || !row.version || *row.displayOrder < 0)
                rr::invalid(sentence);
            const auto code = rr::code(*row.code, sentence);
            if (!codes.insert(code).second)
                rr::invalid("Each row may appear once.");
            auto& change = msg.changes.emplace_back();
            change.write = cr::write_of<messaging::book_purpose_type_write>(
                code, row.name, row.description, row.displayOrder);
            change.precondition.kind = ores::utility::domain::precondition_kind::must_match_version;
            change.precondition.version = row.version;
        }
        msg.intent = cr::intent_of(body->reasonCode, body->commentary);
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
book_purpose_type_classification_routes::handle_delete(const http_request& req,
                                                       nats_client& session) {
    try {
        book_purpose_type_classification_require_editable();
        const auto code = rr::code(req.get_path_param("code"),
                                   "A row is named by a code of 1 to 100 characters.");
        const auto body = rr::read_body<cr::removal_body>(req.body);
        if (!body || !rr::has_reason(body->reasonCode) || !cr::text_fits(body->commentary))
            rr::invalid("A removal needs a change reason.");
        messaging::delete_book_purpose_type_request msg;
        msg.removal.key.code = code;
        msg.removal.precondition.kind = ores::utility::domain::precondition_kind::any;
        msg.intent = cr::intent_of(body->reasonCode, body->commentary);
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
book_purpose_type_classification_routes::handle_label(const http_request& req,
                                                      nats_client& session) {
    try {
        const auto code = rr::code(req.get_path_param("code"),
                                   "A row is named by a code of 1 to 100 characters.");
        const auto body = rr::read_body<cr::label_body>(req.body);
        if (!body || !rr::has_reason(body->reasonCode) || !cr::text_fits(body->commentary) ||
            (body->badgeCode &&
             (rr::trimmed(*body->badgeCode).empty() || body->badgeCode->size() > 200)))
            rr::invalid("A label needs a badge, or null to take it away, and a reason.");
        const auto rows = book_purpose_type_classification_rows(req, session);
        if (std::none_of(
                rows.begin(), rows.end(), [&](const auto& row) { return row.code == code; }))
            rr::refuse(
                http_status::not_found, "not-found", "book-purpose-type has no row " + code + ".");
        const auto intent = cr::intent_of(body->reasonCode, body->commentary);
        classification_routes::set_label(
            req, session, "book_purpose_type", code, body->badgeCode, intent);
        co_return rr::no_content();
    } catch (const rr::refusal& refusal) {
        co_return rr::answer(refusal);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << e.what();
        co_return http_response::api_error(
            http_status::bad_gateway, "internal", std::string(rr::unanswered));
    }
}

void book_purpose_type_classification_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    BOOST_LOG_SEV(lg(), info) << "Registering the book-purpose-type classification routes";

    auto list_route =
        router->get("/api/classifications/book-purpose-type")
            .summary("List the book-purpose-type rows for the classification screen")
            .description("Every row, each with its label.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_list(req, session); });
    const auto list_built = list_route.build();
    router->add_route(list_built);
    registry->register_route(list_built);

    auto create_route =
        router->post("/api/classifications/book-purpose-type/rows")
            .summary("Add one book-purpose-type row")
            .description("A code already in the list is refused rather than replaced.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_create(req, session); });
    const auto create_built = create_route.build();
    router->add_route(create_built);
    registry->register_route(create_built);

    auto update_route =
        router->put("/api/classifications/book-purpose-type/rows/{code}")
            .summary("Correct one book-purpose-type row")
            .description("A correction of the version read.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_update(req, session); });
    const auto update_built = update_route.build();
    router->add_route(update_built);
    registry->register_route(update_built);

    auto order_route =
        router->put("/api/classifications/book-purpose-type/order")
            .summary("Reorder the book-purpose-type rows")
            .description("The rows that moved, each with its version, in one call.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_order(req, session); });
    const auto order_built = order_route.build();
    router->add_route(order_built);
    registry->register_route(order_built);

    auto delete_route =
        router->delete_("/api/classifications/book-purpose-type/rows/{code}")
            .summary("Remove one book-purpose-type row")
            .description("Its versions stay in the history.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_delete(req, session); });
    const auto delete_built = delete_route.build();
    router->add_route(delete_built);
    registry->register_route(delete_built);

    auto label_route =
        router->put("/api/classifications/book-purpose-type/rows/{code}/label")
            .summary("Label one book-purpose-type row")
            .description("Maps the row's code to a badge, or removes the mapping.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_label(req, session); });
    const auto label_built = label_route.build();
    router->add_route(label_built);
    registry->register_route(label_built);
}

}
