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
#include "ores.http/routes/trading/book_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.refdata.api/domain/book_change.hpp"
#include "ores.refdata.api/domain/portfolio_right.hpp"
#include "ores.refdata.api/messaging/book_proposal_operations_protocol.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.refdata.api/messaging/portfolio_protocol.hpp"
#include "ores.refdata.api/messaging/portfolio_right_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/uuid/string_generator.hpp>
#include <algorithm>
#include <cctype>
#include <cstddef>
#include <expected>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::http::routes::trading {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace refdata_domain = ores::refdata::domain;
namespace refdata_messaging = ores::refdata::messaging;
namespace iam_messaging = ores::iam::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

/// The unit's own helpers and bodies, named apart from the other units that
/// build into one translation unit with it.
namespace books {

/// The most lines one proposal may hold, as the server holds it.
constexpr std::size_t most_lines = 1000;

/// The largest text a line's commentary columns hold.
constexpr std::size_t long_text = 2000;

/// Each object written as JSON, in an array.
std::string array_of(const std::vector<std::string>& objects) {
    std::string out = "[";
    for (std::size_t i = 0; i < objects.size(); ++i)
        out += (i == 0 ? "" : ",") + objects[i];
    return out + "]";
}

/// The text without its leading and trailing spaces.
std::string trimmed(const std::string& raw) {
    return rr::trimmed(raw);
}

/// The claim a write makes about the row: new when no version is stated.
ores::utility::domain::precondition claim_of(std::optional<std::uint32_t> version) {
    using ores::utility::domain::precondition_kind;
    if (!version)
        return {precondition_kind::must_not_exist, std::nullopt};
    return {precondition_kind::must_match_version, version};
}

/// The bodies the book screens send, beside the wire_fields each is read with.
struct book_write_body final {
    rr::stated_intent intent;
    std::optional<std::uint32_t> version;
    refdata_messaging::book_write write;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<book_write_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"intent", &book_write_body::intent},
                                              field{"version", &book_write_body::version},
                                              field{"write", &book_write_body::write}};
}

struct portfolio_write_body final {
    rr::stated_intent intent;
    std::optional<std::uint32_t> version;
    refdata_messaging::portfolio_write write;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<portfolio_write_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"intent", &portfolio_write_body::intent},
                                              field{"version", &portfolio_write_body::version},
                                              field{"write", &portfolio_write_body::write}};
}

/// One proposed write, as the client states it.
struct change_line final {
    int version = 0;
    std::string id;
    std::string request_id;
    int line_no = 0;
    std::string operation;
    int base_version = 0;
    bool applied = false;
    std::string entity_id;
    std::string party_id;
    std::string name;
    std::string description;
    std::string parent_portfolio_id;
    std::optional<std::string> owner_unit_id;
    std::string functional_currency;
    std::string gl_account_ref;
    std::string cost_center;
    std::string book_status;
    std::string regulatory_book_type;
    std::string book_purpose_type;
    std::string ledger_feed_type;
    bool is_sweepable = false;
    bool restricts_product_types = false;
    std::string rates_centre_code;
    std::optional<std::string> sandbox_id;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<change_line>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{
        field{"version", &change_line::version},
        field{"id", &change_line::id},
        field{"request_id", &change_line::request_id},
        field{"line_no", &change_line::line_no},
        field{"operation", &change_line::operation},
        field{"base_version", &change_line::base_version},
        field{"applied", &change_line::applied},
        field{"entity_id", &change_line::entity_id},
        field{"party_id", &change_line::party_id},
        field{"name", &change_line::name},
        field{"description", &change_line::description},
        field{"parent_portfolio_id", &change_line::parent_portfolio_id},
        field{"owner_unit_id", &change_line::owner_unit_id},
        field{"functional_currency", &change_line::functional_currency},
        field{"gl_account_ref", &change_line::gl_account_ref},
        field{"cost_center", &change_line::cost_center},
        field{"book_status", &change_line::book_status},
        field{"regulatory_book_type", &change_line::regulatory_book_type},
        field{"book_purpose_type", &change_line::book_purpose_type},
        field{"ledger_feed_type", &change_line::ledger_feed_type},
        field{"is_sweepable", &change_line::is_sweepable},
        field{"restricts_product_types", &change_line::restricts_product_types},
        field{"rates_centre_code", &change_line::rates_centre_code},
        field{"sandbox_id", &change_line::sandbox_id}};
}

/// The lines one proposal states.
struct preview_body final {
    std::vector<change_line> lines;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<preview_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"lines", &preview_body::lines}};
}

/// The lines one proposal raises, and why.
struct raise_body final {
    std::string reason;
    std::vector<change_line> lines;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<raise_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"reason", &raise_body::reason},
                                              field{"lines", &raise_body::lines}};
}

/// One book, as its own page reads it.
std::vector<refdata_domain::book>
books_by_id(const http_request& req, nats_client& session, const boost::uuids::uuid& id) {
    refdata_messaging::list_books_request msg;
    msg.filter.emplace();
    msg.filter->id_one_of = std::vector{id};
    return forwarding::forward_all(
        req, session, msg, &refdata_messaging::list_books_response::books);
}

/// One portfolio, as the book's page reads it.
std::vector<refdata_domain::portfolio>
portfolios_by_id(const http_request& req, nats_client& session, const boost::uuids::uuid& id) {
    refdata_messaging::list_portfolios_request msg;
    msg.filter.emplace();
    msg.filter->id_one_of = std::vector{id};
    return forwarding::forward_all(
        req, session, msg, &refdata_messaging::list_portfolios_response::portfolios);
}

/// The rights held directly at one portfolio node.
std::vector<refdata_domain::portfolio_right>
rights_of(const http_request& req, nats_client& session, const boost::uuids::uuid& portfolio_id) {
    refdata_messaging::list_by_portfolio_id_portfolio_rights_request msg;
    msg.portfolio_id = portfolio_id;
    msg.scope = ores::utility::domain::scope::direct;
    return forwarding::forward_all(
        req,
        session,
        msg,
        &refdata_messaging::list_by_portfolio_id_portfolio_rights_response::portfolio_rights);
}

/// Every account the tenant holds, as a right's holder names it.
std::vector<ores::iam::domain::account> accounts(const http_request& req, nats_client& session) {
    iam_messaging::list_accounts_request msg;
    return forwarding::forward_all(
        req, session, msg, &iam_messaging::list_accounts_response::accounts);
}

/// A body read as its own shape, or refused by the sentence when it is another.
template <typename Body>
Body body_of(const http_request& req, const std::string& sentence) {
    const auto body = json::read_lenient<Body>(req.body);
    if (!body)
        rr::invalid(sentence);
    return *body;
}

/// The intent a write states, or a refusal naming what a write must say.
ores::utility::domain::change_intent write_intent_of(const rr::stated_intent& stated) {
    const auto intent = rr::intent_of(stated);
    if (!intent)
        rr::invalid("A write states why it is made, in 1 to 200 characters, with a note of up to "
                    "2000.");
    return *intent;
}

/// Whether a text names a row: an id, a party and a name of 1 to 200 characters.
void check_row(const std::string& id, const std::string& party_id, const std::string& name) {
    const std::string sentence =
        "A write states the row's id, its party and a name of 1 to 200 characters.";
    if (!rr::is_uuid(id) || !rr::is_uuid(party_id) || trimmed(name).empty() ||
        rr::characters_of(trimmed(name)) > 200)
        rr::invalid(sentence);
}

/// A line as the service reads it, refused by the sentence when it is malformed.
refdata_domain::book_change line_of(const change_line& in, const std::string& sentence) {
    const auto id_of = [&](const std::string& text) {
        if (!rr::is_uuid(text))
            rr::invalid(sentence);
        return boost::uuids::string_generator()(text);
    };
    const auto optional_id = [&](const std::optional<std::string>& text) {
        if (!text || text->empty())
            return std::optional<boost::uuids::uuid>();
        return std::optional<boost::uuids::uuid>(id_of(*text));
    };
    /* The request, the line number and the provenance are the server's to
     * state, so a client that names them is read as naming the entity again. */
    refdata_domain::book_change out;
    out.version = in.version;
    out.id = in.id.empty() ? id_of(in.entity_id) : id_of(in.id);
    out.request_id = in.request_id.empty() ? id_of(in.entity_id) : id_of(in.request_id);
    out.line_no = in.line_no;
    out.operation = trimmed(in.operation);
    out.base_version = in.base_version;
    out.applied = in.applied;
    out.entity_id = id_of(in.entity_id);
    out.party_id = id_of(in.party_id);
    out.name = trimmed(in.name);
    out.description = in.description;
    out.parent_portfolio_id = id_of(in.parent_portfolio_id);
    out.owner_unit_id = optional_id(in.owner_unit_id);
    out.functional_currency = trimmed(in.functional_currency);
    out.gl_account_ref = in.gl_account_ref;
    out.cost_center = in.cost_center;
    out.book_status = trimmed(in.book_status);
    out.regulatory_book_type = trimmed(in.regulatory_book_type);
    out.book_purpose_type = trimmed(in.book_purpose_type);
    out.ledger_feed_type = trimmed(in.ledger_feed_type);
    out.is_sweepable = in.is_sweepable;
    out.restricts_product_types = in.restricts_product_types;
    out.rates_centre_code = in.rates_centre_code;
    out.sandbox_id = optional_id(in.sandbox_id);
    out.change_reason_code = "system.update";
    return out;
}

/// The lines a proposal states, checked as the BFF's schema checked them.
template <typename Body>
Body proposal_of(const http_request& req, const std::string& sentence) {
    auto body = body_of<Body>(req, sentence);
    if (body.lines.empty() || body.lines.size() > most_lines)
        rr::invalid(sentence);
    for (const auto& line : body.lines) {
        const bool operation_ok =
            trimmed(line.operation) == "put" || trimmed(line.operation) == "delete";
        const bool text_ok = !trimmed(line.name).empty() &&
                             rr::characters_of(trimmed(line.name)) <= 200 &&
                             rr::characters_of(line.description) <= long_text &&
                             rr::characters_of(line.gl_account_ref) <= long_text &&
                             rr::characters_of(line.cost_center) <= long_text &&
                             rr::characters_of(line.rates_centre_code) <= long_text &&
                             rr::characters_of(line.functional_currency) <= 16 &&
                             rr::characters_of(line.book_status) <= 100 &&
                             rr::characters_of(line.regulatory_book_type) <= 100 &&
                             rr::characters_of(line.book_purpose_type) <= 100 &&
                             rr::characters_of(line.ledger_feed_type) <= 100;
        if (!operation_ok || line.base_version < 0 || !text_ok)
            rr::invalid(sentence);
    }
    return body;
}

}

} // namespace

std::string
book_routes::preview_json(const std::vector<refdata_messaging::book_line_outcome>& lines,
                          const std::vector<std::string>& part_codes) {
    std::vector<std::string> rows;
    for (const auto& line : lines) {
        rows.push_back("{\"lineNo\":" + std::to_string(line.line_no) +
                       ",\"operation\":" + json::write(line.operation) + ",\"subjectId\":" +
                       json::write(line.entity_id) + ",\"refusal\":" + json::write(line.refusal) +
                       ",\"columns\":" + json::write(line.columns) +
                       ",\"partCodes\":" + json::write(line.part_codes) + "}");
    }
    return "{\"lines\":" + books::array_of(rows) + ",\"partCodes\":" + json::write(part_codes) +
           "}";
}

std::string book_routes::raise_json(const std::vector<refdata_messaging::book_line_outcome>& lines,
                                    const std::vector<std::string>& part_codes,
                                    bool raised,
                                    const std::optional<boost::uuids::uuid>& request_id,
                                    const std::string& refusal_code,
                                    const std::string& refusal_message) {
    auto preview = preview_json(lines, part_codes);
    if (!preview.empty() && preview.back() == '}')
        preview.pop_back();
    return preview + ",\"requestId\":" +
           (raised && request_id ? json::write(*request_id) : std::string("null")) +
           ",\"refusalCode\":" + json::write(raised ? std::string() : refusal_code) +
           ",\"refusalMessage\":" + json::write(raised ? std::string() : refusal_message) + "}";
}

void book_routes::register_routes(std::shared_ptr<ores::http::net::router> router,
                                  std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                  nats_client& session) {
    using namespace books;
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"trading"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/books/{id}")
            .summary("One book and the portfolio it sits in")
            .description("A book names a portfolio by id and no more, so the portfolio's name is "
                         "read by key here rather than left for the screen to look up.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<refdata_messaging::list_books_request>([&] {
                    const auto raw = req.get_path_param("id");
                    if (!rr::is_uuid(raw))
                        rr::invalid("The address does not name a book.");
                    const auto id = boost::uuids::string_generator()(raw);
                    const auto found = books_by_id(req, session, id);
                    if (found.empty())
                        rr::refuse(http_status::not_found, "not-found", "No book has that id.");
                    std::string portfolio = "null";
                    if (!found.front().parent_portfolio_id.is_nil()) {
                        const auto parent =
                            portfolios_by_id(req, session, found.front().parent_portfolio_id);
                        if (!parent.empty())
                            portfolio = json::write(parent.front());
                    }
                    return http_response::json("{\"book\":" + json::write(found.front()) +
                                               ",\"portfolio\":" + portfolio + "}");
                });
            }));

    add(router->get("/api/books/portfolios/{id}/rights")
            .summary("The rights at one portfolio node, and the accounts that hold them")
            .description("The rights are read only. The server addresses a right by its code "
                         "alone, so a grant of a code another account already holds would collide; "
                         "granting and removing wait for the server to address a right by account.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<refdata_messaging::list_by_portfolio_id_portfolio_rights_request>(
                    [&] {
                        const auto raw = req.get_path_param("id");
                        if (!rr::is_uuid(raw))
                            rr::invalid("The address does not name a portfolio.");
                        const auto id = boost::uuids::string_generator()(raw);
                        return http_response::json(
                            "{\"rights\":" + json::write(rights_of(req, session, id)) +
                            ",\"accounts\":" + json::write(accounts(req, session)) + "}");
                    });
            }));

    add(router->put("/api/books/books")
            .summary("Write one book")
            .description("The row and the version the person read. No version claims the row is "
                         "new; a version claims the row is still the one read.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<refdata_messaging::put_book_request>([&] {
                    const std::string sentence =
                        "A write states why it is made, the row, and the version the row was read "
                        "at, or null for a new row.";
                    const auto body = body_of<book_write_body>(req, sentence);
                    check_row(boost::uuids::to_string(body.write.id),
                              boost::uuids::to_string(body.write.party_id),
                              body.write.name);
                    refdata_messaging::put_book_request msg;
                    msg.change.write = body.write;
                    msg.change.write.name = trimmed(msg.change.write.name);
                    msg.change.precondition = claim_of(body.version);
                    msg.intent = write_intent_of(body.intent);
                    const auto written = forwarding::forward(req, session, msg);
                    return http_response::json(
                        "{\"result\":" + json::write(written.result) + ",\"book\":" +
                        (written.book ? json::write(*written.book) : std::string("null")) + "}");
                });
            }));

    add(router->put("/api/books/portfolios")
            .summary("Write one portfolio")
            .description("The row and the version the person read, as a book's write states them.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<refdata_messaging::put_portfolio_request>([&] {
                    const std::string sentence =
                        "A write states why it is made, the row, and the version the row was read "
                        "at, or null for a new row.";
                    const auto body = body_of<portfolio_write_body>(req, sentence);
                    check_row(boost::uuids::to_string(body.write.id),
                              boost::uuids::to_string(body.write.party_id),
                              body.write.name);
                    refdata_messaging::put_portfolio_request msg;
                    msg.change.write = body.write;
                    msg.change.write.name = trimmed(msg.change.write.name);
                    msg.change.precondition = claim_of(body.version);
                    msg.intent = write_intent_of(body.intent);
                    const auto written = forwarding::forward(req, session, msg);
                    return http_response::json(
                        "{\"result\":" + json::write(written.result) + ",\"portfolio\":" +
                        (written.portfolio ? json::write(*written.portfolio) :
                                             std::string("null")) +
                        "}");
                });
            }));

    add(router->post("/api/book-changes/preview")
            .summary("Run the proposed writes through the real service and write nothing")
            .description("Every refusal the write would give comes back at once, and the parts "
                         "the policy needs for the lines together. An empty list of parts says "
                         "the lines need no request.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<refdata_messaging::preview_book_changes_request>([&] {
                    const std::string sentence =
                        "A preview states 1 to 1000 lines, each an operation of put or delete, a "
                        "row, its base version and its columns.";
                    auto body = proposal_of<preview_body>(req, sentence);
                    refdata_messaging::preview_book_changes_request msg;
                    for (const auto& line : body.lines)
                        msg.lines.push_back(line_of(line, sentence));
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);
                    return http_response::json(preview_json(reply.lines, reply.part_codes));
                });
            }));

    add(router->post("/api/book-changes")
            .summary("Raise one request that holds the proposed writes")
            .description("A refusal of the lines, or of a proposal that needs no request, is an "
                         "answer with the reason and no request id, because the maker has "
                         "something to correct.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<refdata_messaging::raise_book_changes_request>([&] {
                    const std::string sentence =
                        "A raise states why it is made, in 1 to 500 characters, and 1 to 1000 "
                        "lines, each an operation of put or delete, a row, its base version and "
                        "its columns.";
                    auto body = proposal_of<raise_body>(req, sentence);
                    body.reason = trimmed(body.reason);
                    if (body.reason.empty() || rr::characters_of(body.reason) > 500)
                        rr::invalid(sentence);
                    refdata_messaging::raise_book_changes_request msg;
                    msg.reason = body.reason;
                    for (const auto& line : body.lines)
                        msg.lines.push_back(line_of(line, sentence));
                    const auto reply = forwarding::forward(req, session, msg);
                    using ores::utility::domain::outcome;
                    const bool raised = reply.result.outcome == outcome::ok;
                    if (!raised && reply.result.outcome != outcome::invalid)
                        rr::require_ok(reply.result);
                    return http_response::json(raise_json(reply.lines,
                                                          reply.part_codes,
                                                          raised,
                                                          reply.request_id,
                                                          reply.result.code,
                                                          reply.result.message));
                });
            }));
}
}
