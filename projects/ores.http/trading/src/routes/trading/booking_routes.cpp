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
#include "ores.http/routes/trading/booking_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.trading.api/domain/instrument_batch.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.api/domain/trade_booking.hpp"
#include "ores.trading.api/messaging/bond_instrument_protocol.hpp"
#include "ores.trading.api/messaging/bond_issue_protocol.hpp"
#include "ores.trading.api/messaging/draft_reason_protocol.hpp"
#include "ores.trading.api/messaging/trade_id_type_protocol.hpp"
#include "ores.trading.api/messaging/trade_identifier_protocol.hpp"
#include "ores.trading.api/messaging/trade_link_protocol.hpp"
#include "ores.trading.api/messaging/trade_link_type_protocol.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "ores.trading.api/messaging/trade_state_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <algorithm>
#include <cctype>
#include <chrono>
#include <cstddef>
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
namespace trading_domain = ores::trading::domain;
namespace trading_messaging = ores::trading::messaging;
namespace refdata_messaging = ores::refdata::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

/// The unit's own helpers and bodies, named apart from the other units that
/// build into one translation unit with it.
namespace booking {

/// The one transition that books a live trade.
constexpr std::string_view new_booking = "new_booking";

/// The activity that captures a draft rather than a concluded deal.
constexpr std::string_view draft_capture = "draft_capture";

/// Why a booked trade exists, as the trade's own change reason.
constexpr std::string_view booking_reason = "system.new_record";

/// The ORE trade type a booking states, because the form books bonds only.
constexpr std::string_view booked_trade_type = "Bond";

/// The intent every write of a booking carries.
ores::utility::domain::change_intent intent() {
    return {std::string(booking_reason), {}};
}

/// The system tenant, which the service stamps the caller's own over.
boost::uuids::uuid stated_by_the_service() {
    return ores::utility::uuid::tenant_id::system().to_uuid();
}

/// Why the deal is a draft, and what the desk stated to book it.
struct trade_body final {
    std::string bookId;
    std::string counterpartyId;
    std::string tradeDate;
    std::string issueId;
    std::string notional;
    std::string identifierType;
    std::string identifierValue;
    std::optional<std::string> draftReasonCode;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{
        field{"bookId", &trade_body::bookId},
        field{"counterpartyId", &trade_body::counterpartyId},
        field{"tradeDate", &trade_body::tradeDate},
        field{"issueId", &trade_body::issueId},
        field{"notional", &trade_body::notional},
        field{"identifierType", &trade_body::identifierType},
        field{"identifierValue", &trade_body::identifierValue},
        field{"draftReasonCode", &trade_body::draftReasonCode}};
}

/// The link a person records between two trades.
struct link_body final {
    std::string linkType;
    std::string otherTradeId;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<link_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"linkType", &link_body::linkType},
                                              field{"otherTradeId", &link_body::otherTradeId}};
}

/// Whether a text is a plain decimal: digits, and a point with digits after it.
bool is_plain_decimal(const std::string& text) {
    const auto point = text.find('.');
    const auto whole = text.substr(0, point);
    const auto fraction = point == std::string::npos ? std::string() : text.substr(point + 1);
    const auto digits = [](const std::string& part) {
        return !part.empty() && std::all_of(part.begin(), part.end(), [](unsigned char c) {
            return std::isdigit(c) != 0;
        });
    };
    return digits(whole) && (point == std::string::npos || digits(fraction));
}

/// Whether a text is an ISO calendar date: four digits, a month and a day.
bool is_iso_date(const std::string& text) {
    if (text.size() != 10 || text[4] != '-' || text[7] != '-')
        return false;
    for (std::size_t i = 0; i < text.size(); ++i) {
        if (i == 4 || i == 7)
            continue;
        if (std::isdigit(static_cast<unsigned char>(text[i])) == 0)
            return false;
    }
    return true;
}

/// An id named by a text, refused by the sentence when the text is not one.
boost::uuids::uuid uuid_of(const std::string& text, const std::string& sentence) {
    if (!rr::is_uuid(text))
        rr::invalid(sentence);
    return boost::uuids::string_generator()(text);
}

/// A date named by a text, refused by the sentence when no calendar holds it.
std::chrono::year_month_day date_of(const std::string& text, const std::string& sentence) {
    if (!is_iso_date(text))
        rr::invalid(sentence);
    const auto number = [&text](std::size_t at, std::size_t count) {
        int value = 0;
        for (std::size_t i = at; i < at + count; ++i)
            value = value * 10 + (text[i] - '0');
        return value;
    };
    const std::chrono::year_month_day date{std::chrono::year{number(0, 4)},
                                           std::chrono::month{static_cast<unsigned>(number(5, 2))},
                                           std::chrono::day{static_cast<unsigned>(number(8, 2))}};
    if (!date.ok())
        rr::invalid(sentence);
    return date;
}

/// The notional a text states, refused by the sentence when it is not a plain decimal.
ores::utility::decimal::decimal notional_of(const std::string& text, const std::string& sentence) {
    if (!is_plain_decimal(text))
        rr::invalid(sentence);
    const auto value = ores::utility::decimal::decimal::from_string(text);
    if (!value)
        rr::invalid(sentence);
    return *value;
}

/// The caller's acting party, as their own token states it.
std::optional<std::string> party_of(const http_request& req) {
    if (!req.authenticated_user)
        return std::nullopt;
    const auto& party = req.authenticated_user->party_id;
    if (!party || party->empty())
        return std::nullopt;
    return party;
}

/// The books read at their own page, each as the form offers it.
std::vector<ores::refdata::domain::book> all_books(const http_request& req, nats_client& session) {
    return forwarding::forward_all(req,
                                   session,
                                   refdata_messaging::list_books_request{},
                                   &refdata_messaging::list_books_response::books);
}

/// The bond issues a bond trade may name, every one the tenant holds.
std::vector<trading_domain::bond_issue> all_issues(const http_request& req, nats_client& session) {
    return forwarding::forward_all(req,
                                   session,
                                   trading_messaging::list_bond_issues_request{},
                                   &trading_messaging::list_bond_issues_response::issues);
}

/// The identifier schemes the tenant declares.
std::vector<trading_domain::trade_id_type> declared_schemes(const http_request& req,
                                                            nats_client& session) {
    return forwarding::forward_all(req,
                                   session,
                                   trading_messaging::list_trade_id_types_request{},
                                   &trading_messaging::list_trade_id_types_response::id_types);
}

/// Every identifier the tenant's trades already carry, read for the schemes they name.
std::vector<trading_domain::trade_identifier> used_identifiers(const http_request& req,
                                                               nats_client& session) {
    return forwarding::forward_all(
        req,
        session,
        trading_messaging::list_trade_identifiers_request{},
        &trading_messaging::list_trade_identifiers_response::identifiers);
}

/// The states a trade holds, newest first.
std::vector<trading_domain::trade_state>
states_of(const http_request& req, nats_client& session, const boost::uuids::uuid& trade_id) {
    trading_messaging::list_trade_states_request msg;
    msg.filter.emplace();
    msg.filter->trade_id_one_of = std::vector{trade_id};
    msg.order.field = "version";
    msg.order.descending = true;
    return forwarding::forward_all(
        req, session, msg, &trading_messaging::list_trade_states_response::trade_states);
}

/// The link a body states, checked as the BFF checked it.
trading_messaging::put_trade_link_request link_of(const http_request& req) {
    const std::string sentence =
        "A link states a link type of 1 to 80 characters and another trade by its id.";
    rr::refuse_unknown_members(req.body, {"linkType", "otherTradeId"}, sentence);
    const auto body = rr::read_body<booking::link_body>(req.body);
    if (!body)
        rr::invalid(sentence);
    const auto& link_type = body->linkType;
    if (link_type.empty() || rr::characters_of(link_type) > 80)
        rr::invalid(sentence);
    trading_messaging::put_trade_link_request msg;
    msg.change.write.link_type = link_type;
    msg.change.write.to_trade_id = uuid_of(body->otherTradeId, sentence);
    msg.intent = intent();
    return msg;
}

/// A booking a body states: the booking write, and the identifier and
/// instrument writes that follow it, each carrying its own facts.
struct stated_booking final {
    trading_messaging::book_trade_request msg;
    std::string identifier_type;
    std::string identifier_value;
    boost::uuids::uuid issue_id;
    ores::utility::decimal::decimal notional;
};

/// The booking a body states, checked as the BFF checked it.
stated_booking
booking_of(const http_request& req, const boost::uuids::uuid& trade_id, const std::string& party) {
    const std::string sentence =
        "A booking states a book, a counterparty, a trade date, an issue, a notional, an "
        "identifier scheme and an identifier value.";
    rr::refuse_unknown_members(req.body,
                               {"bookId",
                                "counterpartyId",
                                "tradeDate",
                                "issueId",
                                "notional",
                                "identifierType",
                                "identifierValue",
                                "draftReasonCode"},
                               sentence);
    const auto body = rr::read_body<booking::trade_body>(req.body);
    if (!body)
        rr::invalid(sentence);
    const auto book_id = uuid_of(body->bookId, sentence);
    const auto counterparty_id = uuid_of(body->counterpartyId, sentence);
    const auto issue_id = uuid_of(body->issueId, sentence);
    const auto trade_date = date_of(body->tradeDate, sentence);
    const auto notional = notional_of(body->notional, sentence);
    if (body->identifierType.empty() || rr::characters_of(body->identifierType) > 40 ||
        body->identifierValue.empty() || rr::characters_of(body->identifierValue) > 80)
        rr::invalid(sentence);
    std::optional<std::string> draft_reason;
    if (body->draftReasonCode) {
        const auto reason = rr::trimmed(*body->draftReasonCode);
        if (reason.empty() || rr::characters_of(reason) > 64)
            rr::invalid(sentence);
        draft_reason = reason;
    }

    const auto now = std::chrono::system_clock::now();
    const auto party_id = uuid_of(party, sentence);

    trading_domain::trade anchor;
    anchor.version = 0;
    anchor.tenant_id = ores::utility::uuid::tenant_id::system();
    anchor.id = trade_id;
    anchor.party_id = party_id;
    anchor.counterparty_id = counterparty_id;
    anchor.trade_type = std::string(booked_trade_type);
    anchor.counterparty_scope = trading_domain::counterparty_scope::external;
    anchor.booking_nature = trading_domain::booking_nature::actual;
    anchor.entry_channel = trading_domain::entry_channel::manual;
    anchor.origin = trading_domain::origin::api;
    anchor.origin_reference.clear();
    anchor.external_version = 0;
    anchor.draft_reason_code = draft_reason.value_or(std::string());
    anchor.change_reason_code = std::string(booking_reason);
    anchor.recorded_at = now;

    trading_domain::trade_booking booking;
    booking.version = 0;
    booking.tenant_id = ores::utility::uuid::tenant_id::system();
    booking.trade_id = trade_id;
    booking.trade_activity_id = stated_by_the_service();
    booking.book_id = book_id;
    booking.trade_date = trade_date;
    booking.execution_timestamp = now;
    booking.change_reason_code = std::string(booking_reason);
    booking.recorded_at = now;

    trading_messaging::book_trade_request msg;
    msg.anchor = std::move(anchor);
    msg.booking = std::move(booking);
    msg.activity_type_code = draft_reason ? std::string(draft_capture) : std::string(new_booking);
    msg.draft_reason_code = draft_reason.value_or(std::string());
    msg.identifiers = {};
    msg.instruments = trading_domain::instrument_batch{};
    return {std::move(msg), body->identifierType, body->identifierValue, issue_id, notional};
}

}

}

std::vector<std::string>
booking_routes::party_books_json(const std::vector<ores::refdata::domain::book>& books,
                                 const std::string& party_id) {
    std::vector<std::string> out;
    if (party_id.empty())
        return out;
    for (const auto& book : books) {
        if (boost::uuids::to_string(book.party_id) != party_id || book.book_status != "Active")
            continue;
        out.push_back("{\"id\":" + json::write(book.id) + ",\"name\":" + json::write(book.name) +
                      "}");
    }
    return out;
}

std::vector<std::string>
booking_routes::identifier_schemes_json(const std::vector<trading_domain::trade_id_type>& declared,
                                        const std::vector<trading_domain::trade_identifier>& used) {
    std::vector<std::string> codes;
    for (const auto& row : declared)
        if (!row.code.empty())
            codes.push_back(row.code);
    if (codes.empty())
        for (const auto& row : used)
            if (!row.id_type.empty())
                codes.push_back(row.id_type);
    std::sort(codes.begin(), codes.end());
    if (declared.empty())
        codes.erase(std::unique(codes.begin(), codes.end()), codes.end());
    std::vector<std::string> out;
    out.reserve(codes.size());
    for (const auto& code : codes)
        out.push_back("{\"code\":" + json::write(code) + "}");
    return out;
}

std::optional<boost::uuids::uuid>
booking_routes::newest_activity(const std::vector<trading_domain::trade_state>& states) {
    const trading_domain::trade_state* newest = nullptr;
    for (const auto& state : states)
        if (newest == nullptr || state.version > newest->version)
            newest = &state;
    if (newest == nullptr || newest->trade_activity_id.is_nil())
        return std::nullopt;
    return newest->trade_activity_id;
}

void booking_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using namespace booking;
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"trading"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/trades/new")
            .summary("What the booking form offers")
            .description("The acting party's open books, the securities a bond trade can name, "
                         "and the identifier schemes the tenant knows. The books and the "
                         "counterparties come from the routes that already serve them, so the "
                         "form has one picker per concept and no second copy.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<trading_messaging::list_bond_issues_request>([&] {
                    const auto party = party_of(req);
                    const auto books =
                        party_books_json(all_books(req, session), party.value_or(std::string()));
                    const auto issues = all_issues(req, session);
                    const auto declared = declared_schemes(req, session);
                    const auto used = declared.empty() ?
                                          used_identifiers(req, session) :
                                          std::vector<trading_domain::trade_identifier>{};
                    return http_response::json(
                        "{\"books\":" + rr::array_of(books) + ",\"issues\":" + json::write(issues) +
                        ",\"idTypes\":" + rr::array_of(identifier_schemes_json(declared, used)) +
                        "}");
                });
            }));

    add(router->get("/api/draft-reasons")
            .summary("The reasons a trade may be captured as a draft with")
            .description("A draft names a code from this list rather than free text, so the form "
                         "offers it and a report of aged drafts can group by it.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<trading_messaging::list_draft_reasons_request>([&] {
                    const auto rows = forwarding::forward_all(
                        req,
                        session,
                        trading_messaging::list_draft_reasons_request{},
                        &trading_messaging::list_draft_reasons_response::draft_reasons);
                    std::string reasons = "[";
                    for (const auto& row : rows) {
                        reasons += reasons.size() == 1 ? "{" : ",{";
                        reasons += "\"code\":" + json::write(row.code) +
                                   ",\"description\":" + json::write(row.description) +
                                   ",\"responsibleRole\":" + json::write(row.responsible_role) +
                                   "}";
                    }
                    return http_response::json("{\"reasons\":" + reasons + "]}");
                });
            }));

    add(router->get("/api/trade-link-types")
            .summary("The link types a person can record, with the words that describe each")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<trading_messaging::list_trade_link_types_request>([&] {
                    const auto rows = forwarding::forward_all(
                        req,
                        session,
                        trading_messaging::list_trade_link_types_request{},
                        &trading_messaging::list_trade_link_types_response::link_types);
                    std::string out = "[";
                    for (const auto& row : rows) {
                        out += out.size() == 1 ? "{" : ",{";
                        out += "\"code\":" + json::write(row.code) +
                               ",\"description\":" + json::write(row.description) + "}";
                    }
                    return http_response::json("{\"rows\":" + out + "]}");
                });
            }));

    add(router->post("/api/trades/{id}/links")
            .summary("Record a link from one trade to another")
            .description("A link is made by an activity, and the activity is the newest state's, "
                         "which the route reads rather than trusting a client to name it. The link "
                         "type is the caller's, because whether a relationship is a hedge or a "
                         "close-out is a fact about the structure.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<trading_messaging::put_trade_link_request>([&] {
                    const std::string address = "The address does not name a trade.";
                    const auto id = uuid_of(req.get_path_param("id"), address);
                    auto msg = link_of(req);
                    if (msg.change.write.to_trade_id == id)
                        rr::invalid("A trade cannot be linked to itself.");
                    const auto states = states_of(req, session, id);
                    const auto activity = newest_activity(states);
                    if (!activity)
                        rr::invalid("The trade has no activity to record the link against.");
                    msg.change.write.from_trade_id = id;
                    msg.change.write.trade_activity_id = *activity;
                    msg.change.precondition.kind =
                        ores::utility::domain::precondition_kind::must_not_exist;
                    const auto written = forwarding::forward(req, session, msg);
                    rr::require_ok(written.result);
                    return http_response::json(
                        "{\"from\":" + json::write(id) +
                        ",\"to\":" + json::write(msg.change.write.to_trade_id) +
                        ",\"linkType\":" + json::write(msg.change.write.link_type) + "}");
                });
            }));

    add(router->post("/api/trades")
            .summary("Book a trade")
            .description(
                "The write is the trading service's operation, so the route states a "
                "request and reads the answer: the service decides whether the book is "
                "open, whether the trade id is free and whether the instrument belongs to "
                "the trade type. A draft names a reason from the lookup; absent, the trade "
                "goes live at once.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<trading_messaging::book_trade_request>([&] {
                    const auto party = party_of(req);
                    if (!party)
                        rr::invalid("A booking is made by a party, and this session works for "
                                    "none.");
                    const auto trade_id = boost::uuids::random_generator()();
                    auto stated = booking_of(req, trade_id, *party);
                    const auto acknowledgement = forwarding::forward(req, session, stated.msg);
                    rr::require_ok(acknowledgement.result);
                    if (!acknowledgement.activity_id)
                        rr::refuse(http_status::bad_gateway,
                                   "upstream-unavailable",
                                   "The booking answered without the activity that made it.");
                    const auto activity = *acknowledgement.activity_id;

                    trading_messaging::put_trade_identifier_request identified;
                    identified.change.write.trade_id = trade_id;
                    identified.change.write.id_type = stated.identifier_type;
                    identified.change.write.trade_activity_id = activity;
                    identified.change.write.id_value = stated.identifier_value;
                    identified.change.precondition.kind =
                        ores::utility::domain::precondition_kind::must_not_exist;
                    identified.intent = intent();
                    const auto written_identifier = forwarding::forward(req, session, identified);
                    rr::require_ok(written_identifier.result);

                    trading_messaging::put_bond_instrument_request instrumented;
                    instrumented.change.write.trade_id = trade_id;
                    instrumented.change.write.trade_type_code = std::string(booked_trade_type);
                    instrumented.change.write.trade_activity_id = activity;
                    instrumented.change.write.issue_id = stated.issue_id;
                    instrumented.change.write.notional = stated.notional;
                    instrumented.change.precondition.kind =
                        ores::utility::domain::precondition_kind::must_not_exist;
                    instrumented.intent = intent();
                    const auto written_instrument = forwarding::forward(req, session, instrumented);
                    rr::require_ok(written_instrument.result);

                    return http_response::json("{\"id\":" + json::write(trade_id) + "}",
                                               http_status::created);
                });
            }));
}

}
