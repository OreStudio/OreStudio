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
#include "ores.http/routes/trading/trade_read_routes.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.trading.api/messaging/bond_instrument_protocol.hpp"
#include "ores.trading.api/messaging/bond_issue_protocol.hpp"
#include "ores.trading.api/messaging/structure_identifier_protocol.hpp"
#include "ores.trading.api/messaging/structure_member_protocol.hpp"
#include "ores.trading.api/messaging/trade_booking_protocol.hpp"
#include "ores.trading.api/messaging/trade_identifier_protocol.hpp"
#include "ores.trading.api/messaging/trade_link_protocol.hpp"
#include "ores.trading.api/messaging/trade_party_role_protocol.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.trading.api/messaging/trade_summary_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <array>
#include <charconv>
#include <cstdint>
#include <cstdlib>
#include <exception>
#include <limits>
#include <memory>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>
#include <yyjson.h>

namespace ores::http::routes::trading {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace forwarding = ores::http::forwarding;
namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace trading_domain = ores::trading::domain;
namespace trading_messaging = ores::trading::messaging;
namespace refdata_messaging = ores::refdata::messaging;

namespace {

/// The most rows one page may hold, what the record list offers with Load all.
constexpr int trade_page_most_rows = 1000;

/// The identifier types a desk quotes, in the order the screen prefers them.
constexpr std::array<std::string_view, 4> identifier_order{"Internal", "UTI", "USI", "ORE"};

/// A JSON object with each named member set, replacing a member of the same name.
std::string with_members(const std::string& object,
                         const std::vector<std::pair<std::string, std::string>>& members) {
    const std::unique_ptr<yyjson_doc, decltype(&yyjson_doc_free)> doc(
        yyjson_read(object.data(), object.size(), 0), &yyjson_doc_free);
    if (!doc || !yyjson_is_obj(yyjson_doc_get_root(doc.get())))
        throw std::logic_error("A trade row is not written as an object.");
    const std::unique_ptr<yyjson_mut_doc, decltype(&yyjson_mut_doc_free)> out(
        yyjson_doc_mut_copy(doc.get(), nullptr), &yyjson_mut_doc_free);
    auto* root = yyjson_mut_doc_get_root(out.get());
    for (const auto& [name, value] : members) {
        const std::unique_ptr<yyjson_doc, decltype(&yyjson_doc_free)> parsed(
            yyjson_read(value.data(), value.size(), 0), &yyjson_doc_free);
        if (!parsed)
            throw std::logic_error("A member added to a trade row is not JSON.");
        yyjson_mut_obj_put(root,
                           yyjson_mut_strncpy(out.get(), name.data(), name.size()),
                           yyjson_val_mut_copy(out.get(), yyjson_doc_get_root(parsed.get())));
    }
    std::size_t length = 0;
    char* text = yyjson_mut_write(out.get(), 0, &length);
    if (text == nullptr)
        throw std::runtime_error("A trade row could not be written as JSON.");
    std::string written(text, length);
    std::free(text);
    return written;
}

std::string text_of(const boost::uuids::uuid& id) {
    return boost::uuids::to_string(id);
}

std::string text_of(const std::optional<boost::uuids::uuid>& id) {
    return id ? boost::uuids::to_string(*id) : std::string();
}

/// Each row written as JSON, in an array.
template <typename Row>
std::string array_of(const std::vector<Row>& rows) {
    return json::write(rows);
}

std::string array_of(const std::vector<std::string>& objects) {
    std::string out = "[";
    for (const auto& o : objects)
        out += (out.size() == 1 ? "" : ",") + o;
    return out + "]";
}

/// The ids named, without repeats, in the order first named.
std::vector<boost::uuids::uuid> distinct(const std::vector<boost::uuids::uuid>& ids) {
    std::vector<boost::uuids::uuid> out;
    for (const auto& id : ids)
        if (std::ranges::find(out, id) == out.end())
            out.push_back(id);
    return out;
}

/// The rows a read the page can do without answers, or none when it refuses.
template <typename Request, typename Rows>
auto optional_all(const http_request& req, nats_client& session, Request msg, Rows member) {
    try {
        return forwarding::forward_all(req, session, std::move(msg), member);
    } catch (const rr::refusal&) {
    } catch (const std::exception&) {
    }
    return std::remove_cvref_t<decltype(std::declval<typename Request::response_type>().*member)>{};
}

/// Every read the page's rows are enriched from, bounded by the rows.
std::vector<std::string> enrich(const http_request& req,
                                nats_client& session,
                                const std::vector<trading_domain::trade_summary>& summaries) {
    std::vector<boost::uuids::uuid> trade_ids, structure_ids, book_ids, counterparty_ids;
    for (const auto& s : summaries) {
        if (s.row_kind == "trade")
            trade_ids.push_back(s.id);
        if (s.row_kind == "structure")
            structure_ids.push_back(s.id);
        if (s.book_id && !s.book_id->is_nil())
            book_ids.push_back(*s.book_id);
        if (s.counterparty_id && !s.counterparty_id->is_nil())
            counterparty_ids.push_back(*s.counterparty_id);
    }
    trade_ids = distinct(trade_ids);
    structure_ids = distinct(structure_ids);
    book_ids = distinct(book_ids);
    counterparty_ids = distinct(counterparty_ids);

    std::vector<trading_domain::structure_identifier> structure_identifiers;
    if (!structure_ids.empty()) {
        trading_messaging::list_structure_identifiers_request msg;
        msg.filter.emplace();
        msg.filter->structure_id_one_of = structure_ids;
        structure_identifiers =
            optional_all(req,
                         session,
                         msg,
                         &trading_messaging::list_structure_identifiers_response::identifiers);
    }
    std::vector<trading_domain::trade_identifier> identifiers;
    std::vector<trading_domain::trade> trades;
    if (!trade_ids.empty()) {
        trading_messaging::list_trade_identifiers_request ids_msg;
        ids_msg.filter.emplace();
        ids_msg.filter->trade_id_one_of = trade_ids;
        identifiers = forwarding::forward_all(
            req,
            session,
            ids_msg,
            &trading_messaging::list_trade_identifiers_response::identifiers);
        trading_messaging::list_trades_request trades_msg;
        trades_msg.filter.emplace();
        trades_msg.filter->id_one_of = trade_ids;
        trades = forwarding::forward_all(
            req, session, trades_msg, &trading_messaging::list_trades_response::trades);
    }
    std::vector<ores::refdata::domain::book> books;
    if (!book_ids.empty()) {
        refdata_messaging::list_books_request msg;
        msg.filter.emplace();
        msg.filter->id_one_of = book_ids;
        books = forwarding::forward_all(
            req, session, msg, &refdata_messaging::list_books_response::books);
    }
    std::vector<ores::refdata::domain::counterparty> counterparties;
    if (!counterparty_ids.empty()) {
        refdata_messaging::list_counterparties_request msg;
        msg.filter.emplace();
        msg.filter->id_one_of = counterparty_ids;
        counterparties = forwarding::forward_all(
            req, session, msg, &refdata_messaging::list_counterparties_response::counterparties);
    }
    return trade_read_routes::enriched_rows(
        summaries, identifiers, structure_identifiers, books, counterparties, trades);
}

/// The trade list's query, each member checked as the BFF checked it.
trading_messaging::list_trade_summaries_request page_of(const http_request& req) {
    const std::string sentence =
        "A page of trades names an offset, a limit of 1 to 1000, a search of up to 256 "
        "characters, an order by trade_date, instrument_type, status or row_kind, and filters "
        "of a status, a type, a party, a counterparty, a book and a date range.";
    const auto whole = [&](const std::string& name,
                           std::int64_t lowest,
                           std::int64_t highest,
                           std::int64_t fallback) {
        if (!req.query_params.contains(name))
            return fallback;
        const auto raw = req.get_query_param(name);
        std::int64_t value = 0;
        const auto [end, error] = std::from_chars(raw.data(), raw.data() + raw.size(), value);
        if (raw.empty() || error != std::errc{} || end != raw.data() + raw.size() ||
            value < lowest || value > highest)
            rr::invalid(sentence);
        return value;
    };
    const auto bounded = [&](const std::string& name, std::size_t most) {
        auto value = rr::trimmed(req.get_query_param(name));
        if (rr::characters_of(value) > most)
            rr::invalid(sentence);
        return value;
    };
    const auto id_or_none = [&](const std::string& name) -> std::optional<boost::uuids::uuid> {
        const auto raw = req.get_query_param(name);
        if (raw.empty())
            return std::nullopt;
        if (!rr::is_uuid(raw))
            rr::invalid(sentence);
        return boost::uuids::string_generator()(raw);
    };

    trading_messaging::list_trade_summaries_request msg;
    msg.offset = static_cast<std::uint32_t>(
        whole("offset", 0, std::numeric_limits<std::uint32_t>::max(), 0));
    msg.limit = static_cast<std::uint32_t>(whole("limit", 1, trade_page_most_rows, 25));
    static const std::set<std::string, std::less<>> sortable{
        "trade_date", "instrument_type", "status", "row_kind"};
    msg.order.field =
        req.query_params.contains("sort") ? req.get_query_param("sort") : std::string("trade_date");
    if (!sortable.contains(msg.order.field))
        rr::invalid(sentence);
    const auto descending =
        req.query_params.contains("descending") ? req.get_query_param("descending") : "true";
    if (descending != "true" && descending != "false")
        rr::invalid(sentence);
    msg.order.descending = descending == "true";

    auto& filter = msg.filter.emplace();
    if (const auto party = id_or_none("party"))
        filter.party_id = *party;
    if (const auto counterparty = id_or_none("counterparty"))
        filter.counterparty_id = std::optional(*counterparty);
    if (const auto book = id_or_none("book"))
        filter.book_id = std::optional(*book);
    if (auto type = bounded("type", 200); !type.empty())
        filter.instrument_type = std::move(type);
    if (auto status = bounded("status", 100); !status.empty())
        filter.status = std::optional(std::move(status));
    if (auto from = bounded("from", 10); !from.empty())
        filter.trade_date_from = std::move(from);
    if (auto to = bounded("to", 10); !to.empty())
        filter.trade_date_to = std::move(to);
    if (auto search = bounded("search", 256); !search.empty())
        filter.search = std::move(search);
    return msg;
}

/// The trade the address names.
boost::uuids::uuid trade_of(const http_request& req) {
    const auto id = req.get_path_param("id");
    if (!rr::is_uuid(id))
        rr::invalid("A trade is named by its identifier.");
    return boost::uuids::string_generator()(id);
}

/**
 * One trade's instrument terms: whether its type has a table, whether the read
 * was refused, and the terms with their issue, or null.
 */
std::string instrument_members(const http_request& req,
                               nats_client& session,
                               const std::string& instrument_type,
                               const boost::uuids::uuid& id) {
    if (instrument_type != "Bond")
        return R"("routed":false,"refused":false,"terms":null)";
    std::vector<trading_domain::bond_instrument> rows;
    try {
        trading_messaging::list_bond_instruments_request msg;
        msg.filter.emplace();
        msg.filter->trade_id_one_of = std::vector{id};
        rows = forwarding::forward_all(
            req,
            session,
            msg,
            &trading_messaging::list_bond_instruments_response::bond_instruments);
    } catch (const rr::refusal&) {
        return R"("routed":true,"refused":true,"terms":null)";
    } catch (const std::exception&) {
        return R"("routed":true,"refused":true,"terms":null)";
    }
    if (rows.empty())
        return R"("routed":true,"refused":false,"terms":null)";
    const auto& terms = rows.front();
    std::string issue = "null";
    if (!terms.issue_id.is_nil()) {
        trading_messaging::list_bond_issues_request msg;
        msg.filter.emplace();
        msg.filter->issue_id_one_of = std::vector{terms.issue_id};
        const auto issues =
            optional_all(req, session, msg, &trading_messaging::list_bond_issues_response::issues);
        if (!issues.empty())
            issue = json::write(issues.front());
    }
    return R"("routed":true,"refused":false,"terms":)" +
           with_members(json::write(terms), {{"issue", issue}});
}

}

std::map<std::string, std::string> trade_read_routes::best_identifiers(
    const std::vector<trading_domain::trade_identifier>& identifiers) {
    const auto rank_of = [](const std::string& type) {
        const auto found = std::ranges::find(identifier_order, type);
        return static_cast<std::size_t>(found - identifier_order.begin());
    };
    std::map<std::string, std::pair<std::size_t, std::string>> best;
    for (const auto& i : identifiers) {
        if (i.id_value.empty())
            continue;
        const auto trade = text_of(i.trade_id);
        const auto rank = rank_of(i.id_type);
        const auto held = best.find(trade);
        if (held == best.end() || rank < held->second.first)
            best[trade] = {rank, i.id_value};
    }
    std::map<std::string, std::string> out;
    for (const auto& [trade, chosen] : best)
        out[trade] = chosen.second;
    return out;
}

std::vector<std::string> trade_read_routes::enriched_rows(
    const std::vector<trading_domain::trade_summary>& summaries,
    const std::vector<trading_domain::trade_identifier>& identifiers,
    const std::vector<trading_domain::structure_identifier>& structures,
    const std::vector<ores::refdata::domain::book>& books,
    const std::vector<ores::refdata::domain::counterparty>& counterparties,
    const std::vector<trading_domain::trade>& trades) {
    std::vector<std::string> out;
    out.reserve(summaries.size());
    const bool nothing_named = std::ranges::none_of(summaries, [](const auto& s) {
        return s.row_kind == "trade" || s.row_kind == "structure" ||
               (s.book_id && !s.book_id->is_nil()) ||
               (s.counterparty_id && !s.counterparty_id->is_nil());
    });
    if (nothing_named) {
        for (const auto& s : summaries)
            out.push_back(json::write(s));
        return out;
    }
    const auto identifier_of = best_identifiers(identifiers);
    std::map<std::string, std::string> structure_reference;
    for (const auto& s : structures)
        if (!s.id_value.empty() && !structure_reference.contains(text_of(s.structure_id)))
            structure_reference[text_of(s.structure_id)] = s.id_value;
    std::map<std::string, std::string> book_name, counterparty_name;
    for (const auto& b : books)
        book_name[text_of(b.id)] = b.name;
    for (const auto& c : counterparties)
        counterparty_name[text_of(c.id)] = c.full_name;
    std::map<std::string, const trading_domain::trade*> trade_of;
    for (const auto& t : trades)
        trade_of[text_of(t.id)] = &t;

    const auto found_in = [](const std::map<std::string, std::string>& names,
                             const std::string& key) {
        const auto found = names.find(key);
        return found == names.end() ? std::string() : found->second;
    };
    for (const auto& s : summaries) {
        const auto id = text_of(s.id);
        const auto trade = trade_of.find(id);
        const bool is_structure = s.row_kind == "structure";
        out.push_back(with_members(
            json::write(s),
            {{"identifier",
              json::write(is_structure ? found_in(structure_reference, id) :
                                         found_in(identifier_of, id))},
             {"counterparty_name",
              json::write(found_in(counterparty_name, text_of(s.counterparty_id)))},
             {"book_name", json::write(found_in(book_name, text_of(s.book_id)))},
             {"version",
              trade == trade_of.end() ? std::string("null") : json::write(trade->second->version)},
             {"external_version",
              trade == trade_of.end() ? std::string("null") :
                                        json::write(trade->second->external_version)}}));
    }
    return out;
}

std::string trade_read_routes::structures_json(
    const std::vector<trading_domain::structure_member>& members,
    const std::vector<trading_domain::structure_identifier>& identifiers) {
    std::map<std::string, std::string> reference_of;
    for (const auto& i : identifiers)
        reference_of[text_of(i.structure_id)] = i.id_value;
    std::string out = "[";
    for (const auto& m : members) {
        const auto structure = text_of(m.structure_id);
        const auto reference = reference_of.find(structure);
        out += out.size() == 1 ? "{" : ",{";
        out += "\"structure_id\":" + json::write(structure);
        out += ",\"reference\":" +
               json::write(reference == reference_of.end() ? std::string() : reference->second);
        out += ",\"role\":" + json::write(m.role);
        out += ",\"sequence_number\":" + json::write(m.sequence_number);
        out += ",\"version\":" + json::write(m.version) + "}";
    }
    return out + "]";
}

std::string trade_read_routes::links_json(const std::vector<trading_domain::trade_link>& links,
                                          bool other_is_to,
                                          const std::map<std::string, std::string>& labels) {
    std::vector<std::string> rows;
    rows.reserve(links.size());
    for (const auto& l : links) {
        const auto other = text_of(other_is_to ? l.to_trade_id : l.from_trade_id);
        const auto label = labels.find(other);
        rows.push_back(with_members(
            json::write(l),
            {{"other_label", json::write(label == labels.end() ? std::string() : label->second)}}));
    }
    return array_of(rows);
}

void trade_read_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"trading"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/trades")
            .summary("Read one page of the trade list, enriched with what a person reads")
            .description("One page of the trade summary read model, with its total, ordered and "
                         "filtered by the service. Each row carries the identifier a desk "
                         "quotes, its counterparty's and book's names, and its trade's versions.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<trading_messaging::list_trade_summaries_request>([&] {
                    const auto reply = forwarding::forward(req, session, page_of(req));
                    rr::require_ok(reply.result);
                    return http_response::json(
                        "{\"rows\":" + array_of(enrich(req, session, reply.trade_summaries)) +
                        ",\"total\":" + std::to_string(reply.total) + "}");
                });
            }));

    add(router->get("/api/trades/{id}")
            .summary("Read one trade's page")
            .description("The trade's summary, identifiers, party roles, booking, instrument "
                         "terms, links at both ends with the other trade's identifier, the "
                         "structures it is a leg of, and its version history, every read "
                         "narrowed to the trade. The structures are supplementary: a caller who "
                         "may not read them still gets the trade.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<trading_messaging::list_trade_summaries_request>([&] {
                    const auto id = trade_of(req);
                    const auto ids = std::vector{id};

                    trading_messaging::list_trade_summaries_request summary_msg;
                    summary_msg.filter.emplace();
                    summary_msg.filter->id_one_of = ids;
                    const auto summaries = forwarding::forward_all(
                        req,
                        session,
                        summary_msg,
                        &trading_messaging::list_trade_summaries_response::trade_summaries);
                    if (summaries.empty())
                        rr::refuse(http_status::not_found, "not-found", "No trade has that id.");
                    const auto trade = enrich(req, session, {summaries.front()}).front();

                    trading_messaging::list_trade_identifiers_request identifiers_msg;
                    identifiers_msg.filter.emplace();
                    identifiers_msg.filter->trade_id_one_of = ids;
                    const auto identifiers = forwarding::forward_all(
                        req,
                        session,
                        identifiers_msg,
                        &trading_messaging::list_trade_identifiers_response::identifiers);
                    trading_messaging::list_trade_party_roles_request roles_msg;
                    roles_msg.filter.emplace();
                    roles_msg.filter->trade_id_one_of = ids;
                    const auto roles = forwarding::forward_all(
                        req,
                        session,
                        roles_msg,
                        &trading_messaging::list_trade_party_roles_response::roles);
                    trading_messaging::list_trade_bookings_request bookings_msg;
                    bookings_msg.filter.emplace();
                    bookings_msg.filter->trade_id_one_of = ids;
                    const auto bookings = forwarding::forward_all(
                        req,
                        session,
                        bookings_msg,
                        &trading_messaging::list_trade_bookings_response::trade_bookings);
                    trading_messaging::list_trade_links_request from_msg;
                    from_msg.filter.emplace();
                    from_msg.filter->from_trade_id_one_of = ids;
                    const auto from_links = forwarding::forward_all(
                        req,
                        session,
                        from_msg,
                        &trading_messaging::list_trade_links_response::links);
                    trading_messaging::list_trade_links_request to_msg;
                    to_msg.filter.emplace();
                    to_msg.filter->to_trade_id_one_of = ids;
                    const auto to_links = forwarding::forward_all(
                        req, session, to_msg, &trading_messaging::list_trade_links_response::links);

                    // A link names its other end by id, so the other trades' identifiers
                    // are read in one go and travel with the link.
                    std::vector<boost::uuids::uuid> others;
                    for (const auto& l : from_links)
                        others.push_back(l.from_trade_id == id ? l.to_trade_id : l.from_trade_id);
                    for (const auto& l : to_links)
                        others.push_back(l.from_trade_id == id ? l.to_trade_id : l.from_trade_id);
                    others = distinct(others);
                    std::map<std::string, std::string> labels;
                    if (!others.empty()) {
                        trading_messaging::list_trade_identifiers_request msg;
                        msg.filter.emplace();
                        msg.filter->trade_id_one_of = others;
                        for (const auto& found : forwarding::forward_all(
                                 req,
                                 session,
                                 msg,
                                 &trading_messaging::list_trade_identifiers_response::identifiers))
                            if (!found.id_value.empty())
                                labels.try_emplace(text_of(found.trade_id), found.id_value);
                    }

                    trading_messaging::list_structure_members_request members_msg;
                    members_msg.filter.emplace();
                    members_msg.filter->trade_id_one_of = ids;
                    const auto memberships =
                        optional_all(req,
                                     session,
                                     members_msg,
                                     &trading_messaging::list_structure_members_response::members);
                    std::vector<boost::uuids::uuid> structures;
                    for (const auto& m : memberships)
                        structures.push_back(m.structure_id);
                    structures = distinct(structures);
                    std::vector<trading_domain::structure_identifier> references;
                    if (!structures.empty()) {
                        trading_messaging::list_structure_identifiers_request msg;
                        msg.filter.emplace();
                        msg.filter->structure_id_one_of = structures;
                        references = optional_all(
                            req,
                            session,
                            msg,
                            &trading_messaging::list_structure_identifiers_response::identifiers);
                    }

                    trading_messaging::list_trade_versions_request versions_msg;
                    versions_msg.key.id = id;
                    versions_msg.order.field = "version";
                    versions_msg.order.descending = true;
                    const auto versions = forwarding::forward_all(
                        req,
                        session,
                        versions_msg,
                        &trading_messaging::list_trade_versions_response::versions);

                    return http_response::json(
                        "{\"trade\":" + trade + ",\"identifiers\":" + array_of(identifiers) +
                        ",\"roles\":" + array_of(roles) + ",\"booking\":" +
                        (bookings.empty() ? std::string("null") : json::write(bookings.front())) +
                        "," +
                        instrument_members(req, session, summaries.front().instrument_type, id) +
                        ",\"links\":{\"from\":" + links_json(from_links, true, labels) +
                        ",\"to\":" + links_json(to_links, false, labels) + "}" +
                        ",\"structures\":" + structures_json(memberships, references) +
                        ",\"history\":" + array_of(versions) + "}");
                });
            }));

    add(router->get("/api/trades/{id}/versions/{version}")
            .summary("Read one trade as it stood at one version")
            .description("A version the trade never had answers a null trade, because it is an "
                         "answer to the question rather than a refusal of it.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<trading_messaging::get_trade_version_request>([&] {
                    const std::string sentence =
                        "The address does not name a trade id and a version.";
                    const auto id = req.get_path_param("id");
                    const auto raw = req.get_path_param("version");
                    std::uint32_t version = 0;
                    const auto [end, error] =
                        std::from_chars(raw.data(), raw.data() + raw.size(), version);
                    if (!rr::is_uuid(id) || raw.empty() || error != std::errc{} ||
                        end != raw.data() + raw.size() || version < 1)
                        rr::invalid(sentence);
                    trading_messaging::get_trade_version_request msg;
                    msg.key.trade.id = boost::uuids::string_generator()(id);
                    msg.key.version = version;
                    const auto reply = forwarding::forward(req, session, msg);
                    if (reply.result.outcome == ores::utility::domain::outcome::missing)
                        return http_response::json(R"({"trade":null})");
                    rr::require_ok(reply.result);
                    return http_response::json(
                        "{\"trade\":" +
                        (reply.version ? json::write(*reply.version) : std::string("null")) + "}");
                });
            }));
}

}
