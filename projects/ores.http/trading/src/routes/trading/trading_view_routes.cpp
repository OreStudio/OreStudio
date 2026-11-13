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
#include "ores.http/routes/trading/trading_view_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/view_answer.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.refdata.api/messaging/asset_class_code_protocol.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/instrument_code_protocol.hpp"
#include "ores.refdata.api/messaging/portfolio_protocol.hpp"
#include "ores.trading.api/messaging/product_type_protocol.hpp"
#include "ores.trading.api/messaging/structure_identifier_protocol.hpp"
#include "ores.trading.api/messaging/structure_member_protocol.hpp"
#include "ores.trading.api/messaging/structure_protocol.hpp"
#include "ores.trading.api/messaging/trade_identifier_protocol.hpp"
#include "ores.trading.api/messaging/trade_summary_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/uuid/string_generator.hpp>
#include <algorithm>
#include <array>
#include <cctype>
#include <charconv>
#include <chrono>
#include <cstddef>
#include <functional>
#include <map>
#include <optional>
#include <set>
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
namespace trading_domain = ores::trading::domain;
namespace trading_messaging = ores::trading::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::utility::domain::order;

/// The unit's own helpers, named apart from the other units that build into one
/// translation unit with it.
namespace views {

/// The book tree is kept for a minute: a book or a portfolio written meanwhile
/// is not the caller's own write, so the view does not wait on it.
constexpr auto tree_stale_after = std::chrono::seconds(60);

/// The blotter is kept for half a minute: no write of the caller changes it
/// alone, and a page read twice in a visit should cost one walk.
constexpr auto blotter_stale_after = std::chrono::seconds(30);

/// How many rows one rail read asks for, and the stop against a runaway read.
constexpr std::uint32_t rail_page = 200;
constexpr std::uint32_t rail_cap = 20000;

/// The most rows one page of the list may hold.
constexpr int blotter_most_rows = 200;

/// The identifiers a trade is quoted by, best first.
constexpr std::array<std::string_view, 4> identifier_order{"Internal", "UTI", "USI", "ORE"};

std::string text_or_empty(const std::optional<std::string>& text) {
    return text.value_or(std::string());
}

/// A date as a person reads it, or the empty text when there is none.
std::string iso_date(const std::optional<std::chrono::year_month_day>& date) {
    return date ? std::format("{:%Y-%m-%d}", *date) : std::string();
}

/// A code made readable: the words the camel case and the underscores hide, with
/// the spaces put back.
std::string readable(const std::string& code) {
    std::string spaced;
    for (std::size_t i = 0; i < code.size(); ++i) {
        const char current = code[i];
        if (current == '_') {
            spaced += ' ';
            continue;
        }
        if (i > 0 && std::isupper(static_cast<unsigned char>(current))) {
            const char previous = code[i - 1];
            if (std::islower(static_cast<unsigned char>(previous)) != 0 ||
                std::isdigit(static_cast<unsigned char>(previous)) != 0)
                spaced += ' ';
        }
        spaced += current;
    }
    return rr::trimmed(spaced);
}

/// The rank of an identifier type: the order above, then the rest.
int rank_of(const std::string& id_type) {
    for (std::size_t i = 0; i < identifier_order.size(); ++i)
        if (identifier_order[i] == id_type)
            return static_cast<int>(i);
    return static_cast<int>(identifier_order.size());
}

/// The key two labels are compared by: the same words, whatever case.
std::string folded(std::string text) {
    std::transform(text.begin(), text.end(), text.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    return text;
}

/// The summary filter a read states, by the members this unit sets.
struct summary_filter final {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<bool> is_leg;
    std::optional<std::string> row_kind;
    std::optional<std::string> search;
};

/// Every portfolio the tenant holds.
std::vector<refdata_domain::portfolio> all_portfolios(const http_request& req,
                                                      nats_client& session) {
    refdata_messaging::list_portfolios_request msg;
    msg.filter.emplace();
    return forwarding::forward_all(
        req, session, msg, &refdata_messaging::list_portfolios_response::portfolios);
}

/// Every book the tenant holds.
std::vector<refdata_domain::book> all_books(const http_request& req, nats_client& session) {
    refdata_messaging::list_books_request msg;
    msg.filter.emplace();
    return forwarding::forward_all(
        req, session, msg, &refdata_messaging::list_books_response::books);
}

/// Every structure the tenant holds.
std::vector<trading_domain::structure> all_structures(const http_request& req,
                                                      nats_client& session) {
    trading_messaging::list_structures_request msg;
    msg.filter.emplace();
    return forwarding::forward_all(
        req, session, msg, &trading_messaging::list_structures_response::structures);
}

/// Every leg of every deal the tenant holds.
std::vector<trading_domain::structure_member> all_members(const http_request& req,
                                                          nats_client& session) {
    trading_messaging::list_structure_members_request msg;
    msg.filter.emplace();
    return forwarding::forward_all(
        req, session, msg, &trading_messaging::list_structure_members_response::members);
}

/// Every counterparty the tenant holds.
std::vector<refdata_domain::counterparty> all_counterparties(const http_request& req,
                                                             nats_client& session) {
    refdata_messaging::list_counterparties_request msg;
    msg.filter.emplace();
    return forwarding::forward_all(
        req, session, msg, &refdata_messaging::list_counterparties_response::counterparties);
}

/// The name each trade type is known by.
std::vector<trading_domain::product_type> all_product_types(const http_request& req,
                                                            nats_client& session) {
    trading_messaging::list_product_types_request msg;
    msg.filter.emplace();
    return forwarding::forward_all(
        req, session, msg, &trading_messaging::list_product_types_response::types);
}

/// The instrument code a desk quotes, and the instrument's own name.
std::vector<refdata_domain::instrument_code> all_instrument_codes(const http_request& req,
                                                                  nats_client& session) {
    refdata_messaging::list_instrument_codes_request msg;
    msg.filter.emplace();
    return forwarding::forward_all(
        req, session, msg, &refdata_messaging::list_instrument_codes_response::instruments);
}

/// The asset classes a desk sorts by.
std::vector<refdata_domain::asset_class_code> all_asset_class_codes(const http_request& req,
                                                                    nats_client& session) {
    refdata_messaging::list_asset_class_codes_request msg;
    msg.filter.emplace();
    return forwarding::forward_all(
        req, session, msg, &refdata_messaging::list_asset_class_codes_response::asset_classes);
}

/// One page of the summaries, as the filter and the order state it.
trading_messaging::list_trade_summaries_response summary_page(const http_request& req,
                                                              nats_client& session,
                                                              const summary_filter& filter,
                                                              const order& sorted,
                                                              std::uint32_t offset,
                                                              std::uint32_t limit) {
    trading_messaging::list_trade_summaries_request msg;
    msg.offset = offset;
    msg.limit = limit;
    msg.order = sorted;
    msg.filter.emplace();
    msg.filter->id_one_of = filter.id_one_of;
    msg.filter->is_leg = filter.is_leg;
    msg.filter->row_kind = filter.row_kind;
    msg.filter->search = filter.search;
    return forwarding::forward(req, session, msg);
}

/// Every row a filter matches, in the order asked for. The rail's figures come
/// from these rows, so a page cannot answer them: a count needs the whole set.
std::vector<trading_domain::trade_summary> all_summaries(const http_request& req,
                                                         nats_client& session,
                                                         const summary_filter& filter,
                                                         const order& sorted) {
    std::vector<trading_domain::trade_summary> rows;
    for (std::uint32_t offset = 0; offset < rail_cap; offset += rail_page) {
        const auto reply = summary_page(req, session, filter, sorted, offset, rail_page);
        rr::require_ok(reply.result);
        rows.insert(rows.end(), reply.trade_summaries.begin(), reply.trade_summaries.end());
        if (reply.trade_summaries.size() < rail_page || rows.size() >= reply.total)
            return rows;
    }
    return rows;
}

/// The reference each structure carries, keyed by the structure's id.
std::map<std::string, std::string> structure_references(
    const http_request& req, nats_client& session, const std::vector<boost::uuids::uuid>& ids) {
    std::map<std::string, std::string> references;
    if (ids.empty())
        return references;
    trading_messaging::list_structure_identifiers_request msg;
    msg.filter.emplace();
    msg.filter->structure_id_one_of = ids;
    const auto rows = forwarding::forward_all(
        req, session, msg, &trading_messaging::list_structure_identifiers_response::identifiers);
    for (const auto& row : rows)
        if (!row.id_value.empty() && !row.structure_id.is_nil())
            references.emplace(rr::as_text(row.structure_id), row.id_value);
    return references;
}

/// The identifier a desk quotes for each trade, best first, keyed by the trade.
std::map<std::string, std::string> trade_references(const http_request& req,
                                                    nats_client& session,
                                                    const std::vector<boost::uuids::uuid>& ids) {
    std::map<std::string, std::string> references;
    if (ids.empty())
        return references;
    trading_messaging::list_trade_identifiers_request msg;
    msg.filter.emplace();
    msg.filter->trade_id_one_of = ids;
    const auto rows = forwarding::forward_all(
        req, session, msg, &trading_messaging::list_trade_identifiers_response::identifiers);
    std::map<std::string, std::pair<int, std::string>> best;
    for (const auto& row : rows) {
        if (row.id_value.empty())
            continue;
        const auto trade = rr::as_text(row.trade_id);
        const auto rank = rank_of(row.id_type);
        const auto held = best.find(trade);
        if (held == best.end() || rank < held->second.first)
            best[trade] = {rank, row.id_value};
    }
    for (const auto& [trade, found] : best)
        references[trade] = found.second;
    return references;
}

/// A node of the rail. Its id says what kind of node it is; the party is the
/// empty id.
struct rail_node final {
    std::string id;
    std::string label;
    std::string kind;
    std::string parent;
    long long count{};
};

std::string book_node(const std::string& id) {
    return "book:" + id;
}

std::string portfolio_node(const std::string& id) {
    return "portfolio:" + id;
}

std::string counterparty_node(const std::string& id) {
    return "counterparty:" + id;
}

std::string instrument_node(const std::string& code) {
    return "instrument:" + code;
}

/// The blotter's query, each member checked as the BFF checked it.
struct query final {
    std::string view = "portfolio";
    std::string node;
    bool flat = false;
    std::string search;
    int offset = 0;
    int limit = 25;
    std::string sort = "trade_date";
    bool descending = true;
};

query query_of(const http_request& req) {
    const std::string sentence =
        "The blotter reads a view of portfolio, counterparty or instrument, a node of up to 100 "
        "characters, a flat of 0 or 1, a search of up to 256 characters, an offset of zero or "
        "more, a limit of 1 to 200, an order by trade_date, instrument_type, status or row_kind, "
        "and a descending of true or false.";
    rr::refuse_unknown_params(
        req, {"view", "node", "flat", "search", "offset", "limit", "sort", "descending"});
    query out;
    const auto param = [&](const std::string& name) {
        return rr::trimmed(req.get_query_param(name));
    };
    if (req.query_params.contains("view")) {
        out.view = param("view");
        if (out.view != "portfolio" && out.view != "counterparty" && out.view != "instrument")
            rr::invalid(sentence);
    }
    if (req.query_params.contains("node")) {
        out.node = param("node");
        if (rr::characters_of(out.node) > 100)
            rr::invalid(sentence);
    }
    if (req.query_params.contains("flat")) {
        const auto flat = param("flat");
        if (flat != "0" && flat != "1")
            rr::invalid(sentence);
        out.flat = flat == "1";
    }
    if (req.query_params.contains("search")) {
        out.search = param("search");
        if (rr::characters_of(out.search) > 256)
            rr::invalid(sentence);
    }
    const auto whole = [&](const std::string& name, int lowest, int highest, int fallback) {
        if (!req.query_params.contains(name))
            return fallback;
        const auto raw = param(name);
        int value = 0;
        const auto [end, error] = std::from_chars(raw.data(), raw.data() + raw.size(), value);
        if (raw.empty() || error != std::errc{} || end != raw.data() + raw.size() ||
            value < lowest || value > highest)
            rr::invalid(sentence);
        return value;
    };
    out.offset = whole("offset", 0, 2147483647, 0);
    out.limit = whole("limit", 1, blotter_most_rows, 25);
    if (req.query_params.contains("sort")) {
        out.sort = param("sort");
        if (out.sort != "trade_date" && out.sort != "instrument_type" && out.sort != "status" &&
            out.sort != "row_kind")
            rr::invalid(sentence);
    }
    if (req.query_params.contains("descending")) {
        const auto descending = param("descending");
        if (descending != "true" && descending != "false")
            rr::invalid(sentence);
        out.descending = descending == "true";
    }
    return out;
}

/// The portfolios above a portfolio, nearest first, ending at a root.
std::vector<std::string> ancestors_of(const std::string& id,
                                      const std::map<std::string, std::string>& parent_of) {
    std::vector<std::string> above;
    const auto start = parent_of.find(id);
    auto parent = start == parent_of.end() ? std::string() : start->second;
    while (!parent.empty() && std::ranges::find(above, parent) == above.end()) {
        above.push_back(parent);
        const auto next = parent_of.find(parent);
        parent = next == parent_of.end() ? std::string() : next->second;
    }
    return above;
}

/// A portfolio and every portfolio under it.
std::vector<std::string> subtree_of(const std::string& id,
                                    const std::map<std::string, std::string>& parent_of) {
    std::vector<std::string> under{id};
    for (std::size_t index = 0; index < under.size(); ++index) {
        const auto current = under[index];
        for (const auto& [candidate, parent] : parent_of)
            if (parent == current && std::ranges::find(under, candidate) == under.end())
                under.push_back(candidate);
    }
    return under;
}

/// The lowest portfolio that holds every one of these portfolios. A set that
/// shares no portfolio above them answers the party, which the empty string
/// states.
std::string lowest_common_ancestor(const std::vector<std::string>& ids,
                                   const std::map<std::string, std::string>& parent_of) {
    if (ids.empty())
        return {};
    const auto first = ids.front();
    std::vector<std::string> candidates{first};
    for (const auto& above : ancestors_of(first, parent_of))
        candidates.push_back(above);
    for (const auto& candidate : candidates) {
        bool holds_all = true;
        for (const auto& id : ids) {
            if (id == candidate)
                continue;
            const auto above = ancestors_of(id, parent_of);
            if (std::ranges::find(above, candidate) == above.end()) {
                holds_all = false;
                break;
            }
        }
        if (holds_all)
            return candidate;
    }
    return {};
}

/// The rail, as the screen reads it.
std::string rail_json(const std::vector<rail_node>& nodes) {
    std::vector<std::string> rows;
    for (const auto& node : nodes)
        rows.push_back("{\"id\":" + json::write(node.id) + ",\"label\":" + json::write(node.label) +
                       ",\"kind\":" + json::write(node.kind) + ",\"parent\":" +
                       json::write(node.parent) + ",\"count\":" + std::to_string(node.count) + "}");
    return rr::array_of(rows);
}

/// One row of the list, as the screen reads it.
std::string row_json(const std::string& row_kind,
                     const std::string& id,
                     const std::string& reference,
                     const std::string& structure,
                     const std::string& structure_reference,
                     const std::string& instrument_type,
                     const std::string& instrument_type_name,
                     const std::string& product_code,
                     const std::string& product_name,
                     const std::string& book_id,
                     const std::string& book_name,
                     const std::string& counterparty_id,
                     const std::string& counterparty_name,
                     const std::string& status,
                     const std::string& trade_date,
                     bool spans_books) {
    return "{\"row_kind\":" + json::write(row_kind) + ",\"id\":" + json::write(id) +
           ",\"reference\":" + json::write(reference) + ",\"structure\":" + json::write(structure) +
           ",\"structure_reference\":" + json::write(structure_reference) +
           ",\"instrument_type\":" + json::write(instrument_type) +
           ",\"instrument_type_name\":" + json::write(instrument_type_name) +
           ",\"product_code\":" + json::write(product_code) +
           ",\"product_name\":" + json::write(product_name) +
           ",\"book_id\":" + json::write(book_id) + ",\"book_name\":" + json::write(book_name) +
           ",\"counterparty_id\":" + json::write(counterparty_id) +
           ",\"counterparty_name\":" + json::write(counterparty_name) +
           ",\"status\":" + json::write(status) + ",\"trade_date\":" + json::write(trade_date) +
           ",\"spans_books\":" + json::write(spans_books) + "}";
}

}

}

http_response trading_view_routes::tree_answer(const http_request& req, std::string body) {
    return ores::http::net::view_answer(
        req,
        std::move(body),
        ores::http::net::view_cache_policy::fresh_for(views::tree_stale_after));
}

http_response trading_view_routes::blotter_answer(const http_request& req, std::string body) {
    return ores::http::net::view_answer(
        req,
        std::move(body),
        ores::http::net::view_cache_policy::fresh_for(views::blotter_stale_after));
}

void trading_view_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using namespace views;
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"trading"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/books/tree")
            .summary("Every portfolio and every book, woven into the tree a person navigates")
            .description("A composed view owned by ores.trading: one read of the portfolios and "
                         "one of the books, woven into the tree. The answer carries an ETag and "
                         "stays fresh for 60 seconds.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<trading_messaging::list_structures_request>([&] {
                    const auto portfolios = all_portfolios(req, session);
                    const auto books = all_books(req, session);
                    return tree_answer(req,
                                       "{\"portfolios\":" + json::write(portfolios) +
                                           ",\"books\":" + json::write(books) + "}");
                });
            }));

    add(router->get("/api/blotter")
            .summary("One page of the blotter, with the rail it is read by")
            .description("A composed view owned by ores.trading: a row is a trade or a whole "
                         "structure, placed on the book it sits in or on the lowest portfolio "
                         "that holds all of its legs. The rail is the grouping the view states and "
                         "each node carries the rows under it. The answer carries an ETag and "
                         "stays fresh for 30 seconds.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<trading_messaging::list_trade_summaries_request>([&] {
                    const auto asked = query_of(req);
                    const order sorted{asked.sort, asked.descending};

                    const auto books = all_books(req, session);
                    const auto portfolios = all_portfolios(req, session);
                    const auto structures = all_structures(req, session);
                    const auto members = all_members(req, session);
                    const auto counterparties = all_counterparties(req, session);
                    const auto types = all_product_types(req, session);
                    const auto instruments = all_instrument_codes(req, session);
                    const auto asset_classes = all_asset_class_codes(req, session);

                    std::map<std::string, std::string> instrument_name_of;
                    for (const auto& row : types) {
                        const auto name = !row.name.empty() ? row.name : row.description;
                        instrument_name_of[row.code] = name.empty() ? readable(row.code) : name;
                    }

                    std::map<std::string, std::string> product_code_of;
                    std::map<std::string, std::string> product_name_of;
                    std::map<std::string, std::string> asset_class_of;
                    for (const auto& row : instruments) {
                        const auto type = text_or_empty(row.ore_trade_type);
                        if (type.empty())
                            continue;
                        product_code_of[type] = row.code;
                        product_name_of[type] = row.name;
                        asset_class_of[type] = text_or_empty(row.asset_class);
                    }
                    std::map<std::string, std::string> class_name_of;
                    for (const auto& row : asset_classes)
                        class_name_of[row.code] = row.name.empty() ? readable(row.code) : row.name;

                    /* Which structure holds which trade. */
                    std::map<std::string, std::string> holder_of;
                    for (const auto& member : members)
                        holder_of[rr::as_text(member.trade_id)] = rr::as_text(member.structure_id);

                    /* A structure may hold another structure, and the legs of the
                     * inner one are as much the outer one's as its own. */
                    std::set<std::string> structure_ids;
                    for (const auto& row : structures)
                        structure_ids.insert(rr::as_text(row.id));
                    std::map<std::string, std::vector<std::string>> inner_of;
                    std::map<std::string, std::vector<std::string>> direct_trades_of;
                    for (const auto& [trade, structure] : holder_of) {
                        if (structure_ids.contains(trade))
                            inner_of[structure].push_back(trade);
                        else
                            direct_trades_of[structure].push_back(trade);
                    }
                    std::map<std::string, std::vector<std::string>> held_by;
                    std::function<std::vector<std::string>(const std::string&,
                                                           std::set<std::string>)>
                        trades_under = [&](const std::string& id,
                                           std::set<std::string> open) -> std::vector<std::string> {
                        const auto held = held_by.find(id);
                        if (held != held_by.end())
                            return held->second;
                        if (open.contains(id))
                            return {};
                        open.insert(id);
                        std::vector<std::string> trades = direct_trades_of[id];
                        const auto inner = inner_of.find(id);
                        if (inner != inner_of.end())
                            for (const auto& child : inner->second) {
                                const auto deeper = trades_under(child, open);
                                trades.insert(trades.end(), deeper.begin(), deeper.end());
                            }
                        held_by[id] = trades;
                        return trades;
                    };

                    std::vector<boost::uuids::uuid> leg_ids;
                    for (const auto& [trade, structure] : holder_of) {
                        (void)structure;
                        leg_ids.push_back(boost::uuids::string_generator()(trade));
                    }
                    summary_filter leg_filter;
                    leg_filter.id_one_of = leg_ids;
                    leg_filter.row_kind = std::string("trade");
                    const auto leg_rows = leg_ids.empty() ?
                                              std::vector<trading_domain::trade_summary>() :
                                              all_summaries(req, session, leg_filter, sorted);

                    /* A structure row states no date and no state of its own:
                     * those are its legs', and the state is the legs' when they
                     * agree. */
                    std::map<std::string, std::string> leg_date_of;
                    std::map<std::string, std::set<std::string>> leg_states_of;
                    std::map<std::string, std::string> book_of_trade;
                    for (const auto& row : leg_rows) {
                        const auto id = rr::as_text(row.id);
                        book_of_trade[id] = rr::as_text(row.book_id);
                        const auto found = holder_of.find(id);
                        if (found == holder_of.end())
                            continue;
                        const auto owner = found->second;
                        const auto date = iso_date(row.trade_date);
                        if (!date.empty()) {
                            const auto held = leg_date_of.find(owner);
                            if (held == leg_date_of.end() || date < held->second)
                                leg_date_of[owner] = date;
                        }
                        const auto state = text_or_empty(row.status);
                        if (!state.empty())
                            leg_states_of[owner].insert(state);
                    }
                    std::map<std::string, std::vector<std::string>> books_of_structure;
                    for (const auto& structure : structure_ids)
                        for (const auto& trade : trades_under(structure, {})) {
                            const auto found = book_of_trade.find(trade);
                            if (found == book_of_trade.end() || found->second.empty())
                                continue;
                            auto& held = books_of_structure[structure];
                            if (std::ranges::find(held, found->second) == held.end())
                                held.push_back(found->second);
                        }

                    std::vector<boost::uuids::uuid> structure_uuids;
                    for (const auto& id : structure_ids)
                        structure_uuids.push_back(boost::uuids::string_generator()(id));
                    const auto references = structure_references(req, session, structure_uuids);

                    std::map<std::string, std::string> portfolio_name_of;
                    std::map<std::string, std::string> parent_of;
                    for (const auto& row : portfolios) {
                        portfolio_name_of[rr::as_text(row.id)] = row.name;
                        parent_of[rr::as_text(row.id)] = rr::as_text(row.parent_portfolio_id);
                    }
                    std::map<std::string, std::string> portfolio_of_book;
                    std::map<std::string, std::string> label_of_book;
                    for (const auto& row : books) {
                        portfolio_of_book[rr::as_text(row.id)] =
                            rr::as_text(row.parent_portfolio_id);
                        label_of_book[rr::as_text(row.id)] =
                            row.name.empty() ? rr::as_text(row.id) : row.name;
                    }
                    std::map<std::string, std::string> label_of_counterparty;
                    for (const auto& row : counterparties)
                        label_of_counterparty[rr::as_text(row.id)] =
                            !row.full_name.empty() ?
                                row.full_name :
                                (!row.short_code.empty() ? row.short_code : rr::as_text(row.id));

                    /* Where a row is placed: a trade on its book, a structure on
                     * the book its legs share or on the lowest portfolio that
                     * holds them. A row with no book sits at the party. */
                    const auto placement_of = [&](const trading_domain::trade_summary& row) {
                        if (row.row_kind != "structure") {
                            const auto book = rr::as_text(row.book_id);
                            return book.empty() ? std::string() : book_node(book);
                        }
                        const auto id = rr::as_text(row.id);
                        const auto found = books_of_structure.find(id);
                        const std::vector<std::string> none;
                        const auto& structure_books =
                            found == books_of_structure.end() ? none : found->second;
                        if (structure_books.empty())
                            return std::string();
                        if (structure_books.size() == 1)
                            return book_node(structure_books.front());
                        std::vector<std::string> holdings;
                        for (const auto& book : structure_books) {
                            const auto held = portfolio_of_book.find(book);
                            if (held != portfolio_of_book.end() && !held->second.empty())
                                holdings.push_back(held->second);
                        }
                        const auto holding = lowest_common_ancestor(holdings, parent_of);
                        return holding.empty() ? std::string() : portfolio_node(holding);
                    };

                    const auto top_rows =
                        all_summaries(req, session, summary_filter{.is_leg = false}, sorted);
                    std::map<std::string, long long> by_node;
                    for (const auto& row : top_rows)
                        by_node[placement_of(row)] += 1;

                    const auto draw_rail = [&]() -> std::vector<rail_node> {
                        const rail_node party_node{.id = std::string(),
                                                   .label = std::string(),
                                                   .kind = "party",
                                                   .parent = std::string(),
                                                   .count =
                                                       static_cast<long long>(top_rows.size())};
                        if (asked.view == "counterparty") {
                            std::map<std::string, long long> counts;
                            for (const auto& row : top_rows) {
                                const auto id = rr::as_text(row.counterparty_id);
                                if (!id.empty())
                                    counts[id] += 1;
                            }
                            std::vector<std::pair<std::string, long long>> ordered(counts.begin(),
                                                                                   counts.end());
                            std::stable_sort(ordered.begin(),
                                             ordered.end(),
                                             [&](const auto& left, const auto& right) {
                                                 return folded(label_of_counterparty[left.first]) <
                                                        folded(label_of_counterparty[right.first]);
                                             });
                            std::vector<rail_node> nodes{party_node};
                            for (const auto& [id, count] : ordered)
                                nodes.push_back(
                                    rail_node{.id = counterparty_node(id),
                                              .label = label_of_counterparty.find(id) ==
                                                               label_of_counterparty.end() ?
                                                           id :
                                                           label_of_counterparty.at(id),
                                              .kind = "counterparty",
                                              .parent = std::string(),
                                              .count = count});
                            return nodes;
                        }
                        if (asked.view == "instrument") {
                            std::map<std::string, long long> counts;
                            long long total = 0;
                            for (const auto& row : top_rows) {
                                const auto found = asset_class_of.find(row.instrument_type);
                                if (found == asset_class_of.end() || found->second.empty())
                                    continue;
                                counts[found->second] += 1;
                                total += 1;
                            }
                            std::vector<std::pair<std::string, long long>> ordered(counts.begin(),
                                                                                   counts.end());
                            std::stable_sort(ordered.begin(),
                                             ordered.end(),
                                             [&](const auto& left, const auto& right) {
                                                 return folded(class_name_of[left.first]) <
                                                        folded(class_name_of[right.first]);
                                             });
                            std::vector<rail_node> nodes{rail_node{.id = std::string(),
                                                                   .label = std::string(),
                                                                   .kind = "party",
                                                                   .parent = std::string(),
                                                                   .count = total}};
                            for (const auto& [code, count] : ordered)
                                nodes.push_back(rail_node{.id = instrument_node(code),
                                                          .label = class_name_of.find(code) ==
                                                                           class_name_of.end() ?
                                                                       readable(code) :
                                                                       class_name_of.at(code),
                                                          .kind = "instrument",
                                                          .parent = std::string(),
                                                          .count = count});
                            return nodes;
                        }
                        std::map<std::string, std::vector<const refdata_domain::book*>> books_under;
                        for (const auto& book : books) {
                            const auto portfolio = rr::as_text(book.parent_portfolio_id);
                            if (!portfolio.empty() && portfolio_name_of.contains(portfolio))
                                books_under[portfolio].push_back(&book);
                        }
                        /* The rail is drawn in the order the portfolios and books
                         * were read, as the BFF drew it, so a screen shows the
                         * catalogue's order and not a uuid sort. */
                        std::set<std::string> shown;
                        std::vector<std::string> shown_order;
                        for (const auto& row : portfolios) {
                            const auto id = rr::as_text(row.id);
                            if (!books_under.contains(id) || !shown.insert(id).second)
                                continue;
                            shown_order.push_back(id);
                        }
                        for (std::size_t index = 0; index < shown_order.size(); ++index) {
                            auto parent = parent_of[shown_order[index]];
                            while (!parent.empty() && portfolio_name_of.contains(parent) &&
                                   !shown.contains(parent)) {
                                shown.insert(parent);
                                shown_order.push_back(parent);
                                parent = parent_of[parent];
                            }
                        }
                        std::map<std::string, std::vector<std::string>> children_of;
                        for (const auto& id : shown_order) {
                            const auto parent = parent_of[id];
                            const auto key =
                                !parent.empty() && shown.contains(parent) ? parent : std::string();
                            children_of[key].push_back(id);
                        }
                        const auto count_under = [&](const std::string& node) {
                            if (node.empty())
                                return static_cast<long long>(top_rows.size());
                            if (node.starts_with("book:"))
                                return by_node[node];
                            const auto portfolio = node.substr(std::string("portfolio:").size());
                            const auto below = subtree_of(portfolio, parent_of);
                            long long total = by_node[node];
                            for (std::size_t i = 1; i < below.size(); ++i)
                                total += by_node[portfolio_node(below[i])];
                            for (const auto& book : books) {
                                const auto holder = rr::as_text(book.parent_portfolio_id);
                                if (std::ranges::find(below, holder) == below.end())
                                    continue;
                                total += by_node[book_node(rr::as_text(book.id))];
                            }
                            return total;
                        };
                        std::vector<rail_node> nodes{party_node};
                        std::function<void(const std::string&)> walk =
                            [&](const std::string& parent) {
                                const auto children = children_of.find(parent);
                                if (children == children_of.end())
                                    return;
                                for (const auto& id : children->second) {
                                    const auto found = portfolio_name_of.find(id);
                                    const auto name = found == portfolio_name_of.end() ?
                                                          std::string() :
                                                          found->second;
                                    nodes.push_back(
                                        rail_node{.id = portfolio_node(id),
                                                  .label = name.empty() ? id : name,
                                                  .kind = "portfolio",
                                                  .parent = parent.empty() ? std::string() :
                                                                             portfolio_node(parent),
                                                  .count = count_under(portfolio_node(id))});
                                    const auto held = books_under.find(id);
                                    if (held != books_under.end())
                                        for (const auto* book : held->second) {
                                            const auto book_id = rr::as_text(book->id);
                                            nodes.push_back(rail_node{
                                                .id = book_node(book_id),
                                                .label = book->name.empty() ? book_id : book->name,
                                                .kind = "book",
                                                .parent = portfolio_node(id),
                                                .count = count_under(book_node(book_id))});
                                        }
                                    walk(id);
                                }
                            };
                        walk(std::string());
                        return nodes;
                    };
                    std::vector<rail_node> rail;
                    for (auto& node : draw_rail())
                        if (node.id.empty() || node.count > 0)
                            rail.push_back(node);

                    /* The rows the list is read from: every trade when flat, the
                     * top rows otherwise. */
                    summary_filter list_filter;
                    if (asked.flat)
                        list_filter.row_kind = std::string("trade");
                    else
                        list_filter.is_leg = false;
                    if (!asked.search.empty())
                        list_filter.search = asked.search;
                    const auto listed = (asked.flat || !asked.search.empty()) ?
                                            all_summaries(req, session, list_filter, sorted) :
                                            top_rows;

                    /* The chain a placement sits under: itself, the portfolios above
                     * it and the party. */
                    const auto chain_of = [&](const std::string& placement) {
                        std::vector<std::string> chain{placement};
                        std::string portfolio;
                        if (placement.starts_with("book:")) {
                            const auto held = portfolio_of_book.find(
                                placement.substr(std::string("book:").size()));
                            portfolio =
                                held == portfolio_of_book.end() ? std::string() : held->second;
                        } else if (placement.starts_with("portfolio:")) {
                            portfolio = placement.substr(std::string("portfolio:").size());
                        }
                        if (!portfolio.empty()) {
                            chain.push_back(portfolio_node(portfolio));
                            for (const auto& above : ancestors_of(portfolio, parent_of))
                                chain.push_back(portfolio_node(above));
                        }
                        chain.push_back(std::string());
                        return chain;
                    };
                    const auto within = [&](const trading_domain::trade_summary& row) {
                        if (asked.node.empty())
                            return true;
                        if (asked.node.starts_with("counterparty:"))
                            return rr::as_text(row.counterparty_id) ==
                                   asked.node.substr(std::string("counterparty:").size());
                        if (asked.node.starts_with("instrument:")) {
                            const auto found = asset_class_of.find(row.instrument_type);
                            return found != asset_class_of.end() &&
                                   found->second ==
                                       asked.node.substr(std::string("instrument:").size());
                        }
                        const auto chain = chain_of(placement_of(row));
                        return std::ranges::find(chain, asked.node) != chain.end();
                    };
                    std::vector<const trading_domain::trade_summary*> rows;
                    for (const auto& row : listed)
                        if (within(row))
                            rows.push_back(&row);

                    const auto field_of = [&](const trading_domain::trade_summary& row) {
                        if (asked.sort == "trade_date") {
                            if (row.row_kind != "structure")
                                return iso_date(row.trade_date);
                            const auto found = leg_date_of.find(rr::as_text(row.id));
                            return found == leg_date_of.end() ? std::string() : found->second;
                        }
                        if (asked.sort == "status") {
                            if (row.row_kind != "structure")
                                return text_or_empty(row.status);
                            const auto found = leg_states_of.find(rr::as_text(row.id));
                            return found != leg_states_of.end() && found->second.size() == 1 ?
                                       *found->second.begin() :
                                       std::string();
                        }
                        if (asked.sort == "instrument_type")
                            return row.instrument_type;
                        return row.row_kind;
                    };
                    std::stable_sort(
                        rows.begin(), rows.end(), [&](const auto* left, const auto* right) {
                            const auto a = field_of(*left);
                            const auto b = field_of(*right);
                            if (a == b)
                                return rr::as_text(left->id) < rr::as_text(right->id);
                            return asked.descending ? b < a : a < b;
                        });

                    /* A trade is found by its identifiers, not by the surrogate key
                     * the summary carries. */
                    std::vector<boost::uuids::uuid> page_trade_ids;
                    for (const auto* row : rows)
                        if (row->row_kind == "trade")
                            page_trade_ids.push_back(row->id);
                    const auto trade_reference_of = trade_references(req, session, page_trade_ids);

                    std::vector<std::string> page;
                    const auto first = static_cast<std::size_t>(asked.offset);
                    for (std::size_t i = first;
                         i < rows.size() && i < first + static_cast<std::size_t>(asked.limit);
                         ++i) {
                        const auto& row = *rows[i];
                        const auto id = rr::as_text(row.id);
                        const auto kind = row.row_kind;
                        const auto found = holder_of.find(id);
                        const auto structure =
                            found == holder_of.end() ? std::string() : found->second;
                        const auto reference =
                            kind == "structure" ?
                                (references.contains(id) ? references.at(id) : std::string()) :
                                (trade_reference_of.contains(id) ? trade_reference_of.at(id) :
                                                                   std::string());
                        const auto structure_reference =
                            structure.empty() ?
                                std::string() :
                                (references.contains(structure) ? references.at(structure) :
                                                                  std::string());
                        std::string instrument_type_name;
                        if (kind == "structure") {
                            instrument_type_name = row.instrument_type;
                        } else {
                            const auto named = instrument_name_of.find(row.instrument_type);
                            instrument_type_name = named == instrument_name_of.end() ?
                                                       readable(row.instrument_type) :
                                                       named->second;
                        }
                        const auto code = product_code_of.find(row.instrument_type);
                        const auto product = product_name_of.find(row.instrument_type);
                        std::string book_name;
                        if (kind == "structure") {
                            const auto held = books_of_structure.find(id);
                            if (held != books_of_structure.end()) {
                                bool first_book = true;
                                for (const auto& book : held->second) {
                                    const auto label = label_of_book.find(book);
                                    if (label == label_of_book.end() || label->second.empty())
                                        continue;
                                    if (!first_book)
                                        book_name += " + ";
                                    book_name += label->second;
                                    first_book = false;
                                }
                            }
                        } else {
                            const auto label = label_of_book.find(rr::as_text(row.book_id));
                            book_name =
                                label == label_of_book.end() ? std::string() : label->second;
                        }
                        std::string status;
                        if (kind == "structure") {
                            const auto held = leg_states_of.find(id);
                            if (held != leg_states_of.end() && held->second.size() == 1)
                                status = *held->second.begin();
                        } else {
                            status = text_or_empty(row.status);
                        }
                        std::string trade_date;
                        if (kind == "structure") {
                            const auto held = leg_date_of.find(id);
                            trade_date = held == leg_date_of.end() ? std::string() : held->second;
                        } else {
                            trade_date = iso_date(row.trade_date);
                        }
                        const auto counterparty_id = rr::as_text(row.counterparty_id);
                        const auto counterparty = label_of_counterparty.find(counterparty_id);
                        const auto held_books = books_of_structure.find(id);
                        page.push_back(row_json(
                            kind,
                            id,
                            reference,
                            structure,
                            structure_reference,
                            row.instrument_type,
                            instrument_type_name,
                            code == product_code_of.end() ? std::string() : code->second,
                            product == product_name_of.end() ? std::string() : product->second,
                            rr::as_text(row.book_id),
                            book_name,
                            counterparty_id,
                            counterparty == label_of_counterparty.end() ? std::string() :
                                                                          counterparty->second,
                            status,
                            trade_date,
                            held_books != books_of_structure.end() &&
                                held_books->second.size() > 1));
                    }
                    return blotter_answer(req,
                                          "{\"tree\":" + rail_json(rail) +
                                              ",\"rows\":" + rr::array_of(page) +
                                              ",\"total\":" + std::to_string(rows.size()) +
                                              ",\"flat\":" + json::write(asked.flat) +
                                              ",\"view\":" + json::write(asked.view) +
                                              ",\"node\":" + json::write(asked.node) + "}");
                });
            }));
}

}
