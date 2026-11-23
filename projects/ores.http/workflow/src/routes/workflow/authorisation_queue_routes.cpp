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
#include "ores.http/routes/workflow/authorisation_queue_routes.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/view_answer.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/workflow/page_query.hpp"
#include "ores.http/routes/workflow/request_joins.hpp"
#include "ores.inbox.api/messaging/approval_operations_protocol.hpp"
#include "ores.logging/logger.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/party_product_type_deal_class_protocol.hpp"
#include "ores.trading.api/messaging/product_type_protocol.hpp"
#include "ores.trading.api/messaging/trade_authorisation_request_protocol.hpp"
#include "ores.trading.api/messaging/trade_identifier_protocol.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.trading.api/messaging/trade_summary_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/text_form.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cstdint>
#include <format>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::http::routes::workflow {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace inbox_messaging = ores::inbox::messaging;
namespace trading_messaging = ores::trading::messaging;
namespace refdata_messaging = ores::refdata::messaging;
namespace trading_domain = ores::trading::domain;
namespace refdata_domain = ores::refdata::domain;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;

/// The unit's own helpers, named apart from the other units that build into
/// one translation unit with it.
namespace queue {

/// The kind of approval request the Operations queue answers.
constexpr std::string_view authorisation_kind = "trading.operational_authorisation";

/// The class a deal nobody has classified reads as: the closer look.
constexpr std::string_view default_class = "non_flow";

/// The class a deal worked as flow reads as on the row.
constexpr std::string_view flow_class = "flow";

/// The most rows one page may ask for. A page costs a join per row.
constexpr std::uint32_t page_max = 100;

/// The page the queue answers with when the caller states none.
constexpr std::uint32_t page_default = 50;

/// The sentence a page states when it is not one.
constexpr std::string_view page_refused = "A page names an offset and a limit of 1 to 100.";

auto& queue_lg() {
    static auto instance =
        ores::logging::make_logger("ores.http.workflow.authorisation_queue_routes");
    return instance;
}

/// The instant as the wire states it.
std::string instant_text(const std::chrono::system_clock::time_point& at) {
    return ores::utility::codec::text_form<std::chrono::system_clock::time_point>::to_text(at);
}

/// One request of the page and the deal it names, as trading keeps the pair.
struct authorisation_pair final {
    std::string request_id;
    std::string trade_id;
    int trade_version = 0;
};

/// What the queue states about one deal, read beside the page.
struct deal_facts final {
    std::string trade_type;
    std::string trade_type_name;
    std::string class_code;
    std::string book_id;
    std::string counterparty_id;
    std::string book;
    std::string counterparty;
    std::string trade_date;
    std::string status;
    int trade_version = 0;
};

/// The class a deal is worked as: the party's own line first, the type's second,
/// and the default for a type nobody has classified.
std::string class_of(const std::string& trade_type,
                     const std::string& party_id,
                     const std::map<std::string, std::string>& type_classes,
                     const std::map<std::string, std::string>& party_classes) {
    const auto stated = party_classes.find(party_id + "/" + trade_type);
    if (stated != party_classes.end())
        return stated->second == flow_class ? std::string(flow_class) : std::string("non-flow");
    const auto held = type_classes.find(trade_type);
    const auto value = held == type_classes.end() ? std::string(default_class) : held->second;
    return value == flow_class ? std::string(flow_class) : std::string("non-flow");
}

/**
 * The deals a page of authorisation requests names, and nothing else.
 *
 * An authorisation request names its trade and the version it was raised
 * against, and nothing about the deal itself, so the book, the counterparty, the
 * trade date, the status and the trade's current version are read here for the
 * whole page: six reads rather than one per row.
 *
 * The version is what makes an amendment visible. A request raised at version
 * three and read against a trade now at version four is out of date, and the row
 * says so: the deal moved while it waited.
 */
std::map<std::string, deal_facts>
deals_of(const http_request& req, nats_client& session, const std::vector<std::string>& trade_ids) {
    std::map<std::string, deal_facts> facts;
    if (trade_ids.empty())
        return facts;

    std::vector<boost::uuids::uuid> ids;
    ids.reserve(trade_ids.size());
    for (const auto& id : trade_ids)
        ids.push_back(boost::uuids::string_generator()(id));

    trading_messaging::list_trades_request trades_request;
    trades_request.filter.emplace();
    trades_request.filter->id_one_of = ids;
    const auto trades = forwarding::forward_all(
        req, session, trades_request, &trading_messaging::list_trades_response::trades);

    trading_messaging::list_trade_summaries_request summaries_request;
    summaries_request.filter.emplace();
    summaries_request.filter->id_one_of = ids;
    const auto summaries =
        forwarding::forward_all(req,
                                session,
                                summaries_request,
                                &trading_messaging::list_trade_summaries_response::trade_summaries);

    const auto books = forwarding::forward_all(req,
                                               session,
                                               refdata_messaging::list_books_request{},
                                               &refdata_messaging::list_books_response::books);
    const auto counterparties =
        forwarding::forward_all(req,
                                session,
                                refdata_messaging::list_counterparties_request{},
                                &refdata_messaging::list_counterparties_response::counterparties);
    const auto types =
        forwarding::forward_all(req,
                                session,
                                trading_messaging::list_product_types_request{},
                                &trading_messaging::list_product_types_response::types);

    std::set<std::string> party_ids;
    for (const auto& trade : trades)
        party_ids.insert(boost::uuids::to_string(trade.party_id));
    refdata_messaging::list_party_product_type_deal_classes_request classes_request;
    classes_request.filter.emplace();
    classes_request.filter->party_id_one_of.emplace();
    for (const auto& party_id : party_ids)
        if (rr::is_uuid(party_id))
            classes_request.filter->party_id_one_of->push_back(
                boost::uuids::string_generator()(party_id));
    const auto party_classes =
        forwarding::forward_all(req,
                                session,
                                classes_request,
                                &refdata_messaging::list_party_product_type_deal_classes_response::
                                    party_product_type_deal_classes);

    std::map<std::string, std::string> book_names;
    for (const auto& book : books)
        book_names.emplace(boost::uuids::to_string(book.id), book.name);
    std::map<std::string, std::string> counterparty_names;
    for (const auto& counterparty : counterparties)
        counterparty_names.emplace(boost::uuids::to_string(counterparty.id),
                                   counterparty.full_name);
    std::map<std::string, std::string> type_names;
    std::map<std::string, std::string> type_classes;
    for (const auto& type : types) {
        type_names.emplace(type.code, type.name.empty() ? type.description : type.name);
        type_classes.emplace(type.code, type.deal_class);
    }
    std::map<std::string, std::string> party_class_of;
    for (const auto& deal_class : party_classes)
        party_class_of.emplace(boost::uuids::to_string(deal_class.party_id) + "/" +
                                   deal_class.product_type_code,
                               deal_class.deal_class);

    std::map<std::string, const trading_domain::trade_summary*> summary_of;
    for (const auto& summary : summaries)
        summary_of.emplace(boost::uuids::to_string(summary.id), &summary);

    for (const auto& trade : trades) {
        const auto id = boost::uuids::to_string(trade.id);
        const auto found = summary_of.find(id);
        const auto* summary = found == summary_of.end() ? nullptr : found->second;
        deal_facts row;
        row.trade_version = trade.version;
        row.trade_type = trade.trade_type;
        const auto named = type_names.find(trade.trade_type);
        row.trade_type_name = named == type_names.end() ? std::string{} : named->second;
        row.class_code = class_of(trade.trade_type,
                                  boost::uuids::to_string(trade.party_id),
                                  type_classes,
                                  party_class_of);
        if (summary != nullptr) {
            if (summary->book_id) {
                row.book_id = boost::uuids::to_string(*summary->book_id);
                const auto book = book_names.find(row.book_id);
                row.book = book == book_names.end() ? std::string{} : book->second;
            }
            if (summary->counterparty_id) {
                row.counterparty_id = boost::uuids::to_string(*summary->counterparty_id);
                const auto counterparty = counterparty_names.find(row.counterparty_id);
                row.counterparty =
                    counterparty == counterparty_names.end() ? std::string{} : counterparty->second;
            }
            if (summary->trade_date)
                row.trade_date = std::format("{}", *summary->trade_date);
            row.status = summary->status.value_or("");
        }
        facts.emplace(id, std::move(row));
    }
    return facts;
}

/// The identifier a person recognises each trade by; a trade with none keeps its uuid.
std::map<std::string, std::string> references_of(const http_request& req,
                                                 nats_client& session,
                                                 const std::vector<std::string>& trade_ids) {
    std::map<std::string, std::string> references;
    if (trade_ids.empty())
        return references;
    trading_messaging::list_trade_identifiers_request msg;
    msg.filter.emplace();
    msg.filter->trade_id_one_of.emplace();
    for (const auto& id : trade_ids)
        if (rr::is_uuid(id))
            msg.filter->trade_id_one_of->push_back(boost::uuids::string_generator()(id));
    const auto identifiers = forwarding::forward_all(
        req, session, msg, &trading_messaging::list_trade_identifiers_response::identifiers);
    for (const auto& identifier : identifiers) {
        const auto id = boost::uuids::to_string(identifier.trade_id);
        if (!identifier.id_value.empty())
            references.emplace(id, identifier.id_value);
    }
    return references;
}

}

}

std::string authorisation_queue_routes::row_json(const queue_row& row) {
    return "{\"requestId\":" + json::write(row.request_id) +
           ",\"requestVersion\":" + std::to_string(row.request_version) +
           ",\"kindCode\":" + json::write(row.kind_code) +
           ",\"stateCode\":" + json::write(row.state_code) +
           ",\"requestedBy\":" + json::write(row.requested_by) +
           ",\"requestedAt\":" + json::write(row.requested_at) +
           ",\"reason\":" + json::write(row.reason) + ",\"tradeId\":" + json::write(row.trade_id) +
           ",\"reference\":" + json::write(row.reference) +
           ",\"requestTradeVersion\":" + std::to_string(row.request_trade_version) +
           ",\"tradeVersion\":" + std::to_string(row.trade_version) +
           ",\"changed\":" + (row.trade_version > row.request_trade_version ? "true" : "false") +
           ",\"classCode\":" + json::write(row.class_code) +
           ",\"bookId\":" + json::write(row.book_id) +
           ",\"counterpartyId\":" + json::write(row.counterparty_id) +
           ",\"tradeType\":" + json::write(row.trade_type) +
           ",\"tradeTypeName\":" + json::write(row.trade_type_name) +
           ",\"book\":" + json::write(row.book) +
           ",\"counterparty\":" + json::write(row.counterparty) +
           ",\"tradeDate\":" + json::write(row.trade_date) +
           ",\"status\":" + json::write(row.status) + "}";
}

http_response authorisation_queue_routes::queue_answer(const http_request& req, std::string body) {
    return ores::http::net::view_answer(
        req, std::move(body), ores::http::net::view_cache_policy::not_stored());
}

void authorisation_queue_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using namespace queue;
    using forwarding::guarded;
    const auto built =
        router->get("/api/queue/authorisations").catalogue("web_get_queue_authorisations", "operations", "authorisations", false)
            .summary("The authorisation queue, with the deal each request names")
            .description("A composed view owned by ores.inbox: the requests this person may "
                         "decide, read from the inbox service, and the answered tail beside "
                         "them. A request names its trade and the version it was raised against, "
                         "so the book, the counterparty, the trade date, the status and the "
                         "trade's version now are read here for the whole page, with the "
                         "identifier a person knows each trade by and the class it is worked as. "
                         "A row whose deal moved says so and stays open. A queue with no "
                         "authorisation request answers empty. The answer is not kept: it states "
                         "what this person may decide now.")
            .tags({"workflow"})
            .auth_required()
            .query_param("offset", "integer", "int32", false, "How many requests to skip")
            .query_param("limit", "integer", "int32", false, "How many requests to return")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<inbox_messaging::list_approval_queue_request>([&] {
                    const auto asked = page_of(req, page_default, page_max, page_refused);

                    inbox_messaging::list_approval_queue_request msg;
                    msg.offset = static_cast<int>(asked.offset);
                    msg.limit = static_cast<int>(asked.limit);
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);

                    std::vector<const ores::inbox::domain::approval_request*> open;
                    std::vector<const ores::inbox::domain::approval_request*> answered;
                    for (const auto& request : reply.requests)
                        if (request.kind_code == authorisation_kind)
                            open.push_back(&request);
                    for (const auto& request : reply.answered)
                        if (request.kind_code == authorisation_kind)
                            answered.push_back(&request);

                    if (open.empty() && answered.empty())
                        return queue_answer(req,
                                            "{\"items\":[],\"answered\":[],\"total\":" +
                                                std::to_string(reply.total) + "}");

                    std::vector<const ores::inbox::domain::approval_request*> authored = open;
                    authored.insert(authored.end(), answered.begin(), answered.end());

                    std::vector<boost::uuids::uuid> request_ids;
                    request_ids.reserve(authored.size());
                    for (const auto* request : authored)
                        request_ids.push_back(request->id);

                    std::map<std::string, std::vector<authorisation_pair>> pairs_of;
                    {
                        trading_messaging::list_trade_authorisation_requests_request pair_request;
                        pair_request.filter.emplace();
                        pair_request.filter->request_id_one_of = request_ids;
                        const auto pairs = forwarding::forward_all(
                            req,
                            session,
                            pair_request,
                            &trading_messaging::list_trade_authorisation_requests_response::
                                trade_authorisation_requests);
                        for (const auto& pair : pairs) {
                            if (pair.request_id.is_nil() || pair.trade_id.is_nil())
                                continue;
                            pairs_of[boost::uuids::to_string(pair.request_id)].push_back(
                                authorisation_pair{boost::uuids::to_string(pair.request_id),
                                                   boost::uuids::to_string(pair.trade_id),
                                                   pair.trade_version});
                        }
                    }

                    std::vector<std::string> trade_ids;
                    for (const auto& [request_id, pairs] : pairs_of)
                        for (const auto& pair : pairs)
                            trade_ids.push_back(pair.trade_id);
                    const auto references = references_of(req, session, trade_ids);
                    const auto deals = deals_of(req, session, trade_ids);

                    std::set<std::string> accounts;
                    for (const auto* request : authored)
                        accounts.insert(rr::as_text(request->requested_by));
                    const auto usernames = joins::usernames_of(req, session, accounts);

                    const auto rows_of =
                        [&](const std::vector<const ores::inbox::domain::approval_request*>&
                                requests) {
                            std::vector<std::string> written;
                            for (const auto* request : requests) {
                                const auto id = boost::uuids::to_string(request->id);
                                const auto found = pairs_of.find(id);
                                if (found == pairs_of.end())
                                    continue;
                                const auto raised_by = rr::as_text(request->requested_by);
                                const auto account = usernames.find(raised_by);
                                const auto requested_by =
                                    account == usernames.end() ? raised_by : account->second;
                                for (const auto& pair : found->second) {
                                    const auto deal = deals.find(pair.trade_id);
                                    const deal_facts none;
                                    const auto& facts = deal == deals.end() ? none : deal->second;
                                    const auto reference = references.find(pair.trade_id);
                                    queue_row row;
                                    row.request_id = id;
                                    row.request_version = request->version;
                                    row.kind_code = request->kind_code;
                                    row.state_code = request->state_code;
                                    row.requested_by = requested_by;
                                    row.requested_at = instant_text(request->requested_at);
                                    row.reason = request->reason;
                                    row.trade_id = pair.trade_id;
                                    row.reference = reference == references.end() ?
                                                        pair.trade_id :
                                                        reference->second;
                                    row.request_trade_version = pair.trade_version;
                                    row.trade_version = facts.trade_version;
                                    row.class_code = facts.class_code;
                                    row.book_id = facts.book_id;
                                    row.counterparty_id = facts.counterparty_id;
                                    row.trade_type = facts.trade_type;
                                    row.trade_type_name = facts.trade_type_name;
                                    row.book = facts.book;
                                    row.counterparty = facts.counterparty;
                                    row.trade_date = facts.trade_date;
                                    row.status = facts.status;
                                    written.push_back(row_json(row));
                                }
                            }
                            return rr::array_of(written);
                        };

                    const auto items = rows_of(open);
                    const auto tail = rows_of(answered);
                    ORES_LOG_SEV(queue_lg(), ores::logging::debug)
                        << "The queue holds " << open.size() << " open requests and "
                        << answered.size() << " answered, and wrote " << items << " and " << tail;
                    return queue_answer(req,
                                        "{\"items\":" + items + ",\"answered\":" + tail +
                                            ",\"total\":" + std::to_string(reply.total) + "}");
                });
            })
            .build();
    router->add_route(built);
    registry->register_route(built);
}

}
