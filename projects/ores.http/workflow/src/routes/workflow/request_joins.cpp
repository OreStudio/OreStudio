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
#include "ores.http/routes/workflow/request_joins.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.iam.api/messaging/role_grant_request_protocol.hpp"
#include "ores.iam.api/messaging/role_request_operations_protocol.hpp"
#include "ores.inbox.api/messaging/approval_decision_protocol.hpp"
#include "ores.inbox.api/messaging/approval_operations_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/messaging/trade_authorisation_request_protocol.hpp"
#include "ores.trading.api/messaging/trade_identifier_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/uuid/string_generator.hpp>
#include <cstddef>
#include <cstdint>
#include <exception>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::http::routes::workflow::joins {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace inbox_domain = ores::inbox::domain;
namespace inbox_messaging = ores::inbox::messaging;
namespace iam_domain = ores::iam::domain;
namespace iam_messaging = ores::iam::messaging;
namespace trading_messaging = ores::trading::messaging;

using ores::http::domain::http_request;
using ores::nats::service::nats_client;

/// The trail a read leaves when a join of a page is deliberately absent.
auto& join_lg() {
    static auto instance = ores::logging::make_logger("ores.http.workflow.request_joins");
    return instance;
}

/// The most trades one request may name, as the screen draws them.
constexpr std::size_t trades_of_request = 10;

/// The label of one trade: its first identifier, or the identifier itself.
std::string label_of(const http_request& req, nats_client& session, const std::string& trade_id) {
    if (!rr::is_uuid(trade_id))
        return trade_id;
    try {
        trading_messaging::list_trade_identifiers_request msg;
        msg.limit = join_page;
        msg.filter.emplace();
        msg.filter->trade_id_one_of = std::vector{boost::uuids::string_generator()(trade_id)};
        const auto reply = forwarding::forward(req, session, msg);
        rr::require_ok(reply.result);
        for (const auto& identifier : reply.identifiers)
            if (!identifier.id_value.empty())
                return identifier.id_value;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(join_lg(), ores::logging::debug)
            << "The label of trade " << trade_id << " was not read: " << e.what();
    }
    return trade_id;
}

}

/**
 * @brief The roles one request asks for, or none when the caller may not read them.
 *
 * The store serves no batched read of a request's roles, so a page costs one
 * read per row; a refusal costs that row's roles and not the page.
 */
std::vector<iam_messaging::requested_role>
roles_of(const http_request& req, nats_client& session, const std::string& request_id) {
    try {
        iam_messaging::get_request_roles_request msg;
        msg.request_id = request_id;
        const auto reply = forwarding::forward(req, session, msg);
        if (reply.result.outcome != ores::utility::domain::outcome::ok)
            return {};
        return reply.roles;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(join_lg(), ores::logging::warn)
            << "The roles of request " << request_id << " were not read: " << e.what();
        return {};
    }
}

/// The account that raised each request, by the request's id.
std::map<std::string, std::string>
requesters_of(const http_request& req,
              nats_client& session,
              const std::vector<boost::uuids::uuid>& request_ids) {
    std::map<std::string, std::string> requesters;
    if (request_ids.empty())
        return requesters;
    try {
        iam_messaging::list_role_grant_requests_request msg;
        msg.limit = join_page;
        msg.filter.emplace();
        msg.filter->request_id_one_of = request_ids;
        const auto reply = forwarding::forward(req, session, msg);
        rr::require_ok(reply.result);
        for (const auto& grant : reply.role_grant_requests)
            requesters.emplace(rr::as_text(grant.request_id), rr::as_text(grant.account_id));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(join_lg(), ores::logging::warn)
            << "The accounts of the requests were not read: " << e.what();
        return requesters;
    }
    return requesters;
}

/// The username of each account named, for the accounts this caller may read.
std::map<std::string, std::string> usernames_of(const http_request& req,
                                                nats_client& session,
                                                const std::set<std::string>& account_ids) {
    std::map<std::string, std::string> usernames;
    if (account_ids.empty())
        return usernames;

    std::vector<boost::uuids::uuid> wanted;
    wanted.reserve(account_ids.size());
    for (const auto& id : account_ids) {
        if (rr::is_uuid(id))
            wanted.push_back(boost::uuids::string_generator()(id));
    }
    if (wanted.empty())
        return usernames;

    try {
        iam_messaging::list_accounts_request msg;
        msg.limit = join_page;
        msg.filter.emplace();
        msg.filter->id_one_of = std::move(wanted);
        const auto reply = forwarding::forward(req, session, msg);
        rr::require_ok(reply.result);
        for (const auto& account : reply.accounts)
            usernames.emplace(rr::as_text(account.id), account.username);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(join_lg(), ores::logging::warn)
            << "The usernames of the accounts were not read: " << e.what();
        return usernames;
    }
    return usernames;
}

/// The latest decision on each request, by the request's id.
std::map<std::string, inbox_domain::approval_decision>
decisions_of(const http_request& req,
             nats_client& session,
             const std::vector<boost::uuids::uuid>& request_ids) {
    std::map<std::string, inbox_domain::approval_decision> decisions;
    if (request_ids.empty())
        return decisions;
    try {
        inbox_messaging::list_approval_decisions_request msg;
        msg.limit = join_page;
        msg.filter.emplace();
        msg.filter->request_id_one_of = request_ids;
        const auto reply = forwarding::forward(req, session, msg);
        rr::require_ok(reply.result);
        for (const auto& decision : reply.decisions) {
            const auto key = rr::as_text(decision.request_id);
            const auto found = decisions.find(key);
            if (found != decisions.end() && found->second.decided_at >= decision.decided_at)
                continue;
            decisions.insert_or_assign(key, decision);
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(join_lg(), ores::logging::warn)
            << "The decisions on the requests were not read: " << e.what();
        return decisions;
    }
    return decisions;
}

/// The trades each request of a page is about, by the request's id.
std::map<std::string, std::vector<subject>>
subjects_of_page(const http_request& req,
                 nats_client& session,
                 const std::vector<boost::uuids::uuid>& request_ids) {
    std::map<std::string, std::vector<subject>> by_request;
    if (request_ids.empty())
        return by_request;

    std::vector<std::pair<std::string, std::pair<std::string, int>>> links;
    try {
        trading_messaging::list_trade_authorisation_requests_request msg;
        msg.limit = join_page;
        msg.filter.emplace();
        msg.filter->request_id_one_of = request_ids;
        const auto reply = forwarding::forward(req, session, msg);
        rr::require_ok(reply.result);
        links.reserve(reply.trade_authorisation_requests.size());
        for (const auto& link : reply.trade_authorisation_requests)
            links.emplace_back(rr::as_text(link.request_id),
                               std::pair{rr::as_text(link.trade_id), link.trade_version});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(join_lg(), ores::logging::warn)
            << "The trades of a page of requests were not read: " << e.what();
        return by_request;
    }

    // The identifiers of every trade the page names, in one read: the label is
    // the name a person recognises, and a refusal costs the label and not the row.
    std::map<std::string, std::string> labels;
    std::vector<boost::uuids::uuid> trade_ids;
    std::set<std::string> seen;
    for (const auto& [request_id, link] : links)
        if (seen.insert(link.first).second && rr::is_uuid(link.first))
            trade_ids.push_back(boost::uuids::string_generator()(link.first));
    if (!trade_ids.empty()) {
        try {
            trading_messaging::list_trade_identifiers_request msg;
            msg.limit = static_cast<std::uint32_t>(trade_ids.size());
            msg.filter.emplace();
            msg.filter->trade_id_one_of = trade_ids;
            const auto reply = forwarding::forward(req, session, msg);
            rr::require_ok(reply.result);
            for (const auto& identifier : reply.identifiers) {
                const auto trade_id = rr::as_text(identifier.trade_id);
                if (!identifier.id_value.empty() && !labels.contains(trade_id))
                    labels.emplace(trade_id, identifier.id_value);
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(join_lg(), ores::logging::warn)
                << "The labels of a page of trades were not read: " << e.what();
        }
    }

    for (const auto& [request_id, link] : links) {
        auto& subjects = by_request[request_id];
        if (subjects.size() == trades_of_request)
            continue;
        const auto label = labels.find(link.first);
        subjects.push_back(
            {link.first, link.second, label == labels.end() ? link.first : label->second});
    }
    return by_request;
}

/// One role a request asks for, as the screen draws it.
std::string role_json(const iam_domain::role& role) {
    return "{\"roleId\":" + json::write(rr::as_text(role.id)) +
           ",\"name\":" + json::write(role.name) +
           ",\"description\":" + json::write(role.description) + "}";
}

/// What was decided about a request, with the decider named where they can be.
std::string decision_json(const inbox_domain::approval_decision& decision,
                          const std::map<std::string, std::string>& usernames) {
    const auto decider = rr::as_text(decision.decided_by);
    const auto named = usernames.find(decider);
    return "{\"decisionCode\":" + json::write(decision.decision_code) +
           ",\"decidedBy\":" + json::write(named == usernames.end() ? decider : named->second) +
           ",\"decidedAt\":" + json::write(decision.decided_at) +
           ",\"comment\":" + json::write(decision.comment) + "}";
}

/// Where each part of one request stands, or none when it names no parts.
std::vector<inbox_messaging::approval_part_progress>
parts_of(const http_request& req, nats_client& session, const std::string& request_id) {
    try {
        inbox_messaging::get_approval_parts_request msg;
        msg.request_id = request_id;
        const auto reply = forwarding::forward(req, session, msg);
        if (reply.result.outcome != ores::utility::domain::outcome::ok)
            return {};
        return reply.parts;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(join_lg(), ores::logging::warn)
            << "The parts of request " << request_id << " were not read: " << e.what();
        return {};
    }
}

/// The trades a request is about, by the link the trading component keeps.
std::vector<subject>
subjects_of(const http_request& req, nats_client& session, const std::string& request_id) {
    if (!rr::is_uuid(request_id))
        return {};
    std::vector<subject> subjects;
    try {
        trading_messaging::list_trade_authorisation_requests_request msg;
        msg.limit = join_page;
        msg.filter.emplace();
        msg.filter->request_id_one_of = std::vector{boost::uuids::string_generator()(request_id)};
        const auto reply = forwarding::forward(req, session, msg);
        rr::require_ok(reply.result);
        for (const auto& link : reply.trade_authorisation_requests) {
            if (subjects.size() == trades_of_request)
                break;
            const auto trade_id = rr::as_text(link.trade_id);
            subjects.push_back({trade_id, link.trade_version, label_of(req, session, trade_id)});
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(join_lg(), ores::logging::warn)
            << "The trades of request " << request_id << " were not read: " << e.what();
        return subjects;
    }
    return subjects;
}

/// Where one part of a request stands, as the screen draws it.
std::string part_json(const inbox_messaging::approval_part_progress& part,
                      const std::map<std::string, std::string>& usernames) {
    const auto named = usernames.find(part.decided_by);
    return "{\"code\":" + json::write(part.code) + ",\"name\":" + json::write(part.name) +
           ",\"answerOrder\":" + json::write(part.answer_order) +
           ",\"stateCode\":" + json::write(part.state) + ",\"decidedBy\":" +
           json::write(named == usernames.end() ? part.decided_by : named->second) +
           ",\"decidedAt\":" + json::write(part.decided_at) +
           ",\"comment\":" + json::write(part.comment) + "}";
}

/// One thing a request is about, as the screen links it.
std::string subject_json(const subject& link) {
    return "{\"tradeId\":" + json::write(link.trade_id) +
           ",\"tradeVersion\":" + json::write(link.trade_version) +
           ",\"label\":" + json::write(link.label) + "}";
}

/// One request as the screen reads it.
std::string request_json(const inbox_domain::approval_request& request,
                         const std::vector<iam_messaging::requested_role>& roles,
                         const std::map<std::string, std::string>& requesters,
                         const std::map<std::string, std::string>& usernames,
                         const std::map<std::string, inbox_domain::approval_decision>& decisions,
                         const std::string& parts,
                         const std::string& subjects) {
    const auto id = rr::as_text(request.id);
    const auto raised_by = rr::as_text(request.requested_by);
    const auto account = requesters.find(id);
    const auto requester = account == requesters.end() ? raised_by : account->second;
    const auto named = usernames.find(requester);

    std::vector<std::string> written;
    written.reserve(roles.size());
    for (const auto& held : roles)
        written.push_back(role_json(held.role));

    const auto decided = decisions.find(id);
    const auto decision = decided == decisions.end() ? std::string("null") :
                                                       decision_json(decided->second, usernames);

    return "{\"id\":" + json::write(id) + ",\"version\":" + json::write(request.version) +
           ",\"kindCode\":" + json::write(request.kind_code) +
           ",\"stateCode\":" + json::write(request.state_code) +
           ",\"autoApprovalCode\":" + json::write(request.auto_approval_code) +
           ",\"requestedBy\":" + json::write(named == usernames.end() ? requester : named->second) +
           ",\"requestedAt\":" + json::write(request.requested_at) +
           ",\"reason\":" + json::write(request.reason) +
           ",\"expiresAt\":" + json::write(request.expires_at) +
           ",\"roles\":" + rr::array_of(written) + ",\"decision\":" + decision +
           ",\"parts\":" + parts + ",\"subjects\":" + subjects + "}";
}
}
