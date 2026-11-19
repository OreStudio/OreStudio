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
#ifndef ORES_HTTP_ROUTES_WORKFLOW_REQUEST_JOINS_HPP
#define ORES_HTTP_ROUTES_WORKFLOW_REQUEST_JOINS_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.iam.api/domain/role.hpp"
#include "ores.iam.api/messaging/role_request_operations_protocol.hpp"
#include "ores.inbox.api/domain/approval_decision.hpp"
#include "ores.inbox.api/messaging/approval_operations_protocol.hpp"
#include "ores.inbox.api/messaging/approval_request_protocol.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace ores::http::routes::workflow::joins {

/**
 * @brief The names and rows a request screen reads beside the page it asked for.
 *
 * A request row names the account that raised it, the people who decided it, the
 * roles it asks for, where each part of it stands and the trades it is about.
 * The store answers none of that with the page: each is a read of its own, and
 * the reads are here so that the four routes that draw a request draw it the same
 * way.
 *
 * Every join is tolerant: a read that the service refuses, or that fails,
 * leaves its name out of the row and logs it, because a page never fails for a
 * decoration. The sentence each route states when the page itself is refused is
 * the route's, not the join's.
 */

/// The most rows one join asks for; the services refuse a longer filter.
inline constexpr std::uint32_t join_page = 1000;

/// One thing a request is about: the trade, the version read and the name it is known by.
struct subject final {
    std::string trade_id;
    int trade_version = 0;
    std::string label;
};

/// The roles one request asks for, or none when the caller may not read them.
std::vector<ores::iam::messaging::requested_role>
roles_of(const ores::http::domain::http_request& req,
         ores::nats::service::nats_client& session,
         const std::string& request_id);

/// The account that raised each request, by the request's id.
std::map<std::string, std::string>
requesters_of(const ores::http::domain::http_request& req,
              ores::nats::service::nats_client& session,
              const std::vector<boost::uuids::uuid>& request_ids);

/// The username of each account, by the account's id.
std::map<std::string, std::string> usernames_of(const ores::http::domain::http_request& req,
                                                ores::nats::service::nats_client& session,
                                                const std::set<std::string>& account_ids);

/// The decision taken on each request, by the request's id.
std::map<std::string, ores::inbox::domain::approval_decision>
decisions_of(const ores::http::domain::http_request& req,
             ores::nats::service::nats_client& session,
             const std::vector<boost::uuids::uuid>& request_ids);

/// Where each part of one request stands.
std::vector<ores::inbox::messaging::approval_part_progress>
parts_of(const ores::http::domain::http_request& req,
         ores::nats::service::nats_client& session,
         const std::string& request_id);

/// The trades one request is about, each with the name it is known by.
std::vector<subject> subjects_of(const ores::http::domain::http_request& req,
                                 ores::nats::service::nats_client& session,
                                 const std::string& request_id);

/**
 * The trades each request of a page is about, by the request's id.
 *
 * A page costs two reads and not one per row: the links of every request in one
 * call, and the labels of every trade they name in another. This is the read a
 * list of requests makes; the single-request read above is for the one a person
 * opened.
 */
std::map<std::string, std::vector<subject>>
subjects_of_page(const ores::http::domain::http_request& req,
                 ores::nats::service::nats_client& session,
                 const std::vector<boost::uuids::uuid>& request_ids);

/// One role a request asks for, as the screen draws it.
std::string role_json(const ores::iam::domain::role& role);

/// One decision, with the decider named where the join could read the account.
std::string decision_json(const ores::inbox::domain::approval_decision& decision,
                          const std::map<std::string, std::string>& usernames);

/// One part of a request, with the person who answered it named where it can be.
std::string part_json(const ores::inbox::messaging::approval_part_progress& part,
                      const std::map<std::string, std::string>& usernames);

/// One thing a request is about, as the screen links it.
std::string subject_json(const subject& link);

/// One request as the screen reads it, with each join already written.
std::string
request_json(const ores::inbox::domain::approval_request& request,
             const std::vector<ores::iam::messaging::requested_role>& roles,
             const std::map<std::string, std::string>& requesters,
             const std::map<std::string, std::string>& usernames,
             const std::map<std::string, ores::inbox::domain::approval_decision>& decisions,
             const std::string& parts,
             const std::string& subjects);

}

#endif
