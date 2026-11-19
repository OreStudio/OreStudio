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
#include "ores.http/routes/workflow/request_proposal_routes.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/view_answer.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/workflow/request_joins.hpp"
#include "ores.inbox.api/messaging/approval_operations_protocol.hpp"
#include "ores.refdata.api/messaging/book_proposal_operations_protocol.hpp"
#include "ores.reporting.api/messaging/engine_settings_proposal_operations_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <cstddef>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::http::routes::workflow {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace inbox_domain = ores::inbox::domain;
namespace inbox_messaging = ores::inbox::messaging;
namespace refdata_messaging = ores::refdata::messaging;
namespace reporting_messaging = ores::reporting::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

/// The sentence a route states when its address names no request.
constexpr std::string_view proposal_id_refused = "A request is named by its identifier.";

/// The kinds that keep lines, and the one that proposes the roles it names.
constexpr std::string_view role_grant_kind = "iam.role_grant";
constexpr std::string_view engine_settings_kind = "reporting.engine_settings_change";
constexpr std::string_view book_change_kind = "refdata.book_change";

/// How long what a request proposes stays fresh: a decision is drawn from it.
constexpr std::chrono::seconds changes_stale_after{15};

/// How long the checks stay fresh: a decider reads them again as the record moves.
constexpr std::chrono::seconds checks_stale_after{30};

/// The identifier a request is named by.
std::string proposal_request_id_of(const http_request& req) {
    const auto raw = req.get_path_param("id");
    if (!rr::is_uuid(raw))
        rr::invalid(std::string(proposal_id_refused));
    return raw;
}

/**
 * The request the address names, or a 404.
 *
 * Opening the request is what says whether this caller may see it: the inbox
 * answers a request they may not open as one that does not exist, and so does
 * this, because telling a stranger that a request exists is the fact the inbox
 * refuses to give.
 */
inbox_domain::approval_request
found_request(const http_request& req, nats_client& session, const std::string& request_id) {
    inbox_messaging::get_approval_request msg;
    msg.request_id = request_id;
    const auto reply = forwarding::forward(req, session, msg);
    if (reply.result.outcome == ores::utility::domain::outcome::missing || !reply.request)
        rr::refuse(http_status::not_found, "not-found", "No such request.");
    rr::require_ok(reply.result);
    return *reply.request;
}

/// One field of a proposed line: the column, and the value it would replace.
std::string
field_json(std::string_view name, std::string_view before, std::string_view after, bool changed) {
    return "{\"name\":" + json::write(std::string(name)) +
           ",\"before\":" + json::write(std::string(before)) +
           ",\"after\":" + json::write(std::string(after)) +
           ",\"changed\":" + json::write(changed) + "}";
}

/// One proposed line, whatever kind it came from.
std::string line_json(int line_no,
                      std::string_view operation,
                      std::string_view subject_id,
                      int base_version,
                      bool applied,
                      const std::vector<std::string>& fields) {
    return "{\"lineNo\":" + std::to_string(line_no) +
           ",\"operation\":" + json::write(std::string(operation)) +
           ",\"subjectId\":" + json::write(std::string(subject_id)) +
           ",\"baseVersion\":" + std::to_string(base_version) +
           ",\"applied\":" + json::write(applied) + ",\"fields\":" + rr::array_of(fields) + "}";
}

/// The lines a role grant proposes: the roles it names, one line each.
std::vector<std::string>
role_grant_changes(const http_request& req, nats_client& session, const std::string& request_id) {
    std::vector<std::string> written;
    const auto roles = joins::roles_of(req, session, request_id);
    written.reserve(roles.size());
    for (std::size_t i = 0; i < roles.size(); ++i) {
        const auto& role = roles[i].role;
        const std::vector<std::string> fields{
            field_json("role", "", role.name, true),
            field_json("description", "", role.description, true)};
        written.push_back(line_json(
            static_cast<int>(i) + 1, "grant", boost::uuids::to_string(role.id), 0, false, fields));
    }
    return written;
}

/// The lines an engine settings proposal holds, each beside the value in force.
std::vector<std::string> engine_settings_changes(const http_request& req,
                                                 nats_client& session,
                                                 const std::string& request_id) {
    reporting_messaging::get_engine_settings_changes_request msg;
    msg.request_id = request_id;
    const auto reply = forwarding::forward(req, session, msg);
    if (reply.result.outcome == ores::utility::domain::outcome::missing ||
        reply.result.outcome == ores::utility::domain::outcome::denied)
        return {};
    rr::require_ok(reply.result);

    std::vector<std::string> written;
    written.reserve(reply.changes.size());
    for (const auto& line : reply.changes) {
        std::vector<std::string> fields;
        fields.reserve(line.fields.size());
        for (const auto& field : line.fields)
            fields.push_back(
                field_json(field.column_name, field.before, field.after, field.changed));
        written.push_back(line_json(line.line_no,
                                    line.group_code,
                                    boost::uuids::to_string(line.engine_settings_id),
                                    line.base_version,
                                    line.applied,
                                    fields));
    }
    return written;
}

/// The lines a book change holds, each beside the live book.
std::vector<std::string>
book_changes(const http_request& req, nats_client& session, const std::string& request_id) {
    refdata_messaging::get_book_changes_request msg;
    msg.request_id = request_id;
    const auto reply = forwarding::forward(req, session, msg);
    if (reply.result.outcome == ores::utility::domain::outcome::missing ||
        reply.result.outcome == ores::utility::domain::outcome::denied)
        return {};
    rr::require_ok(reply.result);

    std::vector<std::string> written;
    written.reserve(reply.changes.size());
    for (const auto& line : reply.changes) {
        std::vector<std::string> fields;
        fields.reserve(line.fields.size());
        for (const auto& field : line.fields)
            fields.push_back(field_json(field.name, field.before, field.after, field.changed));
        written.push_back(line_json(line.line_no,
                                    line.operation,
                                    boost::uuids::to_string(line.entity_id),
                                    line.base_version,
                                    line.applied,
                                    fields));
    }
    return written;
}

/// What the request proposes, as the kind it is states it.
std::vector<std::string> changes_of(const http_request& req,
                                    nats_client& session,
                                    const inbox_domain::approval_request& request) {
    const auto id = rr::as_text(request.id);
    if (request.kind_code == role_grant_kind)
        return role_grant_changes(req, session, id);
    if (request.kind_code == engine_settings_kind)
        return engine_settings_changes(req, session, id);
    if (request.kind_code == book_change_kind)
        return book_changes(req, session, id);
    return {};
}

/// One line's verdict, as the checks screen reads it.
std::string check_json(int line_no,
                       std::string_view operation,
                       std::string_view subject_id,
                       const std::string& refusal) {
    return "{\"lineNo\":" + std::to_string(line_no) +
           ",\"operation\":" + json::write(std::string(operation)) +
           ",\"subjectId\":" + json::write(std::string(subject_id)) +
           ",\"refusal\":" + json::write(refusal) + "}";
}

/// The refusal of each proposed line, and the parts the policy names.
struct checks final {
    std::vector<std::string> lines;
    std::vector<std::string> part_codes;
};

/// What running an engine settings proposal says, read now.
checks engine_settings_checks(const http_request& req,
                              nats_client& session,
                              const std::string& request_id) {
    reporting_messaging::recheck_engine_settings_changes_request msg;
    msg.request_id = request_id;
    const auto reply = forwarding::forward(req, session, msg);
    if (reply.result.outcome == ores::utility::domain::outcome::missing ||
        reply.result.outcome == ores::utility::domain::outcome::denied)
        return {};
    rr::require_ok(reply.result);

    checks out;
    out.part_codes = reply.part_codes;
    out.lines.reserve(reply.changes.size());
    for (std::size_t i = 0; i < reply.changes.size(); ++i) {
        const auto& line = reply.changes[i];
        out.lines.push_back(
            check_json(line.line_no,
                       line.group_code,
                       boost::uuids::to_string(line.engine_settings_id),
                       i < reply.refusals.size() ? reply.refusals[i] : std::string()));
    }
    return out;
}

/// What running a book change says, read now.
checks book_checks(const http_request& req, nats_client& session, const std::string& request_id) {
    refdata_messaging::recheck_book_changes_request msg;
    msg.request_id = request_id;
    const auto reply = forwarding::forward(req, session, msg);
    if (reply.result.outcome == ores::utility::domain::outcome::missing ||
        reply.result.outcome == ores::utility::domain::outcome::denied)
        return {};
    rr::require_ok(reply.result);

    checks out;
    out.part_codes = reply.part_codes;
    out.lines.reserve(reply.lines.size());
    for (const auto& line : reply.lines)
        out.lines.push_back(check_json(
            line.line_no, line.operation, boost::uuids::to_string(line.entity_id), line.refusal));
    return out;
}

/// What running the request's lines now says, as the kind it is states it.
checks checks_of(const http_request& req,
                 nats_client& session,
                 const inbox_domain::approval_request& request) {
    const auto id = rr::as_text(request.id);
    if (request.kind_code == engine_settings_kind)
        return engine_settings_checks(req, session, id);
    if (request.kind_code == book_change_kind)
        return book_checks(req, session, id);
    return {};
}

}

http_response request_proposal_routes::changes_answer(const http_request& req, std::string body) {
    return ores::http::net::view_answer(
        req, std::move(body), ores::http::net::view_cache_policy::fresh_for(changes_stale_after));
}

http_response request_proposal_routes::checks_answer(const http_request& req, std::string body) {
    return ores::http::net::view_answer(
        req, std::move(body), ores::http::net::view_cache_policy::fresh_for(checks_stale_after));
}

void request_proposal_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"workflow"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/requests/{id}/changes")
            .summary("What the request proposes, for any kind")
            .description("A composed view owned by ores.workflow: the request is read first, "
                         "because its kind decides where its lines are kept, and then the "
                         "component that owns the kind answers them. A role grant proposes the "
                         "roles it names, one line each; an engine settings change and a book "
                         "change each state the lines they hold beside the value in force; and a "
                         "kind that keeps no lines proposes none. The answer carries an ETag and "
                         "stays fresh for 15 seconds.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<inbox_messaging::get_approval_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto request_id = proposal_request_id_of(req);
                    const auto request = found_request(req, session, request_id);
                    return changes_answer(
                        req,
                        "{\"requestId\":" + json::write(request_id) +
                            ",\"kindCode\":" + json::write(request.kind_code) + ",\"changes\":" +
                            rr::array_of(changes_of(req, session, request)) + "}");
                });
            }));

    add(router->get("/api/requests/{id}/checks")
            .summary("What running the request's lines through the real write now says")
            .description("A composed view owned by ores.workflow: the live record may have moved "
                         "since the request was raised, so a decider reads the checks as they "
                         "stand. The kind's own component runs its lines through the write "
                         "without applying them and answers the refusal of each, and the parts "
                         "the policy names for the request. The answer carries an ETag and stays "
                         "fresh for 30 seconds.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<inbox_messaging::get_approval_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto request_id = proposal_request_id_of(req);
                    const auto request = found_request(req, session, request_id);
                    const auto verdict = checks_of(req, session, request);
                    return checks_answer(req,
                                         "{\"requestId\":" + json::write(request_id) +
                                             ",\"lines\":" + rr::array_of(verdict.lines) +
                                             ",\"partCodes\":" + json::write(verdict.part_codes) +
                                             "}");
                });
            }));
}

}
