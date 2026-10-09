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
#ifndef ORES_IAM_CORE_MESSAGING_ROLE_REQUEST_HANDLER_HPP
#define ORES_IAM_CORE_MESSAGING_ROLE_REQUEST_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam.api/domain/permission_codes.hpp"
#include "ores.iam.api/domain/role_grant_request.hpp"
#include "ores.iam.api/domain/role_grant_request_role.hpp"
#include "ores.iam.api/messaging/role_request_operations_protocol.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.iam.core/repository/role_grant_request_repository.hpp"
#include "ores.iam.core/repository/role_grant_request_role_repository.hpp"
#include "ores.iam.core/repository/role_repository.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.inbox.api/messaging/approval_operations_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <optional>
#include <span>
#include <string>
#include <unordered_map>
#include <vector>

namespace ores::iam::messaging {

namespace {

inline auto& role_request_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.iam.messaging.role_request_handler");
    return instance;
}

inline ores::utility::domain::result
role_request_result(ores::utility::domain::outcome o, std::string code, std::string message) {
    return ores::utility::domain::result{
        .outcome = o, .code = std::move(code), .message = std::move(message), .fields = {}};
}

} // namespace

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief Answers a person asking for roles.
 *
 * The request is raised in the inbox as the person, by passing on the token
 * the person called with, so the inbox records who asked and applies its own
 * rules. IAM then records which roles the request asks for. If that record
 * cannot be written, IAM withdraws the request it raised, so a request never
 * waits in the queue naming nothing.
 */
class role_request_handler {
public:
    role_request_handler(ores::nats::service::client& nats,
                         ores::database::context ctx,
                         std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void ask(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        BOOST_LOG_SEV(role_request_handler_lg(), debug) << "Handling " << msg.subject;
        auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx) {
            error_reply(nats_, msg, ctx.error());
            return;
        }
        auto req = decode<ask_for_roles_request>(msg);
        if (!req) {
            reply(nats_,
                  msg,
                  ask_for_roles_response{
                      .result = role_request_result(
                          outcome::invalid, "bad_request", "The request could not be read.")});
            return;
        }

        try {
            const auto refusal = check(*ctx, *req);
            if (refusal) {
                reply(nats_, msg, ask_for_roles_response{.result = *refusal});
                return;
            }

            const auto raised = raise(msg, req->reason);
            if (raised.result.outcome != outcome::ok) {
                reply(nats_, msg, ask_for_roles_response{.result = raised.result});
                return;
            }

            try {
                record(*ctx, raised.request, *req);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(role_request_handler_lg(), error)
                    << "Recording role request " << boost::uuids::to_string(raised.request.id)
                    << " failed, withdrawing it: " << e.what();
                withdraw(msg, raised.request);
                reply(nats_,
                      msg,
                      ask_for_roles_response{
                          .result = role_request_result(outcome::failed,
                                                        "record_failed",
                                                        "The request could not be recorded.")});
                return;
            }

            reply(nats_,
                  msg,
                  ask_for_roles_response{.result = role_request_result(outcome::ok, "", ""),
                                         .request_id = boost::uuids::to_string(raised.request.id)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(role_request_handler_lg(), error)
                << "Error asking for roles: " << e.what();
            reply(nats_,
                  msg,
                  ask_for_roles_response{
                      .result = role_request_result(
                          outcome::failed, "ask_failed", "The request could not be made.")});
        }
    }

    /**
     * @brief Reads the roles one approval request asks for.
     *
     * The person who raised the request may read what it asks for; an
     * administrator, who may read the roles of any request, may read it too. A
     * caller entitled to neither is told the request does not exist, so the
     * answer is not a way to learn who asked for what.
     */
    void get_roles(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        BOOST_LOG_SEV(role_request_handler_lg(), debug) << "Handling " << msg.subject;
        auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx) {
            error_reply(nats_, msg, ctx.error());
            return;
        }
        auto req = decode<get_request_roles_request>(msg);
        if (!req) {
            reply(nats_,
                  msg,
                  get_request_roles_response{
                      .result = role_request_result(
                          outcome::invalid, "bad_request", "The request could not be read.")});
            return;
        }

        try {
            reply(nats_, msg, read_request_roles(*ctx, req->request_id));
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(role_request_handler_lg(), error)
                << "Reading the roles of " << req->request_id << " failed: " << e.what();
            reply(nats_,
                  msg,
                  get_request_roles_response{
                      .result = role_request_result(
                          outcome::failed, "read_failed", "The roles could not be read.")});
        }
    }

private:
    /**
     * @brief Reads the roles, or says the request does not exist.
     *
     * Refused and absent give one answer, so a caller who may not read a
     * request cannot tell a request they may not read from one that never
     * existed.
     */
    get_request_roles_response read_request_roles(const ores::database::context& ctx,
                                                  const std::string& request_id) {
        using ores::utility::domain::outcome;
        boost::uuids::string_generator parse;
        boost::uuids::uuid id;
        try {
            id = parse(request_id);
        } catch (const std::exception&) {
            return get_request_roles_response{
                .result = role_request_result(
                    outcome::invalid, "bad_request", "The request could not be read.")};
        }

        repository::role_grant_request_repository requests;
        const auto detail = requests.read_latest(ctx, boost::uuids::to_string(id));
        const auto absent = get_request_roles_response{
            .result = role_request_result(outcome::missing, "not_found", "No such request.")};
        if (detail.empty())
            return absent;

        service::authorization_service auth(ctx);
        const auto me = auth.caller_account();
        const auto asked_by = detail.front().account_id;
        // The permission is read only when the caller did not ask, so the
        // person who did pays no lookup for their own request.
        const bool entitled =
            (me && *me == asked_by) ||
            (me && auth.has_permission(*me, domain::permissions::role_grant_request_roles_read));
        if (!entitled) {
            BOOST_LOG_SEV(role_request_handler_lg(), warn)
                << "Reading the roles of request " << request_id << " denied to " << ctx.actor();
            return absent;
        }

        get_request_roles_response answer;
        repository::role_grant_request_role_repository rows(ctx);
        const auto asked = rows.read_latest_by_request(id);

        std::vector<std::string> role_ids;
        role_ids.reserve(asked.size());
        for (const auto& row : asked)
            role_ids.push_back(boost::uuids::to_string(row.role_id));

        if (!role_ids.empty()) {
            repository::role_repository roles;
            const auto catalogue = roles.read_latest(ctx, role_ids);
            for (const auto& row : asked) {
                const auto id_as_text = boost::uuids::to_string(row.role_id);
                const auto found =
                    std::ranges::find(catalogue, id_as_text, [](const domain::role& r) {
                        return boost::uuids::to_string(r.id);
                    });
                if (found == catalogue.end())
                    continue;
                answer.roles.push_back(requested_role{.role = *found,
                                                      .asked_at = row.recorded_at,
                                                      .applied_at = row.applied_at,
                                                      .applied_by = row.modified_by});
            }
            std::ranges::sort(
                answer.roles, {}, [](const requested_role& r) { return r.role.name; });
        }
        return answer;
    }

    /**
     * @brief Why a request may not be made, or nothing when it may.
     *
     * Every role must exist in the tenant, must be one the tenant offers to
     * its members, must not be held already, and must not be waiting in
     * another request: a second request for the same role would only queue
     * twice.
     */
    std::optional<ores::utility::domain::result> check(const ores::database::context& ctx,
                                                       const ask_for_roles_request& req) {
        using ores::utility::domain::outcome;
        if (req.role_ids.empty())
            return role_request_result(
                outcome::invalid, "roles_required", "Choose a role to ask for.");
        if (req.reason.empty())
            return role_request_result(outcome::invalid, "reason_required", "Say why you ask.");

        boost::uuids::string_generator parse;
        repository::role_repository roles;
        for (const auto& id : req.role_ids) {
            try {
                parse(id);
            } catch (const std::exception&) {
                return role_request_result(outcome::invalid, "unknown_role", "No such role: " + id);
            }
            const auto found = roles.read_latest(ctx, id);
            if (found.empty())
                return role_request_result(outcome::invalid, "unknown_role", "No such role: " + id);
            if (!found.front().is_requestable)
                return role_request_result(outcome::denied,
                                           "not_requestable",
                                           "This role is not one a member may ask for.");
        }

        const auto me = account_id(ctx);
        if (!me)
            return role_request_result(
                outcome::denied, "no_account", "The signed-in account was not found.");

        // Every id parsed as a UUID above, so the literal is built from the
        // parsed values and holds nothing but UUIDs.
        std::vector<std::string> parsed;
        for (const auto& id : req.role_ids)
            parsed.push_back(boost::uuids::to_string(parse(id)));
        const auto ids = array_literal(parsed);
        const auto held = ores::database::repository::execute_parameterized_string_query(
            ctx,
            "select role_id::text from ores_iam_account_roles_tbl"
            " where account_id = $1::uuid and role_id = any($2::uuid[])"
            " and valid_to = ores_utility_infinity_timestamp_fn()",
            {boost::uuids::to_string(*me), ids},
            role_request_handler_lg(),
            "Reading the roles a person already holds");
        if (!held.empty())
            return role_request_result(
                outcome::conflict, "already_held", "You already hold a role you asked for.");

        const auto waiting = ores::database::repository::execute_parameterized_string_query(
            ctx,
            "select role_id::text from ores_iam_open_role_grant_requests_fn($1::uuid, $2::uuid[])",
            {boost::uuids::to_string(*me), ids},
            role_request_handler_lg(),
            "Reading the roles a person already waits for");
        if (!waiting.empty())
            return role_request_result(
                outcome::conflict,
                "already_asked",
                "You already asked for a role in a request that still waits.");
        return std::nullopt;
    }

    struct raised_request {
        ores::utility::domain::result result;
        ores::inbox::domain::approval_request request;
    };

    /**
     * @brief Raises the inbox request as the person, with the token they
     * called with.
     */
    raised_request raise(const ores::nats::message& msg, const std::string& reason) {
        const auto r = forward(msg,
                               ores::inbox::messaging::raise_approval_request_request{
                                   .kind_code = "iam.role_grant", .reason = reason});
        return raised_request{.result = r.result, .request = r.request};
    }

    void withdraw(const ores::nats::message& msg, const ores::inbox::domain::approval_request& r) {
        try {
            const auto w = forward(msg,
                                   ores::inbox::messaging::withdraw_approval_request_request{
                                       .request_id = boost::uuids::to_string(r.id),
                                       .version = r.version,
                                       .comment = "The request could not be recorded."});
            if (w.result.outcome != ores::utility::domain::outcome::ok)
                BOOST_LOG_SEV(role_request_handler_lg(), error)
                    << "Withdrawing request " << boost::uuids::to_string(r.id)
                    << " was refused: " << w.result.message;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(role_request_handler_lg(), error)
                << "Withdrawing request " << boost::uuids::to_string(r.id)
                << " failed: " << e.what();
        }
    }

    void record(const ores::database::context& ctx,
                const ores::inbox::domain::approval_request& raised,
                const ask_for_roles_request& req) {
        domain::role_grant_request detail;
        detail.tenant_id = ctx.tenant_id();
        detail.request_id = raised.id;
        detail.account_id = raised.requested_by;
        detail.modified_by = ctx.actor();
        detail.change_reason_code = "system.new_record";
        repository::role_grant_request_repository requests;
        requests.write(ctx, detail, ores::utility::domain::precondition{});

        boost::uuids::string_generator parse;
        std::vector<domain::role_grant_request_role> rows;
        for (const auto& id : req.role_ids) {
            domain::role_grant_request_role row;
            row.tenant_id = ctx.tenant_id().to_string();
            row.request_id = raised.id;
            row.role_id = parse(id);
            row.modified_by = ctx.actor();
            row.change_reason_code = "system.new_record";
            rows.push_back(row);
        }
        repository::role_grant_request_role_repository roles(ctx);
        roles.write(rows);
    }

    /**
     * @brief Sends a request to another service as the person who called,
     * passing on their token.
     */
    template <typename Request>
    typename Request::response_type forward(const ores::nats::message& msg, const Request& req) {
        std::unordered_map<std::string, std::string> headers;
        const auto auth = msg.headers.find(std::string(ores::nats::headers::authorization));
        if (auth != msg.headers.end())
            headers.emplace(auth->first, auth->second);

        const auto& codec = ores::nats::default_wire_codec();
        const auto bytes = codec.encode(req);
        const auto answer =
            nats_.request_sync(Request::nats_subject, std::span<const std::byte>(bytes), headers);
        if (const auto it = answer.headers.find(std::string(ores::nats::headers::x_error));
            it != answer.headers.end())
            throw std::runtime_error("The inbox refused " + std::string(Request::nats_subject) +
                                     ": " + it->second);
        auto decoded = codec.decode<typename Request::response_type>(answer.data);
        if (!decoded)
            throw std::runtime_error("The inbox's answer to " + std::string(Request::nats_subject) +
                                     " could not be read.");
        return *decoded;
    }

    std::optional<boost::uuids::uuid> account_id(const ores::database::context& ctx) {
        repository::account_repository accounts;
        const auto found = accounts.read_latest_by_username(ctx, ctx.actor());
        if (found.empty())
            return std::nullopt;
        return found.front().id;
    }

    static std::string array_literal(const std::vector<std::string>& values) {
        std::string out = "{";
        for (std::size_t i = 0; i < values.size(); ++i) {
            if (i > 0)
                out += ',';
            out += values[i];
        }
        return out + "}";
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
