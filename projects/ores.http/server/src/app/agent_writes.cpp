/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.http.server/app/agent_writes.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http.server/app/agent_write_rules.hpp"
#include "ores.iam.api/domain/agent_held_write.hpp"
#include "ores.iam.api/messaging/agent_grant_operations_protocol.hpp"
#include "ores.iam.core/repository/agent_held_write_store.hpp"
#include "ores.inbox.api/messaging/approval_operations_protocol.hpp"
#include "ores.inbox.api/messaging/approval_request_protocol.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.security/authorization/agent_withholding.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/json/parse.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <exception>
#include <span>
#include <string_view>

namespace ores::http::server::app {

namespace {

using namespace ores::logging;
using ores::http::domain::http_method;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using store = ores::iam::repository::agent_held_write_store;

auto& agent_writes_lg() {
    static auto instance = make_logger("ores.http.server.app.agent_writes");
    return instance;
}

std::string agent_write_method(http_method method) {
    switch (method) {
        case http_method::post:
            return "POST";
        case http_method::put:
            return "PUT";
        case http_method::patch:
            return "PATCH";
        case http_method::delete_:
            return "DELETE";
        case http_method::get:
            return "GET";
        case http_method::head:
            return "HEAD";
        case http_method::options:
            return "OPTIONS";
    }
    return "POST";
}

std::optional<http_method> agent_write_method_of(const std::string& text) {
    for (const auto m :
         {http_method::post, http_method::put, http_method::patch, http_method::delete_})
        if (agent_write_method(m) == text)
            return m;
    return std::nullopt;
}


void fail_agent_write(const ores::database::context& tctx,
                      const std::string& request_id,
                      int status,
                      const std::string& why) {
    store(tctx).finish(request_id, store::failed, status, why);
    try {
        (void)ores::database::query_builder::execute_parameterized_multi_column_query(
            tctx,
            "select outcome from ores_inbox_fail_apply_fn($1::uuid, $2, $3)",
            {request_id, why.substr(0, 500), tctx.actor()},
            agent_writes_lg(),
            "Marking an agent write that could not apply");
    } catch (const std::exception& e) {
        ORES_LOG_SEV(agent_writes_lg(), error)
            << "Agent write " << request_id << " failed and could not be marked: " << e.what();
    }
}

void replay_agent_write(const std::string& tenant_text,
                        const std::string& request_id,
                        ores::database::context ctx,
                        ores::nats::service::nats_client& service,
                        ores::http::net::http_server& server) {
    const auto tenant = ores::utility::uuid::tenant_id::from_string(tenant_text);
    if (!tenant)
        return;
    const auto tctx = ctx.with_tenant(*tenant, ctx.actor());

    const auto decided = ores::database::query_builder::execute_parameterized_multi_column_query(
        tctx,
        "select state_code from ores_inbox_approval_requests_tbl where id = $1::uuid "
        "and kind_code = 'iam.write_by_agent' and valid_to = ores_utility_infinity_timestamp_fn()",
        {request_id},
        agent_writes_lg(),
        "Reading an agent write's request");
    if (decided.empty() || decided.front().empty() || decided.front().front() != "approved")
        return;
    if (!store(tctx).claim(request_id))
        return;
    const auto held = store(tctx).find(request_id);
    if (!held)
        return;

    // The replay token is IAM's, for this request alone, and names the
    // person and the agent; the gateway never signs one.
    ores::iam::messaging::issue_agent_replay_token_request ask;
    ask.tenant_id = tenant->to_string();
    ask.request_id = request_id;
    std::string token;
    try {
        const auto wire = ores::nats::default_wire_codec().encode(ask);
        // One retry: a request lost in transit is not a refusal.
        const auto ask_iam = [&] {
            return service.authenticated_request(
                ores::iam::messaging::issue_agent_replay_token_request::nats_subject,
                std::span<const std::byte>(wire));
        };
        auto reply = [&] {
            try {
                return ask_iam();
            } catch (const std::exception&) {
                return ask_iam();
            }
        }();
        const auto answer =
            ores::nats::default_wire_codec()
                .decode<ores::iam::messaging::issue_agent_replay_token_response>(reply.data);
        if (!answer || !answer->success || answer->token.empty()) {
            fail_agent_write(
                tctx, request_id, 0, answer ? answer->message : "IAM gave no replay token.");
            return;
        }
        token = answer->token;
    } catch (const std::exception& e) {
        fail_agent_write(tctx, request_id, 0, std::string("No replay token: ") + e.what());
        return;
    }

    const auto& verifier = server.verifier();
    auto claims = verifier ? verifier->validate(token) : decltype(verifier->validate(token)){};
    const auto method = agent_write_method_of(held->method);
    if (!claims || !method) {
        fail_agent_write(tctx, request_id, 0, "The replay token or the held call is not valid.");
        return;
    }

    ores::http::domain::http_request req;
    req.method = *method;
    req.target = held->target;
    req.body = held->body;
    req.headers["authorization"] = "Bearer " + token;
    req.headers["content-type"] =
        held->content_type.empty() ? std::string("application/json") : held->content_type;
    req.headers["x-ores-request"] = "1";
    req.authenticated_user = std::move(claims.value());

    try {
        const auto answer = server.run_in_process(std::move(req)).get();
        const auto status = static_cast<int>(answer.status);
        std::string why;
        if (agent_write_applied(answer, why)) {
            store(tctx).finish(request_id, store::applied, status, "Applied.");
            ORES_LOG_SEV(agent_writes_lg(), info)
                << "Replayed agent write " << request_id << " with status " << status;
        } else {
            fail_agent_write(tctx, request_id, status, why.empty() ? answer.body : why);
        }
    } catch (const std::exception& e) {
        fail_agent_write(tctx, request_id, 0, std::string("The replay failed: ") + e.what());
    }
}

}

ores::http::domain::exchanged_write_holder
agent_writes::holder(ores::database::context ctx, ores::nats::service::nats_client& service) {
    // The client is the application's, which outlives the server that calls this.
    return [ctx = std::move(ctx),
            &service](const ores::http::domain::http_request& req,
                      const std::string& route) -> std::optional<http_response> {
        if (!req.authenticated_user)
            return http_response::forbidden("An agent write needs its agent's token.");
        const auto& claims = *req.authenticated_user;
        const auto tenant =
            ores::utility::uuid::tenant_id::from_string(claims.tenant_id.value_or(""));
        if (!tenant)
            return http_response::forbidden("An agent write needs its agent's tenant.");
        if (agent_call_only_reads(route))
            return std::nullopt;
        // A token that can write nothing, such as a read-only role's, is
        // refused before any request is raised: its person would approve a
        // write the service then refuses.
        if (!agent_token_can_write(claims.roles))
            return http_response::forbidden("The agent's grant allows no write.");
        const auto tctx = ctx.with_tenant(*tenant, claims.username.value_or(""));
        const auto method = agent_write_method(req.method);

        try {
            const auto exempt =
                ores::database::query_builder::execute_parameterized_multi_column_query(
                    tctx,
                    "select 1 from ores_inbox_approval_policies_tbl "
                    "where entity_type = 'agent_write' and operation = $1 and field_name = $2 "
                    "and part_code = 'none' and valid_to = ores_utility_infinity_timestamp_fn() "
                    "limit 1",
                    {method, route},
                    agent_writes_lg(),
                    "Asking whether an agent write is exempt");
            if (!exempt.empty())
                return std::nullopt;

            // Raised with the agent's own token, so the request names the
            // person as its asker and carries the agent's stamp.
            ores::inbox::messaging::raise_approval_request_request raise;
            raise.kind_code = std::string(kind_code);
            raise.reason = "An LLM agent asks to " + method + " " + route + ".";
            const auto raised = ores::http::forwarding::forward(req, service, raise);
            if (raised.result.outcome != ores::utility::domain::outcome::ok)
                return http_response::error(http_status::conflict,
                                            raised.result.message.empty() ?
                                                "The agent write could not be held." :
                                                raised.result.message);

            ores::iam::domain::agent_held_write held;
            held.request_id = boost::uuids::to_string(raised.request.id);
            held.tenant_id = tenant->to_string();
            held.party_id = claims.party_id.value_or("");
            held.grant_id = claims.grant_id.value_or("");
            held.grantor_account_id = claims.subject;
            held.method = method;
            held.route = route;
            held.target = req.target;
            held.content_type = req.get_header("content-type");
            held.body = req.body;
            store(tctx).hold(held);

            ORES_LOG_SEV(agent_writes_lg(), info)
                << "Held an agent write as request " << held.request_id << ": " << method << " "
                << route;
            return http_response::json(
                "{\"held\":true,\"requestId\":" +
                    ores::utility::codec::json::write(held.request_id) + ",\"message\":" +
                    ores::utility::codec::json::write(
                        std::string("The write waits for the approval of the person the agent "
                                    "acts for.")) +
                    "}",
                http_status::accepted);
        } catch (const std::exception& e) {
            ORES_LOG_SEV(agent_writes_lg(), error)
                << "An agent write could not be held for " << route << ": " << e.what();
            return http_response::error(http_status::service_unavailable,
                                        "The agent write could not be held now; retry shortly.");
        }
    };
}

agent_write_replays::agent_write_replays(ores::database::context ctx,
                                         ores::nats::service::nats_client& service,
                                         ores::http::net::http_server& server)
    : ctx_(std::move(ctx))
    , service_(service)
    , server_(server)
    , worker_([this] { run(); }) {}

agent_write_replays::~agent_write_replays() {
    {
        std::lock_guard lock(mutex_);
        stopping_ = true;
    }
    ready_.notify_all();
    worker_.join();
}

void agent_write_replays::on_request_event(const ores::nats::message& msg) {
    const auto tenant = msg.headers.find(std::string(ores::nats::headers::x_tenant_id));
    if (tenant == msg.headers.end())
        return;
    const auto event =
        ores::nats::default_wire_codec().decode<ores::inbox::messaging::approval_request_event>(
            msg.data);
    if (!event)
        return;
    {
        std::lock_guard lock(mutex_);
        pending_.emplace_back(tenant->second, boost::uuids::to_string(event->key.id));
    }
    ready_.notify_one();
}

void agent_write_replays::run() {
    for (;;) {
        std::pair<std::string, std::string> next;
        {
            std::unique_lock lock(mutex_);
            ready_.wait(lock, [this] { return stopping_ || !pending_.empty(); });
            if (stopping_)
                return;
            next = std::move(pending_.front());
            pending_.pop_front();
        }
        try {
            replay_agent_write(next.first, next.second, ctx_, service_, server_);
        } catch (const std::exception& e) {
            ORES_LOG_SEV(agent_writes_lg(), error)
                << "Replaying agent write " << next.second << " failed: " << e.what();
        }
    }
}

}
