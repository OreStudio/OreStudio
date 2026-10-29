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
#ifndef ORES_REPORTING_SERVICE_MESSAGING_ENGINE_SETTINGS_PROPOSAL_HANDLER_HPP
#define ORES_REPORTING_SERVICE_MESSAGING_ENGINE_SETTINGS_PROPOSAL_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.reporting.api/messaging/engine_settings_proposal_operations_protocol.hpp"
#include "ores.reporting.service/service/engine_settings_proposal.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <algorithm>
#include <optional>
#include <string>
#include <utility>
#include <vector>

namespace ores::reporting::messaging {

/**
 * @brief Answers the operations that propose a change to engine run settings.
 *
 * Every operation acts as the signed-in person. The handler proves the request,
 * checks the permission the write needs, and leaves the lifecycle rules to the
 * proposal service, which is the one place they are stated.
 *
 * The lines and the checks of a proposal are answered to the person who raised
 * it, to whoever may decide one of its parts, and to whoever may read the
 * tenant's requests. A stranger is answered as if the request did not exist.
 */
class engine_settings_proposal_handler {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.reporting.messaging.engine_settings_proposal_handler");
        return instance;
    }

    static ores::utility::domain::result
    result_of(ores::utility::domain::outcome o, std::string code, std::string message) {
        return ores::utility::domain::result{
            .outcome = o, .code = std::move(code), .message = std::move(message), .fields = {}};
    }

public:
    engine_settings_proposal_handler(ores::nats::service::client& nats,
                                     ores::database::context ctx,
                                     std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves reporting.v1.ops.raise_engine_settings_change.
     */
    void raise(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        auto req = ores::service::messaging::decode<raise_engine_settings_change_request>(msg);
        if (!req) {
            ores::service::messaging::error_reply(
                nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        if (!may_write_engine_settings(*ctx, req->group_code)) {
            ores::service::messaging::error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        try {
            auto raised = service::engine_settings_proposal_service(*ctx).raise(*req);
            ores::service::messaging::reply(nats_, msg, raised);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), ores::logging::error)
                << "Error raising an engine run settings proposal: " << e.what();
            ores::service::messaging::reply(
                nats_,
                msg,
                raise_engine_settings_change_response{
                    .result = result_of(
                        outcome::failed, "raise_failed", "The change could not be proposed."),
                    .request_id = {}});
        }
    }

    /**
     * @brief Serves reporting.v1.ops.get_engine_settings_changes.
     */
    void get_changes(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        auto req = ores::service::messaging::decode<get_engine_settings_changes_request>(msg);
        if (!req) {
            ores::service::messaging::error_reply(
                nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        if (!may_open(*ctx, req->request_id)) {
            ores::service::messaging::reply(
                nats_,
                msg,
                get_engine_settings_changes_response{
                    .result = result_of(outcome::missing, {}, "No such request."), .changes = {}});
            return;
        }
        try {
            ores::service::messaging::reply(
                nats_,
                msg,
                get_engine_settings_changes_response{
                    .result = result_of(outcome::ok, {}, {}),
                    .changes =
                        service::engine_settings_proposal_service(*ctx).lines(req->request_id)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), ores::logging::error)
                << "Error reading an engine run settings proposal: " << e.what();
            ores::service::messaging::reply(
                nats_,
                msg,
                get_engine_settings_changes_response{
                    .result =
                        result_of(outcome::failed, "read_failed", "The lines could not be read."),
                    .changes = {}});
        }
    }

    /**
     * @brief Serves reporting.v1.ops.recheck_engine_settings_changes.
     */
    void recheck(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        auto req = ores::service::messaging::decode<recheck_engine_settings_changes_request>(msg);
        if (!req) {
            ores::service::messaging::error_reply(
                nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        if (!may_open(*ctx, req->request_id)) {
            ores::service::messaging::reply(
                nats_,
                msg,
                recheck_engine_settings_changes_response{
                    .result = result_of(outcome::missing, {}, "No such request."),
                    .changes = {},
                    .refusals = {},
                    .part_codes = {}});
            return;
        }
        try {
            auto service = service::engine_settings_proposal_service(*ctx);
            auto changes = service.lines(req->request_id);
            const auto refusals = service.refusals(req->request_id);
            std::vector<std::string> parts;
            for (const auto& change : changes) {
                for (const auto& code : service.part_codes(change.group_code)) {
                    if (std::ranges::find(parts, code) == parts.end())
                        parts.push_back(code);
                }
            }
            ores::service::messaging::reply(
                nats_,
                msg,
                recheck_engine_settings_changes_response{.result = result_of(outcome::ok, {}, {}),
                                                         .changes = std::move(changes),
                                                         .refusals = refusals,
                                                         .part_codes = std::move(parts)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), ores::logging::error)
                << "Error checking an engine run settings proposal: " << e.what();
            ores::service::messaging::reply(
                nats_,
                msg,
                recheck_engine_settings_changes_response{
                    .result =
                        result_of(outcome::failed, "check_failed", "The lines could not be read."),
                    .changes = {},
                    .refusals = {},
                    .part_codes = {}});
        }
    }

private:
    /**
     * @brief Whether the caller may propose a change to this group.
     *
     * The permission a write to the group needs, because a proposal is the way
     * a person writes a number-changing group.
     */
    bool may_write_engine_settings(const ores::database::context& ctx,
                                   const std::string& group_code) {
        return ores::service::messaging::has_permission(
            ctx, "reporting::engine_settings_" + group_code + ":write");
    }

    /**
     * @brief Whether the caller may open the proposal an identifier names.
     *
     * The person who raised it, whoever may decide one of the parts its kind
     * names, and whoever may read the tenant's requests. A request of another
     * kind, or one that does not exist, is not one this operation opens.
     */
    bool may_open(const ores::database::context& ctx, const std::string& request_id) {
        ores::inbox::service::approval_lifecycle lifecycle(ctx);
        const auto found = lifecycle.request(request_id);
        if (!found ||
            found->kind_code != std::string(service::engine_settings_proposal_service::kind_code))
            return false;
        const auto me = lifecycle.actor_account_id();
        if (me && found->requested_by == *me)
            return true;
        const auto kind = lifecycle.kind(found->kind_code);
        if (kind && ores::service::messaging::has_permission(ctx, kind->decide_permission_code))
            return true;
        for (const auto& code : lifecycle.part_codes_of_kind(found->kind_code)) {
            const auto part = lifecycle.part(code);
            if (part && ores::service::messaging::has_permission(ctx, part->decide_permission_code))
                return true;
        }
        return ores::service::messaging::has_permission(ctx, "inbox::approval_requests:read");
    }

    std::optional<ores::database::context> context_for(const ores::nats::message& msg) {
        BOOST_LOG_SEV(lg(), ores::logging::debug) << "Handling " << msg.subject;
        auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx) {
            ores::service::messaging::error_reply(nats_, msg, ctx.error());
            return std::nullopt;
        }
        return *ctx;
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
