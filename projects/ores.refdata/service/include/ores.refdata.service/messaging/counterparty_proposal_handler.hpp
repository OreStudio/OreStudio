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
#ifndef ORES_REFDATA_SERVICE_MESSAGING_COUNTERPARTY_PROPOSAL_HANDLER_HPP
#define ORES_REFDATA_SERVICE_MESSAGING_COUNTERPARTY_PROPOSAL_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/messaging/counterparty_proposal_operations_protocol.hpp"
#include "ores.refdata.service/messaging/counterparty_proposal_mapping.hpp"
#include "ores.refdata.service/service/counterparty_proposal.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <optional>
#include <string>
#include <utility>

namespace ores::refdata::messaging {

/**
 * @brief Answers the operations that propose changes to counterparties.
 *
 * Every operation acts as the signed-in person. The handler proves the
 * request, checks the permission each line's write needs, and leaves the rest
 * to the proposal service.
 */
class counterparty_proposal_handler {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.refdata.messaging.counterparty_proposal_handler");
        return instance;
    }

    static ores::utility::domain::result
    result_of(ores::utility::domain::outcome o, std::string code, std::string message) {
        return ores::utility::domain::result{
            .outcome = o, .code = std::move(code), .message = std::move(message), .fields = {}};
    }

public:
    counterparty_proposal_handler(ores::nats::service::client& nats,
                                  ores::database::context ctx,
                                  std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves refdata.v1.ops.preview_counterparty_changes.
     */
    void preview(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        auto req = ores::service::messaging::decode<preview_counterparty_changes_request>(msg);
        if (!req) {
            ores::service::messaging::error_reply(
                nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        const auto lines = lines_of(*req);
        if (!admitted(*ctx, msg, lines))
            return;
        try {
            const auto previewed = service::counterparty_proposal_service(*ctx).preview(lines);
            ores::service::messaging::reply(
                nats_,
                msg,
                preview_counterparty_changes_response{.result = result_of(outcome::ok, "", ""),
                                                      .lines = to_outcomes(previewed),
                                                      .part_codes = previewed.part_codes});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), ores::logging::error)
                << "Error previewing counterparty changes: " << e.what();
            ores::service::messaging::reply(
                nats_,
                msg,
                preview_counterparty_changes_response{
                    .result = result_of(
                        outcome::failed, "preview_failed", "The changes could not be previewed."),
                    .lines = {},
                    .part_codes = {}});
        }
    }

    /**
     * @brief Serves refdata.v1.ops.raise_counterparty_changes.
     */
    void raise(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        auto req = ores::service::messaging::decode<raise_counterparty_changes_request>(msg);
        if (!req) {
            ores::service::messaging::error_reply(
                nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        const auto lines = lines_of(*req);
        if (!admitted(*ctx, msg, lines))
            return;
        try {
            const auto proposal =
                service::counterparty_proposal_service(*ctx).raise(lines, req->reason);
            raise_counterparty_changes_response response{.result = {},
                                                         .request_id = std::nullopt,
                                                         .lines = to_outcomes(proposal.preview),
                                                         .part_codes = proposal.preview.part_codes};
            if (proposal.request) {
                response.request_id = proposal.request->id;
                response.result = result_of(outcome::ok, "", "");
            } else {
                response.result =
                    result_of(outcome::invalid, refusal_code(proposal), proposal.message);
            }
            ores::service::messaging::reply(nats_, msg, response);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), ores::logging::error)
                << "Error raising counterparty changes: " << e.what();
            ores::service::messaging::reply(
                nats_,
                msg,
                raise_counterparty_changes_response{
                    .result = result_of(
                        outcome::failed, "raise_failed", "The changes could not be raised."),
                    .request_id = std::nullopt,
                    .lines = {},
                    .part_codes = {}});
        }
    }

    /**
     * @brief Serves refdata.v1.ops.get_counterparty_changes.
     *
     * Answers the lines of a request beside the live rows to whoever may open
     * the request, and as not found to anyone else.
     */
    void get_changes(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        auto req = ores::service::messaging::decode<get_counterparty_changes_request>(msg);
        if (!req) {
            ores::service::messaging::error_reply(
                nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        try {
            if (!may_open(*ctx, req->request_id)) {
                ores::service::messaging::reply(
                    nats_,
                    msg,
                    get_counterparty_changes_response{
                        .result = result_of(outcome::missing, "not_found", ""), .changes = {}});
                return;
            }
            ores::service::messaging::reply(
                nats_,
                msg,
                get_counterparty_changes_response{
                    .result = result_of(outcome::ok, "", ""),
                    .changes = to_deltas(
                        service::counterparty_proposal_service(*ctx).deltas(req->request_id))});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), ores::logging::error)
                << "Error reading the counterparty changes of " << req->request_id << ": "
                << e.what();
            ores::service::messaging::reply(
                nats_,
                msg,
                get_counterparty_changes_response{
                    .result =
                        result_of(outcome::failed, "read_failed", "The changes could not be read."),
                    .changes = {}});
        }
    }

    /**
     * @brief Serves refdata.v1.ops.recheck_counterparty_changes.
     */
    void recheck(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        auto req = ores::service::messaging::decode<recheck_counterparty_changes_request>(msg);
        if (!req) {
            ores::service::messaging::error_reply(
                nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        try {
            if (!may_open(*ctx, req->request_id)) {
                ores::service::messaging::reply(
                    nats_,
                    msg,
                    recheck_counterparty_changes_response{
                        .result = result_of(outcome::missing, "not_found", ""),
                        .lines = {},
                        .part_codes = {}});
                return;
            }
            const auto checked =
                service::counterparty_proposal_service(*ctx).recheck(req->request_id);
            ores::service::messaging::reply(
                nats_,
                msg,
                recheck_counterparty_changes_response{.result = result_of(outcome::ok, "", ""),
                                                      .lines = to_outcomes(checked),
                                                      .part_codes = checked.part_codes});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), ores::logging::error)
                << "Error rechecking the counterparty changes of " << req->request_id << ": "
                << e.what();
            ores::service::messaging::reply(
                nats_,
                msg,
                recheck_counterparty_changes_response{
                    .result = result_of(
                        outcome::failed, "recheck_failed", "The changes could not be checked."),
                    .lines = {},
                    .part_codes = {}});
        }
    }

private:
    /**
     * @brief The lines a message carries, by kind.
     */
    template <typename Request>
    static service::counterparty_lines lines_of(const Request& req) {
        return {.counterparties = req.counterparties,
                .identifiers = req.identifiers,
                .links = req.links};
    }

    /**
     * @brief Why a proposal raised nothing, as a code a screen can switch on.
     */
    static std::string refusal_code(const service::counterparty_proposal& proposal) {
        if (proposal.preview.lines.empty())
            return "bad_request";
        if (proposal.preview.refused())
            return "line_refused";
        if (proposal.preview.part_codes.empty())
            return "needs_no_request";
        return "bad_request";
    }

    /**
     * @brief Whether the lines can be proposed by this caller, replying with
     * the refusal when they cannot.
     *
     * The preview shows the effect of a write, so it needs the permission the
     * write needs. Lines that cannot be proposed at all are refused before the
     * permission is read.
     */
    bool admitted(const ores::database::context& ctx,
                  const ores::nats::message& msg,
                  const service::counterparty_lines& lines) {
        if (!invalid_lines(lines).empty()) {
            ores::service::messaging::error_reply(
                nats_, msg, ores::service::error_code::bad_request);
            return false;
        }
        for (const auto& permission : required_permissions(lines)) {
            if (!ores::service::messaging::has_permission(ctx, permission)) {
                ores::service::messaging::error_reply(
                    nats_, msg, ores::service::error_code::forbidden);
                return false;
            }
        }
        return true;
    }

    /**
     * @brief Whether the caller may open the counterparty change request an identifier
     * names.
     *
     * The person who raised it, whoever may decide its kind, and whoever may
     * read the tenant's requests. A request of another kind, or one that does
     * not exist, is not one this operation opens.
     */
    bool may_open(const ores::database::context& ctx, const std::string& request_id) {
        ores::inbox::service::approval_lifecycle lifecycle(ctx);
        const auto found = lifecycle.request(request_id);
        if (!found || found->kind_code != "refdata.counterparty_change")
            return false;
        const auto me = lifecycle.actor_account_id();
        const auto kind = lifecycle.kind(found->kind_code);
        return (me && found->requested_by == *me) ||
               (kind &&
                ores::service::messaging::has_permission(ctx, kind->decide_permission_code)) ||
               ores::service::messaging::has_permission(ctx, "inbox::approval_requests:read");
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
