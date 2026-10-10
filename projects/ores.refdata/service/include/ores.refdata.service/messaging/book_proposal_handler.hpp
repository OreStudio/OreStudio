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
#ifndef ORES_REFDATA_SERVICE_MESSAGING_BOOK_PROPOSAL_HANDLER_HPP
#define ORES_REFDATA_SERVICE_MESSAGING_BOOK_PROPOSAL_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/messaging/book_proposal_operations_protocol.hpp"
#include "ores.refdata.service/messaging/book_proposal_mapping.hpp"
#include "ores.refdata.service/service/book_proposal.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <optional>
#include <string>
#include <utility>

namespace ores::refdata::messaging {

/**
 * @brief Answers the operations that propose changes to books.
 *
 * Every operation acts as the signed-in person. The handler proves the
 * request, checks the permission each line's write needs, and leaves the rest
 * to the proposal service.
 */
class book_proposal_handler {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.refdata.messaging.book_proposal_handler");
        return instance;
    }

    static ores::utility::domain::result
    result_of(ores::utility::domain::outcome o, std::string code, std::string message) {
        return ores::utility::domain::result{
            .outcome = o, .code = std::move(code), .message = std::move(message), .fields = {}};
    }

public:
    book_proposal_handler(ores::nats::service::client& nats,
                          ores::database::context ctx,
                          std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves refdata.v1.ops.preview_book_changes.
     */
    void preview(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        auto req = ores::service::messaging::decode<preview_book_changes_request>(msg);
        if (!req) {
            ores::service::messaging::error_reply(
                nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        if (!admitted(*ctx, msg, req->lines))
            return;
        try {
            const auto previewed = service::book_proposal_service(*ctx).preview(req->lines);
            ores::service::messaging::reply(nats_,
                                            msg,
                                            preview_book_changes_response{
                                                .result = result_of(outcome::ok, "", ""),
                                                .lines = to_outcomes(previewed),
                                                .part_codes = previewed.part_codes});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), ores::logging::error) << "Error previewing book changes: " << e.what();
            ores::service::messaging::reply(
                nats_,
                msg,
                preview_book_changes_response{
                    .result = result_of(outcome::failed,
                                        "preview_failed",
                                        "The changes could not be previewed."),
                    .lines = {},
                    .part_codes = {}});
        }
    }

    /**
     * @brief Serves refdata.v1.ops.raise_book_changes.
     */
    void raise(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        auto req = ores::service::messaging::decode<raise_book_changes_request>(msg);
        if (!req) {
            ores::service::messaging::error_reply(
                nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        if (!admitted(*ctx, msg, req->lines))
            return;
        try {
            const auto proposal = service::book_proposal_service(*ctx).raise(req->lines, req->reason);
            raise_book_changes_response response{.result = {},
                                                 .request_id = std::nullopt,
                                                 .lines = to_outcomes(proposal.preview),
                                                 .part_codes = proposal.preview.part_codes};
            if (proposal.request) {
                response.request_id = proposal.request->id;
                response.result = result_of(outcome::ok, "", "");
            } else {
                response.result = result_of(outcome::invalid, refusal_code(proposal), proposal.message);
            }
            ores::service::messaging::reply(nats_, msg, response);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), ores::logging::error) << "Error raising book changes: " << e.what();
            ores::service::messaging::reply(
                nats_,
                msg,
                raise_book_changes_response{
                    .result = result_of(outcome::failed,
                                        "raise_failed",
                                        "The changes could not be raised."),
                    .request_id = std::nullopt,
                    .lines = {},
                    .part_codes = {}});
        }
    }

private:
    /**
     * @brief Why a proposal raised nothing, as a code a screen can switch on.
     */
    static std::string refusal_code(const service::book_proposal& proposal) {
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
                  const std::vector<domain::book_change>& lines) {
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
