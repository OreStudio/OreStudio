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
#ifndef ORES_REFDATA_SERVICE_MESSAGING_PRODUCT_APPROVAL_HANDLER_HPP
#define ORES_REFDATA_SERVICE_MESSAGING_PRODUCT_APPROVAL_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/messaging/product_approval_operations_protocol.hpp"
#include "ores.refdata.service/service/product_approval.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <optional>
#include <string>
#include <utility>

namespace ores::refdata::messaging {

/**
 * @brief Answers the resolution of the approval chain for a list of books.
 *
 * The adapter decides nothing: it proves the request, checks the permissions the
 * answer needs, asks the database's own resolution and replies with what it
 * answered.
 */
class product_approval_handler {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.refdata.messaging.product_approval_handler");
        return instance;
    }

    static ores::utility::domain::result
    result_of(ores::utility::domain::outcome o, std::string code, std::string message) {
        return ores::utility::domain::result{
            .outcome = o, .code = std::move(code), .message = std::move(message), .fields = {}};
    }

public:
    product_approval_handler(ores::nats::service::client& nats,
                             const ores::database::context& ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(ctx)
        , verifier_(verifier) {}

    /**
     * @brief Serves refdata.v1.ops.get_book_product_approvals.
     */
    void get(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        ORES_LOG_SEV(lg(), ores::logging::debug) << "Handling " << msg.subject;
        auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx) {
            ores::service::messaging::error_reply(nats_, msg, ctx.error());
            return;
        }
        /*
         * A book's set is resolved through its desk and its party, so reading it
         * reads all three levels. The booking path grants a trader the three for
         * the same reason: the answer names the sets of the levels above as much
         * as the book's own.
         */
        for (const auto* permission : {"refdata::book_product_types:read",
                                       "refdata::business_unit_product_types:read",
                                       "refdata::party_product_types:read"}) {
            if (!ores::service::messaging::has_permission(*ctx, permission)) {
                ores::service::messaging::error_reply(
                    nats_, msg, ores::service::error_code::forbidden);
                return;
            }
        }
        auto req = ores::service::messaging::decode<get_book_product_approvals_request>(msg);
        if (!req) {
            ores::service::messaging::error_reply(
                nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        try {
            ores::service::messaging::reply(
                nats_,
                msg,
                get_book_product_approvals_response{
                    .result = result_of(outcome::ok, "", ""),
                    .approvals = service::book_product_approvals(*ctx, req->book_ids)});
        } catch (const std::exception& e) {
            ORES_LOG_SEV(lg(), ores::logging::error)
                << "Error reading the product types a book permits: " << e.what();
            ores::service::messaging::reply(
                nats_,
                msg,
                get_book_product_approvals_response{
                    .result = result_of(outcome::failed,
                                        "read_failed",
                                        "The product types the books permit could not be read."),
                    .approvals = {}});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
