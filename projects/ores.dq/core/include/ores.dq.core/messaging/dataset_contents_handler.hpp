/** -*- mode: c++-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
#ifndef ORES_DQ_CORE_MESSAGING_DATASET_CONTENTS_HANDLER_HPP
#define ORES_DQ_CORE_MESSAGING_DATASET_CONTENTS_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/messaging/dataset_contents_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/service/publish_service.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>

namespace ores::dq::messaging {

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::reply;

/**
 * @brief The contents verb: the tables a dataset stages into, and one page of
 * one table's rows.
 *
 * The answer is rows of tables the dataset names indirectly, through its
 * artefact type, so the shape is the store's and not a row's. It is hand-written
 * beside the =dataset_contents= operation model that declares its messages,
 * because a family's columns differ from family to family and the rows are
 * carried as JSON rather than as a message the generator can state.
 *
 * Reading is what it does, so it asks for the dataset read and not the write the
 * publish verbs ask for.
 */
class dataset_contents_handler {
public:
    dataset_contents_handler(ores::nats::service::client& nats,
                             const ores::database::context& ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(ctx)
        , verifier_(std::move(verifier)) {}

    void dataset_contents(ores::nats::message msg) {
        ORES_LOG_SEV(lg(), debug) << "Handling " << msg.subject;

        auto req = decode<dataset_contents_request>(msg);
        if (!req) {
            ORES_LOG_SEV(lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "dq::datasets:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        dataset_contents_response resp;
        try {
            service::publish_service svc(ctx);
            resp = svc.read_dataset_contents(
                req->dataset_code, req->table_name, req->limit, req->offset);
        } catch (const std::exception& e) {
            ORES_LOG_SEV(lg(), error) << "Failed to read the contents: " << e.what();
            resp.success = false;
            resp.error_message = e.what();
        }

        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;

    /**
     * @brief The service's own context, held as a value and never as a
     * reference: registration receives it as a temporary, so a reference would
     * dangle as soon as registration returns.
     */
    ores::database::context ctx_;

    std::optional<ores::security::jwt::jwt_authenticator> verifier_;

    static ores::logging::logger_t& lg() {
        static auto instance =
            ores::logging::make_logger("ores.dq.messaging.dataset_contents_handler");
        return instance;
    }
};

} // namespace ores::dq::messaging

#endif
