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
#ifndef ORES_DQ_MESSAGING_PUBLICATION_HANDLER_HPP
#define ORES_DQ_MESSAGING_PUBLICATION_HANDLER_HPP

#include <optional>
#include <stdexcept>
#include <boost/uuid/string_generator.hpp>
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.dq/messaging/publication_protocol.hpp"
#include "ores.dq/messaging/publish_bundle_protocol.hpp"
#include "ores.dq/service/publication_service.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::dq::messaging {

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using namespace ores::logging;

namespace {
inline auto& publication_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.dq.messaging.publication_handler");
    return instance;
}
} // namespace

class publication_handler {
public:
    publication_handler(
        ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list_publications(ores::nats::message msg) {
        BOOST_LOG_SEV(publication_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_publications_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(publication_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::publication_service svc(ctx);
        try {
            boost::uuids::string_generator gen;
            const auto pubs =
                svc.get_publication_history(gen(req->dataset_id));
            get_publications_response resp;
            resp.success = true;
            resp.publications = pubs;
            BOOST_LOG_SEV(publication_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(publication_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_publications_response resp;
            resp.success = false;
            resp.message = e.what();
            reply(nats_, msg, resp);
        }
    }

    void publish_bundle(ores::nats::message msg) {
        BOOST_LOG_SEV(publication_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<publish_bundle_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(publication_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::publication_service svc(ctx);
        try {
            const auto result = svc.publish_bundle(
                req->bundle_code,
                req->mode,
                req->published_by,
                req->atomic,
                req->params_json);
            publish_bundle_response resp;
            resp.success = result.success;
            resp.error_message = result.error_message;
            resp.datasets_succeeded =
                static_cast<int>(result.datasets_succeeded);
            resp.total_records_inserted =
                static_cast<int>(result.total_records_inserted);
            resp.total_records_updated =
                static_cast<int>(result.total_records_updated);
            for (const auto& dr : result.dataset_results) {
                bundle_dataset_result bdr;
                bdr.dataset_code = dr.dataset_code;
                bdr.success = (dr.status == "success");
                bdr.error_message = dr.error_message;
                bdr.records_inserted =
                    static_cast<int>(dr.records_inserted);
                bdr.records_updated =
                    static_cast<int>(dr.records_updated);
                bdr.records_skipped =
                    static_cast<int>(dr.records_skipped);
                bdr.records_deleted =
                    static_cast<int>(dr.records_deleted);
                resp.dataset_results.push_back(std::move(bdr));
            }
            BOOST_LOG_SEV(publication_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(publication_handler_lg(), error) << msg.subject << " failed: " << e.what();
            publish_bundle_response resp;
            resp.success = false;
            resp.error_message = e.what();
            reply(nats_, msg, resp);
        }
    }

private:

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::dq::messaging

#endif
