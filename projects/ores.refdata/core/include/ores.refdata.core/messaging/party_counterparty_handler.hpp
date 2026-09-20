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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_nats_handler.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_CORE_MESSAGING_PARTY_COUNTERPARTY_HANDLER_HPP
#define ORES_REFDATA_CORE_MESSAGING_PARTY_COUNTERPARTY_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/messaging/party_counterparty_protocol.hpp"
#include "ores.refdata.core/service/party_counterparty_service.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cstddef>
#include <optional>

namespace ores::refdata::messaging {

namespace {
inline auto& party_counterparty_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.refdata.messaging.party_counterparty_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for party counterparties operations.
 */
class party_counterparty_handler {
public:
    party_counterparty_handler(ores::nats::service::client& nats,
                               ores::database::context ctx,
                               std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(party_counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::party_counterparty_service svc(req_ctx);
        if (auto req = decode<get_party_counterparties_request>(msg)) {
            get_party_counterparties_response resp;
            try {
                resp.party_counterparties = svc.list_party_counterparties(req->offset, req->limit);
                resp.total_available_count =
                    static_cast<int>(svc.get_total_party_counterparty_count());
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(party_counterparty_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
            BOOST_LOG_SEV(party_counterparty_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } else {
            BOOST_LOG_SEV(party_counterparty_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void list_by_party(ores::nats::message msg) {
        BOOST_LOG_SEV(party_counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::party_counterparty_service svc(req_ctx);
        if (auto req = decode<get_party_counterparties_by_party_request>(msg)) {
            get_party_counterparties_by_party_response resp;
            try {
                auto rows = svc.list_party_counterparties_by_party(
                    boost::lexical_cast<boost::uuids::uuid>(req->party_id),
                    req->offset,
                    req->limit);
                resp.total_available_count =
                    static_cast<int>(svc.get_total_party_counterparty_count_by_party(
                        boost::lexical_cast<boost::uuids::uuid>(req->party_id)));
                resp.party_counterparties.reserve(rows.size());
                for (auto& row : rows) {
                    party_counterparty_view view;
                    view.party_counterparty = std::move(row);
                    resp.party_counterparties.push_back(std::move(view));
                }
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(party_counterparty_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
            BOOST_LOG_SEV(party_counterparty_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } else {
            BOOST_LOG_SEV(party_counterparty_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(party_counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::party_counterparties:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::party_counterparty_service svc(req_ctx);
        if (auto req = decode<save_party_counterparty_request>(msg)) {
            save_party_counterparty_response resp;
            try {
                svc.save_party_counterparties(req->party_counterparties);
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(party_counterparty_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
            BOOST_LOG_SEV(party_counterparty_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } else {
            BOOST_LOG_SEV(party_counterparty_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void remove(ores::nats::message msg) {
        BOOST_LOG_SEV(party_counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::party_counterparties:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::party_counterparty_service svc(req_ctx);
        if (auto req = decode<delete_party_counterparty_request>(msg)) {
            if (req->party_ids.size() != req->counterparty_ids.size()) {
                BOOST_LOG_SEV(party_counterparty_handler_lg(), warn)
                    << msg.subject << " rejected: key vectors differ in length, "
                    << req->party_ids.size() << " and " << req->counterparty_ids.size();
                error_reply(nats_, msg, ores::service::error_code::bad_request);
                return;
            }
            delete_party_counterparty_response resp;
            try {
                for (std::size_t i = 0; i < req->party_ids.size(); ++i) {
                    const auto party_id_key =
                        boost::lexical_cast<boost::uuids::uuid>(req->party_ids[i]);
                    const auto counterparty_id_key =
                        boost::lexical_cast<boost::uuids::uuid>(req->counterparty_ids[i]);
                    svc.remove_party_counterparty(party_id_key, counterparty_id_key);
                }
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(party_counterparty_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
            BOOST_LOG_SEV(party_counterparty_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } else {
            BOOST_LOG_SEV(party_counterparty_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void count_by_party(ores::nats::message msg) {
        BOOST_LOG_SEV(party_counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::party_counterparty_service svc(req_ctx);
        if (auto req = decode<count_party_counterparties_by_party_request>(msg)) {
            count_party_counterparties_by_party_response resp;
            try {
                resp.total_available_count =
                    static_cast<int>(svc.get_total_party_counterparty_count_by_party(
                        boost::lexical_cast<boost::uuids::uuid>(req->party_id)));
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(party_counterparty_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.total_available_count = 0;
            }
            BOOST_LOG_SEV(party_counterparty_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } else {
            BOOST_LOG_SEV(party_counterparty_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void count_by_counterparty(ores::nats::message msg) {
        BOOST_LOG_SEV(party_counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::party_counterparty_service svc(req_ctx);
        if (auto req = decode<count_party_counterparties_by_counterparty_request>(msg)) {
            count_party_counterparties_by_counterparty_response resp;
            try {
                resp.total_available_count =
                    static_cast<int>(svc.get_total_party_counterparty_count_by_counterparty(
                        boost::lexical_cast<boost::uuids::uuid>(req->counterparty_id)));
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(party_counterparty_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.total_available_count = 0;
            }
            BOOST_LOG_SEV(party_counterparty_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } else {
            BOOST_LOG_SEV(party_counterparty_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::refdata::messaging

#endif
