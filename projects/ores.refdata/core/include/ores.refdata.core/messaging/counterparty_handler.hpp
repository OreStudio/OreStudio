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
#ifndef ORES_REFDATA_CORE_MESSAGING_COUNTERPARTY_HANDLER_HPP
#define ORES_REFDATA_CORE_MESSAGING_COUNTERPARTY_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.core/service/counterparty_contact_information_service.hpp"
#include "ores.refdata.core/service/counterparty_identifier_service.hpp"
#include "ores.refdata.core/service/counterparty_service.hpp"
#include "ores.refdata.core/service/csa_eligible_currency_service.hpp"
#include "ores.refdata.core/service/csa_service.hpp"
#include "ores.refdata.core/service/netting_agreement_service.hpp"
#include "ores.refdata.core/service/netting_set_identifier_service.hpp"
#include "ores.refdata.core/service/netting_set_service.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>

namespace ores::refdata::messaging {

namespace {
inline auto& counterparty_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.refdata.messaging.counterparty_handler");
    return instance;
}
}

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for counterparty operations.
 */
class counterparty_handler {
public:
    counterparty_handler(ores::nats::service::client& nats,
                         ores::database::context ctx,
                         std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves refdata.v1.counterparties.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_counterparties(ores::nats::message msg) {
        BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::counterparties:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<list_counterparties_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::counterparty_service svc(req_ctx);
        try {
            auto response = svc.list_counterparties(*req);
            BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(counterparty_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_counterparties_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.counterparties.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_counterparty(ores::nats::message msg) {
        BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::counterparties:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_counterparty_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::counterparty_service svc(req_ctx);
        try {
            auto response = svc.get_counterparty(*req);
            BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(counterparty_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_counterparty_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.counterparties.get_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_many_counterparties(ores::nats::message msg) {
        BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::counterparties:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_many_counterparties_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::counterparty_service svc(req_ctx);
        try {
            auto response = svc.get_many_counterparties(*req);
            BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(counterparty_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_many_counterparties_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.counterparties.put.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_counterparty(ores::nats::message msg) {
        BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::counterparties:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_counterparty_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::counterparty_service svc(req_ctx);
        try {
            auto response = svc.put_counterparty(*req);
            BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(counterparty_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            put_counterparty_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.counterparties.put_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_many_counterparties(ores::nats::message msg) {
        BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::counterparties:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_many_counterparties_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::counterparty_service svc(req_ctx);
        try {
            auto response = svc.put_many_counterparties(*req);
            BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(counterparty_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            put_many_counterparties_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.counterparties.delete.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_counterparty(ores::nats::message msg) {
        BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::counterparties:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_counterparty_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::counterparty_service svc(req_ctx);
        try {
            auto response = svc.delete_counterparty(*req);
            BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(counterparty_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            delete_counterparty_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.counterparties.delete_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_many_counterparties(ores::nats::message msg) {
        BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::counterparties:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_many_counterparties_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::counterparty_service svc(req_ctx);
        try {
            auto response = svc.delete_many_counterparties(*req);
            BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(counterparty_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            delete_many_counterparties_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.counterparties_versions.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_counterparty_versions(ores::nats::message msg) {
        BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::counterparties:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<list_counterparty_versions_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::counterparty_service svc(req_ctx);
        try {
            auto response = svc.list_counterparty_versions(*req);
            BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(counterparty_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_counterparty_versions_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.counterparties_versions.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_counterparty_version(ores::nats::message msg) {
        BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::counterparties:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_counterparty_version_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::counterparty_service svc(req_ctx);
        try {
            auto response = svc.get_counterparty_version(*req);
            BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(counterparty_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_counterparty_version_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    void composite_as_of(ores::nats::message msg) {
        BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::counterparties:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_counterparty_composite_as_of_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        try {
            service::counterparty_service cpty_svc(req_ctx);
            const auto version = static_cast<std::uint32_t>(req->version);
            auto current = cpty_svc.get_counterparty_at_version(req->id, version);
            if (!current) {
                reply(nats_,
                      msg,
                      get_counterparty_composite_as_of_response{
                          .success = false,
                          .message =
                              "No such counterparty version: " + boost::uuids::to_string(req->id) +
                              " v" + std::to_string(version)});
                return;
            }

            // See party_handler::composite_as_of — windows are contiguous
            // by construction; the domain object doesn't surface valid_to.
            auto next = cpty_svc.get_counterparty_at_version(req->id, version + 1);
            const auto window_end =
                next ? next->recorded_at :
                       std::chrono::system_clock::now() + std::chrono::hours(24 * 365 * 100);

            service::counterparty_identifier_service identifier_svc(req_ctx);
            auto identifiers =
                identifier_svc.list_counterparty_identifiers_by_counterparty_id_as_of(
                    boost::uuids::to_string(req->id), current->recorded_at, window_end);

            service::counterparty_contact_information_service contact_svc(req_ctx);
            auto contacts =
                contact_svc.list_counterparty_contact_informations_by_counterparty_id_as_of(
                    boost::uuids::to_string(req->id), current->recorded_at, window_end);

            BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_,
                  msg,
                  get_counterparty_composite_as_of_response{.success = true,
                                                            .counterparty = std::move(*current),
                                                            .identifiers = std::move(identifiers),
                                                            .contacts = std::move(contacts)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(counterparty_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_,
                  msg,
                  get_counterparty_composite_as_of_response{.success = false, .message = e.what()});
        }
    }

    void put_composite(ores::nats::message msg) {
        BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::counterparties:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_counterparty_composite_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        try {
            // One transaction over every row the confirm staged. The unit of
            // work rolls back when it is destroyed uncommitted, so a refusal
            // anywhere leaves none of the eight writes behind. Each service is
            // built on the unit of work's context, which is what makes its
            // repository calls join the transaction rather than open their own.
            ores::database::repository::unit_of_work uow(req_ctx);
            const auto& uctx = uow.ctx();
            const auto reason =
                req->intent.reason_code.empty() ?
                    std::string(ores::service::messaging::change_reasons::new_record) :
                    req->intent.reason_code;
            // The reason is the caller's, and the commentary with it; the audit
            // stamps stay the services' to fill.
            const auto apply_intent = [&](auto& row) {
                if (row.change_reason_code.empty())
                    row.change_reason_code = reason;
                row.change_commentary = req->intent.commentary;
            };

            auto counterparty = req->counterparty;
            apply_intent(counterparty);
            service::counterparty_service counterparties(uctx);
            counterparties.save_counterparty(counterparty);

            auto identifiers = req->identifiers;
            for (auto& row : identifiers)
                apply_intent(row);
            service::counterparty_identifier_service identifier_service(uctx);
            identifier_service.save_counterparty_identifiers(identifiers);

            auto contacts = req->contacts;
            for (auto& row : contacts)
                apply_intent(row);
            service::counterparty_contact_information_service contact_service(uctx);
            contact_service.save_counterparty_contact_informations(contacts);

            auto agreements = req->agreements;
            for (auto& row : agreements)
                apply_intent(row);
            service::netting_agreement_service agreement_service(uctx);
            agreement_service.save_netting_agreements(agreements);

            auto netting_sets = req->netting_sets;
            for (auto& row : netting_sets)
                apply_intent(row);
            service::netting_set_service netting_set_service(uctx);
            netting_set_service.save_netting_sets(netting_sets);

            auto set_identifiers = req->netting_set_identifiers;
            for (auto& row : set_identifiers)
                apply_intent(row);
            service::netting_set_identifier_service set_identifier_service(uctx);
            set_identifier_service.save_netting_set_identifiers(set_identifiers);

            auto csas = req->csas;
            for (auto& row : csas)
                apply_intent(row);
            service::csa_service csa_service(uctx);
            csa_service.save_csas(csas);

            auto currencies = req->eligible_currencies;
            for (auto& row : currencies)
                apply_intent(row);
            service::csa_eligible_currency_service eligible_currency_service(uctx);
            eligible_currency_service.save_csa_eligible_currencies(currencies);

            uow.commit();

            // Read the row back from the committed transaction, so the reply
            // carries the version and the audit stamps the store gave it.
            service::counterparty_service reader(req_ctx);
            auto written = reader.find_counterparty(counterparty.id);
            BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_,
                  msg,
                  put_counterparty_composite_response{
                      .result = {},
                      .counterparty = written ? std::move(*written) : std::move(counterparty)});
        } catch (const std::exception& e) {
            // The store refused one of the rows and the transaction is gone
            // with it. The reason is the store's own words, so nothing here
            // states a code the catalogue could own instead.
            BOOST_LOG_SEV(counterparty_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            put_counterparty_composite_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
