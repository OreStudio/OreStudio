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
#ifndef ORES_REFDATA_CORE_MESSAGING_PARTY_HANDLER_HPP
#define ORES_REFDATA_CORE_MESSAGING_PARTY_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.refdata.core/service/party_contact_information_service.hpp"
#include "ores.refdata.core/service/party_identifier_service.hpp"
#include "ores.refdata.core/service/party_service.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <boost/uuid/string_generator.hpp>
#include <chrono>
#include <optional>

namespace ores::refdata::messaging {

namespace {
inline auto& party_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.refdata.messaging.party_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for party operations.
 */
class party_handler {
public:
    party_handler(ores::nats::service::client& nats,
                  ores::database::context ctx,
                  std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves refdata.v1.parties.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_parties(ores::nats::message msg) {
        BOOST_LOG_SEV(party_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        auto req = decode<list_parties_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::party_service svc(req_ctx);
        try {
            auto response = svc.list_parties(*req);
            BOOST_LOG_SEV(party_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(party_handler_lg(), error) << msg.subject << " failed: " << e.what();
            list_parties_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.parties.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_party(ores::nats::message msg) {
        BOOST_LOG_SEV(party_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        auto req = decode<get_party_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::party_service svc(req_ctx);
        try {
            auto response = svc.get_party(*req);
            BOOST_LOG_SEV(party_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(party_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_party_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.parties.get_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_many_parties(ores::nats::message msg) {
        BOOST_LOG_SEV(party_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        auto req = decode<get_many_parties_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::party_service svc(req_ctx);
        try {
            auto response = svc.get_many_parties(*req);
            BOOST_LOG_SEV(party_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(party_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_many_parties_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.parties.put.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_party(ores::nats::message msg) {
        using ores::service::messaging::is_workflow_command;
        using ores::service::messaging::extract_workflow_header;
        using ores::service::messaging::publish_step_completion;
        using ores::service::messaging::check_step_idempotency;
        using ores::workflow::messaging::step_id_header;
        using ores::workflow::messaging::instance_id_header;
        using ores::workflow::messaging::tenant_id_header;

        // Workflow step command: bypass JWT auth; use X-Tenant-Id for context.
        if (is_workflow_command(msg)) {
            const auto step_id = extract_workflow_header(msg, step_id_header);
            const auto inst_id = extract_workflow_header(msg, instance_id_header);
            const auto tenant_id = extract_workflow_header(msg, tenant_id_header);

            // Idempotency guard: replay cached result if this step already completed.
            if (auto cached = check_step_idempotency(nats_, step_id)) {
                publish_step_completion(nats_,
                                        step_id,
                                        inst_id,
                                        cached->outcome,
                                        cached->result_json,
                                        cached->error_message,
                                        cached->log);
                return;
            }

            auto req = decode<put_party_request>(msg);
            if (!req) {
                publish_step_completion(nats_,
                                        step_id,
                                        inst_id,
                                        ores::workflow::messaging::step_outcome::failed,
                                        "",
                                        "Failed to decode put_party_request");
                return;
            }
            try {
                using ores::database::service::tenant_context;
                auto wf_ctx = tenant_context::with_tenant(ctx_, tenant_id);
                service::party_service svc(wf_ctx);
                // The service answers the canonical response, whose result
                // states the outcome. A refusal is the step failing.
                const auto resp = svc.put_party(*req);
                if (resp.result.outcome != ores::utility::domain::outcome::ok) {
                    publish_step_completion(nats_,
                                            step_id,
                                            inst_id,
                                            ores::workflow::messaging::step_outcome::failed,
                                            "",
                                            resp.result.message);
                    return;
                }
                BOOST_LOG_SEV(party_handler_lg(), debug)
                    << "Workflow step completed: " << msg.subject;
                publish_step_completion(nats_,
                                        step_id,
                                        inst_id,
                                        ores::workflow::messaging::step_outcome::completed,
                                        rfl::json::write(resp),
                                        "");
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(party_handler_lg(), error)
                    << "Workflow step failed: " << msg.subject << " — " << e.what();
                publish_step_completion(nats_,
                                        step_id,
                                        inst_id,
                                        ores::workflow::messaging::step_outcome::failed,
                                        "",
                                        e.what());
            }
            return;
        }
        BOOST_LOG_SEV(party_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::parties:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_party_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::party_service svc(req_ctx);
        try {
            auto response = svc.put_party(*req);
            BOOST_LOG_SEV(party_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(party_handler_lg(), error) << msg.subject << " failed: " << e.what();
            put_party_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.parties.put_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_many_parties(ores::nats::message msg) {
        BOOST_LOG_SEV(party_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::parties:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_many_parties_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::party_service svc(req_ctx);
        try {
            auto response = svc.put_many_parties(*req);
            BOOST_LOG_SEV(party_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(party_handler_lg(), error) << msg.subject << " failed: " << e.what();
            put_many_parties_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.parties.delete.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_party(ores::nats::message msg) {
        BOOST_LOG_SEV(party_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::parties:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_party_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::party_service svc(req_ctx);
        try {
            auto response = svc.delete_party(*req);
            BOOST_LOG_SEV(party_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(party_handler_lg(), error) << msg.subject << " failed: " << e.what();
            delete_party_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.parties.delete_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_many_parties(ores::nats::message msg) {
        BOOST_LOG_SEV(party_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::parties:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_many_parties_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::party_service svc(req_ctx);
        try {
            auto response = svc.delete_many_parties(*req);
            BOOST_LOG_SEV(party_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(party_handler_lg(), error) << msg.subject << " failed: " << e.what();
            delete_many_parties_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.parties_versions.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_party_versions(ores::nats::message msg) {
        BOOST_LOG_SEV(party_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        auto req = decode<list_party_versions_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::party_service svc(req_ctx);
        try {
            auto response = svc.list_party_versions(*req);
            BOOST_LOG_SEV(party_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(party_handler_lg(), error) << msg.subject << " failed: " << e.what();
            list_party_versions_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.parties_versions.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_party_version(ores::nats::message msg) {
        BOOST_LOG_SEV(party_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        auto req = decode<get_party_version_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::party_service svc(req_ctx);
        try {
            auto response = svc.get_party_version(*req);
            BOOST_LOG_SEV(party_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(party_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_party_version_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    void composite_as_of(ores::nats::message msg) {
        BOOST_LOG_SEV(party_handler_lg(), debug) << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        auto req = decode<get_party_composite_as_of_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            reply(nats_,
                  msg,
                  get_party_composite_as_of_response{.success = false,
                                                     .message = "Failed to decode request"});
            return;
        }
        try {
            service::party_service party_svc(ctx);
            const auto version = static_cast<std::uint32_t>(req->version);
            auto current = party_svc.get_party_at_version(req->id, version);
            if (!current) {
                reply(nats_,
                      msg,
                      get_party_composite_as_of_response{.success = false,
                                                         .message =
                                                             "No such party version: " + req->id +
                                                             " v" + std::to_string(version)});
                return;
            }

            // Windows are contiguous by construction: the next version's
            // valid_from is this version's valid_to. If there is no next
            // version, this is the current one — its window is still open,
            // so bound it with a safely-far-future instant instead (see the
            // "Temporal composite entity versioning" architecture doc; the
            // domain object does not surface valid_to directly).
            auto next = party_svc.get_party_at_version(req->id, version + 1);
            const auto window_end =
                next ? next->recorded_at :
                       std::chrono::system_clock::now() + std::chrono::hours(24 * 365 * 100);

            service::party_identifier_service identifier_svc(ctx);
            auto identifiers = identifier_svc.list_party_identifiers_by_party_id_as_of(
                req->id, current->recorded_at, window_end);

            service::party_contact_information_service contact_svc(ctx);
            auto contacts = contact_svc.list_party_contact_informations_by_party_id_as_of(
                req->id, current->recorded_at, window_end);

            BOOST_LOG_SEV(party_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_,
                  msg,
                  get_party_composite_as_of_response{.success = true,
                                                     .party = std::move(*current),
                                                     .identifiers = std::move(identifiers),
                                                     .contacts = std::move(contacts)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(party_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_,
                  msg,
                  get_party_composite_as_of_response{.success = false, .message = e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::refdata::messaging

#endif
