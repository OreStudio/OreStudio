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
using ores::service::messaging::log_handler_entry;
using namespace ores::logging;

class party_handler {
public:
    party_handler(ores::nats::service::client& nats,
                  ores::database::context ctx,
                  std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(party_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::party_service svc(ctx);
        get_parties_response resp;
        auto req = decode<get_parties_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            reply(nats_, msg, resp);
            return;
        }
        try {
            resp.parties = svc.list_parties(static_cast<std::uint32_t>(req->offset),
                                            static_cast<std::uint32_t>(req->limit));
            resp.total_available_count = static_cast<int>(svc.count_parties());
            BOOST_LOG_SEV(party_handler_lg(), debug) << "Completed " << msg.subject;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(party_handler_lg(), error) << msg.subject << " failed: " << e.what();
        }
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        using ores::service::messaging::is_workflow_command;
        using ores::service::messaging::extract_workflow_header;
        using ores::service::messaging::publish_step_completion;
        using ores::service::messaging::check_step_idempotency;
        using ores::service::messaging::workflow_step_id_header;
        using ores::service::messaging::workflow_instance_id_header;
        using ores::service::messaging::workflow_tenant_id_header;

        // Workflow step command: bypass JWT auth; use X-Tenant-Id for context.
        if (is_workflow_command(msg)) {
            const auto step_id = extract_workflow_header(msg, workflow_step_id_header);
            const auto inst_id = extract_workflow_header(msg, workflow_instance_id_header);
            const auto tenant_id = extract_workflow_header(msg, workflow_tenant_id_header);

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

            auto req = decode<save_party_request>(msg);
            if (!req) {
                publish_step_completion(nats_,
                                        step_id,
                                        inst_id,
                                        ores::workflow::messaging::step_outcome::failed,
                                        "",
                                        "Failed to decode save_party_request");
                return;
            }
            try {
                using ores::database::service::tenant_context;
                auto wf_ctx = tenant_context::with_tenant(ctx_, tenant_id);
                service::party_service svc(wf_ctx);
                svc.save_party(req->data);
                BOOST_LOG_SEV(party_handler_lg(), debug)
                    << "Workflow step completed: " << msg.subject;
                publish_step_completion(nats_,
                                        step_id,
                                        inst_id,
                                        ores::workflow::messaging::step_outcome::completed,
                                        rfl::json::write(save_party_response{.success = true}),
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

        [[maybe_unused]] const auto correlation_id = log_handler_entry(party_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "refdata::parties:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::party_service svc(ctx);
        auto req = decode<save_party_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            svc.save_party(req->data);
            BOOST_LOG_SEV(party_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, save_party_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(party_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_party_response{.success = false, .message = e.what()});
        }
    }

    void remove(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(party_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "refdata::parties:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::party_service svc(ctx);
        auto req = decode<delete_party_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            boost::uuids::string_generator gen;
            for (const auto& id_str : req->ids)
                svc.remove_party(gen(id_str));
            BOOST_LOG_SEV(party_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, delete_party_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(party_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_party_response{.success = false, .message = e.what()});
        }
    }

    void read_for_cache(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(party_handler_lg(), msg);
        auto req = decode<read_parties_for_cache_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            reply(nats_,
                  msg,
                  read_parties_for_cache_response{.success = false,
                                                  .message = "Failed to decode request"});
            return;
        }
        try {
            using ores::database::service::tenant_context;
            auto tctx = tenant_context::with_tenant(ctx_, req->tenant_id);
            service::party_service svc(tctx);
            auto parties = svc.list_parties();
            BOOST_LOG_SEV(party_handler_lg(), debug)
                << "Completed " << msg.subject << " (tenant=" << req->tenant_id
                << ", count=" << parties.size() << ")";
            reply(nats_,
                  msg,
                  read_parties_for_cache_response{.success = true, .parties = std::move(parties)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(party_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(
                nats_, msg, read_parties_for_cache_response{.success = false, .message = e.what()});
        }
    }

    void history(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(party_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::party_service svc(ctx);
        auto req = decode<get_party_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            boost::uuids::string_generator gen;
            auto h = svc.get_party_history(gen(req->id));
            BOOST_LOG_SEV(party_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, get_party_history_response{.success = true, .history = std::move(h)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(party_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_party_history_response{.success = false, .message = e.what()});
        }
    }

    void hierarchy(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(party_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::party_service svc(ctx);
        auto req = decode<get_party_hierarchy_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            reply(nats_,
                  msg,
                  get_party_hierarchy_response{.success = false,
                                               .message = "Failed to decode request"});
            return;
        }
        try {
            boost::uuids::string_generator gen;
            auto roots = svc.get_hierarchy(gen(req->root_id), req->from_root);
            BOOST_LOG_SEV(party_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_,
                  msg,
                  get_party_hierarchy_response{.success = true, .roots = std::move(roots)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(party_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_party_hierarchy_response{.success = false, .message = e.what()});
        }
    }

    void composite_as_of(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(party_handler_lg(), msg);
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
            boost::uuids::string_generator gen;
            const auto id_uuid = gen(req->id);
            service::party_service party_svc(ctx);
            const auto version = static_cast<std::uint32_t>(req->version);
            auto current = party_svc.get_party_at_version(id_uuid, version);
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
            auto next = party_svc.get_party_at_version(id_uuid, version + 1);
            const auto window_end =
                next ? next->recorded_at :
                       std::chrono::system_clock::now() + std::chrono::hours(24 * 365 * 100);

            service::party_identifier_service identifier_svc(ctx);
            auto identifiers = identifier_svc.list_party_identifiers_by_party_id_as_of(
                req->id, current->recorded_at, window_end);

            service::party_contact_information_service contact_svc(ctx);
            auto contacts = contact_svc.list_party_contact_informations_by_party_as_of(
                id_uuid, current->recorded_at, window_end);

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
