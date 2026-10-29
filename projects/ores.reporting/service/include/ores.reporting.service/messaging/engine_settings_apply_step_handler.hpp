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
#ifndef ORES_REPORTING_SERVICE_MESSAGING_ENGINE_SETTINGS_APPLY_STEP_HANDLER_HPP
#define ORES_REPORTING_SERVICE_MESSAGING_ENGINE_SETTINGS_APPLY_STEP_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.inbox.api/workflow/apply_step_command.hpp"
#include "ores.inbox.core/service/approval_announcer.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.reporting.service/service/engine_settings_proposal.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include <exception>
#include <string>
#include <utility>

namespace ores::reporting::messaging {

/**
 * @brief Answers the commands of the engine run settings apply workflow.
 *
 * The workflow runs for the tenant, not for a person, so the handler acts as the
 * service. The engine run settings belong to a party, and the proposal service
 * writes the part under the configuration's own party, which it reads from the
 * configuration and not from the caller.
 *
 * Every command is idempotent. A step the engine already holds a terminal record
 * of is answered from that record, and a line the apply wrote is marked applied
 * in the same transaction, so a repeated command finds nothing to do.
 */
class engine_settings_apply_step_handler {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance = ores::logging::make_logger(
            "ores.reporting.messaging.engine_settings_apply_step_handler");
        return instance;
    }

public:
    engine_settings_apply_step_handler(ores::nats::service::client& nats,
                                       ores::database::context ctx)
        : nats_(nats)
        , ctx_(std::move(ctx)) {}

    /**
     * @brief Serves reporting.v1.steps.recheck_engine_settings_changes.
     *
     * A group that moved since the raise will fail the apply. The request ends
     * in apply_failed here, before anything is written, with the group and the
     * reason.
     */
    void recheck(ores::nats::message msg) {
        run(msg, [](const auto& ctx, const std::string& request_id) {
            // A run resumed after the request ended has nothing left to check:
            // the request is no longer approved, and its state stands.
            const auto request = ores::inbox::service::approval_lifecycle(ctx).request(request_id);
            if (!request || request->state_code != "approved") {
                return step_outcome{.completed = false,
                                    .error = "The request is " +
                                             (request ? request->state_code : std::string("gone")) +
                                             " and cannot be applied."};
            }
            const auto refusals =
                service::engine_settings_proposal_service(ctx).refusals(request_id);
            for (const auto& refusal : refusals) {
                if (refusal.empty())
                    continue;
                ores::inbox::service::approval_lifecycle(ctx).fail_apply(request_id, refusal);
                return step_outcome{.completed = false, .error = refusal};
            }
            return step_outcome{.completed = true, .error = {}};
        });
    }

    /**
     * @brief Serves reporting.v1.steps.apply_engine_settings_changes.
     */
    void apply(ores::nats::message msg) {
        run(msg, [](const auto& ctx, const std::string& request_id) {
            const auto applied = service::engine_settings_proposal_service(ctx).apply(request_id);
            if (applied.applied)
                return step_outcome{.completed = true, .error = {}};
            // A transient failure leaves the request approved and the run to be
            // resumed, so the step fails without moving the request.
            if (!applied.transient)
                ores::inbox::service::approval_lifecycle(ctx).fail_apply(request_id,
                                                                         applied.refusal);
            return step_outcome{.completed = false, .error = applied.refusal};
        });
    }

private:
    struct step_outcome {
        bool completed = false;
        std::string error;
    };

    template <typename Body>
    void run(const ores::nats::message& msg, Body&& body) {
        // A durable consumer answers every delivery, so each path below states
        // the outcome; the guard retries a path that returns without deciding.
        ores::nats::ack_guard ack(msg);

        auto wf = ores::service::messaging::workflow_step_context::from_message(nats_, msg);
        if (!wf) {
            BOOST_LOG_SEV(lg(), ores::logging::warn)
                << "Ignored a command with no workflow headers: " << msg.subject;
            ack.reject();
            return;
        }
        try {
            if (const auto cached = ores::service::messaging::check_step_idempotency(
                    nats_, wf->step_id, wf->tenant_id)) {
                ores::service::messaging::publish_step_completion(nats_,
                                                                  wf->step_id,
                                                                  wf->instance_id,
                                                                  cached->outcome,
                                                                  cached->result_json,
                                                                  cached->error_message,
                                                                  cached->log);
                ack.accept();
                return;
            }

            // The engine commits a step command as JSON text, whatever wire
            // format this process speaks elsewhere. The default codec is a
            // process-wide value another client in this process can change, so
            // the format is stated here rather than inherited.
            const auto command = ores::nats::wire_codec(ores::nats::wire_format::json)
                                     .decode<ores::inbox::workflow::apply_step_command>(msg.data);
            if (!command || command->request_id.empty()) {
                wf->fail("The command names no request.");
                ack.accept();
                return;
            }

            const auto tenant =
                ores::database::service::tenant_context::with_tenant(ctx_, wf->tenant_id);
            const auto result = body(tenant, command->request_id);
            if (result.completed)
                wf->complete("{}");
            else
                wf->fail(result.error);
        } catch (const std::exception& e) {
            // The work could not be done and nothing says it never will be, so
            // the step fails and the engine's retry may run it again.
            BOOST_LOG_SEV(lg(), ores::logging::error)
                << "Step " << wf->step_id << " failed: " << e.what();
            wf->fail(e.what());
        }
        ack.accept();
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
};

}

#endif
