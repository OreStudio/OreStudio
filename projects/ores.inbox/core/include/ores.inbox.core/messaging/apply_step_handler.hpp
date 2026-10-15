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
#ifndef ORES_INBOX_CORE_MESSAGING_APPLY_STEP_HANDLER_HPP
#define ORES_INBOX_CORE_MESSAGING_APPLY_STEP_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.inbox.api/workflow/apply_step_command.hpp"
#include "ores.inbox.core/service/approval_announcer.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include <exception>
#include <string>
#include <utility>

namespace ores::inbox::messaging {

/**
 * @brief The words a notice uses for how a request's apply ended.
 *
 * An approved request whose apply finished is applied. A request in any other
 * state keeps its own name, so the notice says apply_failed when it failed.
 */
inline std::string apply_result_state(const std::string& request_state) {
    return request_state == "approved" ? "applied" : request_state;
}

/**
 * @brief Answers the last step of an apply workflow: telling the person who
 * asked how it ended.
 *
 * The inbox serves it for every kind, because telling a person is the inbox's
 * work. It reads the request's own state, so it needs nothing from the owning
 * component, and telling is never the operation: a failure to tell is logged
 * and the step still completes.
 */
class apply_step_handler {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.inbox.messaging.apply_step_handler");
        return instance;
    }

public:
    apply_step_handler(ores::nats::service::client& nats, ores::database::context ctx)
        : nats_(nats)
        , ctx_(std::move(ctx)) {}

    /**
     * @brief Serves inbox.v1.steps.tell_apply_result.
     */
    void tell(ores::nats::message msg) {
        // A durable consumer answers every delivery, so each path below states
        // the outcome; the guard retries a path that returns without deciding.
        ores::nats::ack_guard ack(msg);

        auto wf = ores::service::messaging::workflow_step_context::from_message(nats_, msg);
        if (!wf) {
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

            const auto command =
                ores::service::messaging::decode<ores::inbox::workflow::apply_step_command>(msg);
            if (!command || command->request_id.empty()) {
                wf->fail("The command names no request.");
                ack.accept();
                return;
            }

            // The notice is sent by the service, which is an account of the
            // tenant, so the context acts as the service account.
            const auto tenant =
                ores::database::service::tenant_context::with_tenant(ctx_, wf->tenant_id);
            const auto ctx = tenant.with_tenant(tenant.tenant_id(), tenant.service_account());
            service::approval_lifecycle lifecycle(ctx);
            const auto request = lifecycle.request(command->request_id);
            if (!request) {
                wf->fail("No such request.");
                ack.accept();
                return;
            }
            const auto kind = lifecycle.kind(request->kind_code);
            if (kind) {
                // The announcer logs a failure to tell and never throws.
                service::approval_announcer(ctx).tell_asker(*kind,
                                                            *request,
                                                            apply_result_state(request->state_code),
                                                            request->change_commentary);
            } else {
                BOOST_LOG_SEV(lg(), ores::logging::warn)
                    << "Request " << command->request_id << " has the unknown kind "
                    << request->kind_code << ", so the person who asked was not told.";
            }
            wf->complete("{}");
            ack.accept();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), ores::logging::error)
                << "Step " << wf->step_id << " failed: " << e.what();
            wf->fail(e.what());
            ack.accept();
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
};

}

#endif
