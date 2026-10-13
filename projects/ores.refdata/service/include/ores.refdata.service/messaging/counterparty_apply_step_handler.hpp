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
#ifndef ORES_REFDATA_SERVICE_MESSAGING_COUNTERPARTY_APPLY_STEP_HANDLER_HPP
#define ORES_REFDATA_SERVICE_MESSAGING_COUNTERPARTY_APPLY_STEP_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.inbox.api/workflow/apply_step_command.hpp"
#include "ores.inbox.core/service/approval_announcer.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.service/messaging/counterparty_proposal_mapping.hpp"
#include "ores.refdata.service/service/counterparty_proposal.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <exception>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::refdata::messaging {

/**
 * @brief Answers the commands of the counterparty apply workflow.
 *
 * The workflow runs for the tenant, not for a person, so the handler acts as the
 * service. A counterparty and its identifiers belong to the tenant, but the link
 * that lets a party trade with a counterparty belongs to a party, and a party's
 * rows are visible only to those who may see the party. So the handler scopes its
 * context to the parties of the request's own link lines, and to no others. A
 * request with no link line acts for the tenant alone.
 *
 * Every command is idempotent. A step the engine already holds a terminal record
 * of is answered from that record, and a line the apply wrote is marked applied
 * in the same transaction, so a repeated command finds nothing to do.
 */
class counterparty_apply_step_handler {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.refdata.messaging.counterparty_apply_step_handler");
        return instance;
    }

public:
    counterparty_apply_step_handler(ores::nats::service::client& nats, ores::database::context ctx)
        : nats_(nats)
        , ctx_(std::move(ctx)) {}

    /**
     * @brief Serves refdata.v1.steps.recheck_counterparty_changes.
     *
     * A line the live row now refuses will fail the apply. The request ends in
     * apply_failed here, before anything is written, with the line and the
     * reason.
     */
    void recheck(ores::nats::message msg) {
        run(msg, [this](const auto& ctx, const std::string& request_id) {
            // A run resumed after the request ended has nothing left to
            // check: the request is no longer approved, and its state stands.
            const auto request = ores::inbox::service::approval_lifecycle(ctx).request(request_id);
            if (!request || request->state_code != "approved") {
                return step_result{.completed = false,
                                   .error = "The request is " +
                                            (request ? request->state_code : std::string("gone")) +
                                            " and cannot be applied."};
            }

            const auto preview = service::counterparty_proposal_service(ctx).recheck(request_id);
            const auto result = recheck_step_result(preview);
            if (!result.completed) {
                ores::inbox::service::approval_lifecycle(ctx).fail_apply(request_id, result.error);
                tell_failed(ctx, request_id, result.error);
            }
            return result;
        });
    }

    /**
     * @brief Serves refdata.v1.steps.apply_counterparty_changes.
     */
    void apply(ores::nats::message msg) {
        run(msg, [this](const auto& ctx, const std::string& request_id) {
            const auto applied = service::counterparty_proposal_service(ctx).apply(request_id);
            const auto result = apply_step_result(applied);
            // The run stops on a failure and the tell step never runs, so a
            // refusal is told here. A transient failure leaves the request
            // approved and the run to be resumed, so nobody is told yet.
            if (!result.completed && !applied.transient)
                tell_failed(ctx, request_id, result.error);
            return result;
        });
    }

private:
    /**
     * @brief Tells the person who asked that the request ended in apply_failed.
     *
     * Telling is never the step: a failure is logged by the announcer.
     */
    void tell_failed(const ores::database::context& ctx,
                     const std::string& request_id,
                     const std::string& reason) const {
        ores::inbox::service::approval_lifecycle lifecycle(ctx);
        const auto request = lifecycle.request(request_id);
        if (!request)
            return;
        const auto kind = lifecycle.kind(request->kind_code);
        if (!kind)
            return;
        const auto sender = ctx.with_tenant(ctx.tenant_id(), ctx.service_account());
        ores::inbox::service::approval_announcer(sender).tell_asker(
            *kind, *request, request->state_code, reason);
    }

    /**
     * @brief The context a request's apply acts in: the tenant, and the parties
     * of the request's lines.
     */
    ores::database::context context_of(const std::string& tenant_id,
                                       const std::string& request_id) const {
        const auto tenant = ores::database::service::tenant_context::with_tenant(ctx_, tenant_id);
        const auto rows = ores::database::repository::execute_parameterized_multi_column_query(
            tenant,
            "select p::text from ores_refdata_counterparty_change_parties_fn($1::uuid) as p",
            {request_id},
            lg(),
            "Reading the parties of a request's lines");
        std::vector<boost::uuids::uuid> parties;
        for (const auto& row : rows) {
            if (!row.empty() && row[0])
                parties.push_back(boost::uuids::string_generator{}(*row[0]));
        }
        // A request of counterparties and identifiers alone touches no row that a
        // party owns, so the tenant is the whole scope.
        if (parties.empty())
            return tenant;
        // The actor is empty because nobody is signed in: the workflow runs for
        // the tenant, and its writes are stamped with the service account.
        return tenant.with_party(tenant.tenant_id(), parties.front(), parties, "");
    }

    template <typename Body>
    void run(const ores::nats::message& msg, Body&& body) {
        auto wf = ores::service::messaging::workflow_step_context::from_message(nats_, msg);
        if (!wf) {
            BOOST_LOG_SEV(lg(), ores::logging::warn)
                << "Ignored a command with no workflow headers: " << msg.subject;
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
                return;
            }

            const auto command =
                ores::service::messaging::decode<ores::inbox::workflow::apply_step_command>(msg);
            if (!command || command->request_id.empty()) {
                wf->fail("The command names no request.");
                return;
            }

            const auto ctx = context_of(wf->tenant_id, command->request_id);
            const auto result = body(ctx, command->request_id);
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
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
};

}

#endif
