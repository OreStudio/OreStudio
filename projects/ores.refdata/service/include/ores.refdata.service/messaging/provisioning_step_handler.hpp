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
#ifndef ORES_REFDATA_SERVICE_MESSAGING_PROVISIONING_STEP_HANDLER_HPP
#define ORES_REFDATA_SERVICE_MESSAGING_PROVISIONING_STEP_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.iam.api/workflow/provision_tenant_workflow.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/delivery.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.core/service/calendar_materialisation_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <chrono>
#include <cstdint>
#include <exception>
#include <string>
#include <utility>

namespace ores::refdata::messaging {

/**
 * @brief Answers the provisioning steps whose work ores.refdata owns.
 *
 * A provisioning run is one workflow whose steps act on different services'
 * data. The calendars belong to refdata, so the step that materialises them is
 * dispatched here and reaches the store directly. The command is the same
 * payload every step of the run carries; the kind says which work it is, and a
 * kind this service does not own is refused rather than worked wrongly.
 *
 * The run acts for a tenant and not for a person, so the step reads and writes
 * in the tenant the command names and its writes are stamped with the service
 * account.
 */
class provisioning_step_handler {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.refdata.messaging.provisioning_step_handler");
        return instance;
    }

public:
    provisioning_step_handler(ores::nats::service::client& nats, ores::database::context ctx)
        : nats_(nats)
        , ctx_(std::move(ctx)) {}

    /// The last year the calendar rules and exceptions are materialised to.
    /// Later days would be built as weekdays with no holidays.
    static constexpr int last_generated_holiday_year = 2060;

    /// Serves refdata.v1.ops.provision_tenant_step.
    void step(ores::nats::delivery delivery) {
        const auto& msg = delivery.msg();

        auto wf = ores::service::messaging::workflow_step_context::from_message(nats_, msg);
        if (!wf) {
            BOOST_LOG_SEV(lg(), ores::logging::warn)
                << "Ignored a command with no workflow headers: " << msg.subject;
            std::move(delivery).reject();
            return;
        }
        try {
            // A step the engine already holds a terminal record of is answered
            // from that record, so a redelivery writes no second set of days.
            if (const auto cached = ores::service::messaging::check_step_idempotency(
                    nats_, wf->step_id, wf->tenant_id)) {
                ores::service::messaging::publish_step_completion(nats_,
                                                                  wf->step_id,
                                                                  wf->instance_id,
                                                                  cached->outcome,
                                                                  cached->result_json,
                                                                  cached->error_message,
                                                                  cached->log);
                std::move(delivery).commit();
                return;
            }

            const std::string_view payload(reinterpret_cast<const char*>(msg.data.data()),
                                           msg.data.size());
            auto command = ores::utility::codec::json::read_expected<
                ores::iam::workflow::provision_tenant_step_command>(payload);
            if (!command) {
                wf->fail("The provisioning step command could not be read: " +
                         std::string(command.error()));
                std::move(delivery).commit();
                return;
            }
            run(*wf, *command);
        } catch (const std::exception& e) {
            // The work could not be done and nothing says it never will be, so
            // the step fails and the engine's retry may run it again.
            BOOST_LOG_SEV(lg(), ores::logging::error)
                << "Step " << wf->step_id << " failed: " << e.what();
            wf->fail(e.what());
        }
        std::move(delivery).commit();
    }

private:
    void run(ores::service::messaging::workflow_step_context& wf,
             const ores::iam::workflow::provision_tenant_step_command& command) {
        using ores::iam::workflow::build_calendars_step_kind;
        if (command.kind != build_calendars_step_kind) {
            wf.fail("The step kind '" + command.kind +
                    "' is not one ores.refdata executes; its work belongs to another service.");
            return;
        }

        wf.progress("the holiday calendars");
        const auto tenant =
            ores::database::service::tenant_context::with_tenant(ctx_, command.tenant_id);
        service::calendar_materialisation_service calendars(tenant);
        const auto rows =
            calendars.regenerate_in_use(std::chrono::year{last_generated_holiday_year});

        BOOST_LOG_SEV(lg(), ores::logging::info)
            << "build_calendars: wrote " << rows << " day(s) for tenant " << command.tenant_id;
        // Nothing downstream reads this step's result: the kinds that consume a
        // step read its state, not its answer. The count is in the service log.
        wf.complete("{}");
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
};

}

#endif
