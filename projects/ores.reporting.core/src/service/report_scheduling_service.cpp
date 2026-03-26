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
#include "ores.reporting.core/service/report_scheduling_service.hpp"

#include <span>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/asio/use_awaitable.hpp>
#include "ores.utility/rfl/reflectors.hpp"
#include "ores.scheduler.api/rfl/reflectors.hpp"
#include "ores.scheduler.api/domain/job_definition.hpp"
#include "ores.scheduler.api/messaging/scheduler_protocol.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.reporting.core/repository/report_definition_repository.hpp"
#include "ores.reporting.core/service/report_definition_service.hpp"

namespace ores::reporting::service {

using namespace ores::logging;

namespace {

// JSON payload stored in job_definition.action_payload for report trigger jobs.
struct report_trigger_action_payload {
    std::string subject;
    std::string report_definition_id;
    std::string tenant_id;
};

boost::uuids::uuid gen_uuid() {
    static boost::uuids::random_generator rg;
    return rg();
}

} // anonymous namespace

report_scheduling_service::report_scheduling_service(
    context ctx, ores::nats::service::client& nats)
    : ctx_(std::move(ctx)), nats_(nats) {}

bool report_scheduling_service::send_schedule_request(
    const domain::report_definition& def,
    const boost::uuids::uuid& job_id,
    const std::string& performed_by) {

    // Parse the cron expression.
    auto cron = ores::scheduler::domain::cron_expression::from_string(
        def.schedule_expression);
    if (!cron) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid cron expression for definition "
            << def.id << ": " << cron.error();
        return false;
    }

    // Build the action_payload containing the trigger subject and definition id.
    const report_trigger_action_payload payload{
        .subject = std::string(
            ores::reporting::messaging::trigger_report_instance_message::nats_subject),
        .report_definition_id = boost::uuids::to_string(def.id),
        .tenant_id = def.tenant_id.to_string()
    };

    ores::scheduler::domain::job_definition job;
    job.id = job_id;
    job.tenant_id = def.tenant_id.to_uuid();
    job.party_id = def.party_id;
    job.job_name = "report_definition." + boost::uuids::to_string(def.id);
    job.description = "Scheduler job for report: " + def.name;
    job.schedule_expression = *cron;
    job.action_type = "nats_publish";
    job.action_payload = rfl::json::write(payload);
    job.is_active = true;
    job.modified_by = performed_by;
    job.performed_by = performed_by;
    job.change_reason_code = "system.report_scheduled";
    job.change_commentary = "Scheduled by reporting service";

    const ores::scheduler::messaging::schedule_job_request req{
        .definition = job,
        .change_reason_code = "system.report_scheduled",
        .change_commentary = "Scheduled by reporting service"
    };

    const auto req_json = rfl::json::write(req);
    const auto* p = reinterpret_cast<const std::byte*>(req_json.data());
    try {
        const auto reply_msg = nats_.request_sync(
            ores::scheduler::messaging::schedule_job_request::nats_subject,
            std::span<const std::byte>(p, req_json.size()));

        const std::string_view sv(
            reinterpret_cast<const char*>(reply_msg.data.data()),
            reply_msg.data.size());
        auto resp = rfl::json::read<ores::scheduler::messaging::schedule_job_response>(sv);
        if (!resp || !resp->success) {
            BOOST_LOG_SEV(lg(), error)
                << "Scheduler rejected job for definition " << def.id
                << ": " << (resp ? resp->message : "parse error");
            return false;
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to call scheduler for definition " << def.id
            << ": " << e.what();
        return false;
    }
    return true;
}

bool report_scheduling_service::schedule_one(
    const domain::report_definition& def,
    const std::string& performed_by) {

    if (def.scheduler_job_id.has_value()) {
        BOOST_LOG_SEV(lg(), debug) << "Definition already scheduled: " << def.id;
        return false;
    }

    const auto job_id = gen_uuid();
    if (!send_schedule_request(def, job_id, performed_by))
        return false;

    // Update the definition with the new scheduler_job_id.
    auto updated = def;
    updated.scheduler_job_id = job_id;
    updated.modified_by = performed_by;
    updated.performed_by = performed_by;
    updated.change_reason_code = "system.report_scheduled";
    updated.change_commentary = "Linked to scheduler job";

    const auto tenant_ctx = ctx_.with_tenant(def.tenant_id, performed_by);
    report_definition_service svc(tenant_ctx);
    svc.save_definition(updated);

    BOOST_LOG_SEV(lg(), info) << "Scheduled definition " << def.id
        << " as job " << job_id;
    return true;
}

bool report_scheduling_service::unschedule_one(
    const domain::report_definition& def,
    const std::string& performed_by) {

    if (!def.scheduler_job_id.has_value()) {
        BOOST_LOG_SEV(lg(), debug) << "Definition not scheduled: " << def.id;
        return false;
    }

    const auto job_id_str = boost::uuids::to_string(*def.scheduler_job_id);
    const ores::scheduler::messaging::unschedule_job_request req{
        .job_definition_id = job_id_str,
        .change_reason_code = "system.report_unscheduled",
        .change_commentary = "Unscheduled by reporting service"
    };

    const auto req_json = rfl::json::write(req);
    const auto* p = reinterpret_cast<const std::byte*>(req_json.data());
    try {
        const auto reply_msg = nats_.request_sync(
            ores::scheduler::messaging::unschedule_job_request::nats_subject,
            std::span<const std::byte>(p, req_json.size()));

        const std::string_view sv(
            reinterpret_cast<const char*>(reply_msg.data.data()),
            reply_msg.data.size());
        auto resp = rfl::json::read<ores::scheduler::messaging::unschedule_job_response>(sv);
        if (!resp || !resp->success) {
            BOOST_LOG_SEV(lg(), error)
                << "Scheduler failed to unschedule job " << job_id_str
                << ": " << (resp ? resp->message : "parse error");
            return false;
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to call scheduler to unschedule definition " << def.id
            << ": " << e.what();
        return false;
    }

    // Clear scheduler_job_id on the definition.
    auto updated = def;
    updated.scheduler_job_id = std::nullopt;
    updated.modified_by = performed_by;
    updated.performed_by = performed_by;
    updated.change_reason_code = "system.report_unscheduled";
    updated.change_commentary = "Scheduler job removed";

    const auto tenant_ctx = ctx_.with_tenant(def.tenant_id, performed_by);
    report_definition_service svc(tenant_ctx);
    svc.save_definition(updated);

    BOOST_LOG_SEV(lg(), info) << "Unscheduled definition " << def.id;
    return true;
}

boost::asio::awaitable<void> report_scheduling_service::reconcile() {
    BOOST_LOG_SEV(lg(), info)
        << "Starting scheduler reconciliation for report definitions.";

    repository::report_definition_repository repo;
    const auto unscheduled = repo.read_all_unscheduled(ctx_);

    BOOST_LOG_SEV(lg(), info)
        << "Found " << unscheduled.size() << " unscheduled report definition(s).";

    int scheduled = 0;
    int failed = 0;
    for (const auto& def : unscheduled) {
        try {
            if (schedule_one(def, ctx_.service_account()))
                ++scheduled;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to schedule definition " << def.id
                << ": " << e.what();
            ++failed;
        }
    }

    BOOST_LOG_SEV(lg(), info)
        << "Reconciliation complete. Scheduled: " << scheduled
        << ", failed: " << failed << ".";
    co_return;
}

}
