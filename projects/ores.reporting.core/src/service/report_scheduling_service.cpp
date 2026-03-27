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
#include <unordered_set>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/asio/use_awaitable.hpp>
#include "ores.utility/rfl/reflectors.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.iam.api/domain/tenant_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.api/messaging/tenant_protocol.hpp"
#include "ores.scheduler.api/rfl/reflectors.hpp"
#include "ores.scheduler.api/domain/job_definition.hpp"
#include "ores.scheduler.api/messaging/scheduler_protocol.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
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
    boost::uuids::random_generator rg;
    return rg();
}

std::optional<boost::uuids::uuid>
find_fsm_state_id(const ores::database::context& ctx, logging::logger_t& log,
    const std::string& state_name, const std::string& fn_name) {
    using ores::database::repository::execute_parameterized_string_query;
    const auto sql = "SELECT " + fn_name + "()::text";
    const auto rows = execute_parameterized_string_query(
        ctx, sql, {}, log,
        "Looking up report_definition_lifecycle " + state_name + " state");
    if (rows.empty() || rows.front().empty()) {
        BOOST_LOG_SEV(log, warn) << "report_definition_lifecycle " << state_name
                                 << " FSM state not found";
        return std::nullopt;
    }
    return boost::lexical_cast<boost::uuids::uuid>(rows.front());
}

} // anonymous namespace

report_scheduling_service::report_scheduling_service(
    context ctx, ores::nats::service::client& nats)
    : ctx_(std::move(ctx)), nats_(nats) {}

std::optional<ores::scheduler::domain::job_definition>
report_scheduling_service::build_job_definition(
    const domain::report_definition& def,
    const boost::uuids::uuid& job_id,
    const std::string& performed_by) {

    auto cron = ores::scheduler::domain::cron_expression::from_string(
        def.schedule_expression);
    if (!cron) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid cron expression for definition "
            << def.id << ": " << cron.error();
        return std::nullopt;
    }

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
    return job;
}

bool report_scheduling_service::send_schedule_request(
    const domain::report_definition& def,
    const boost::uuids::uuid& job_id,
    const std::string& performed_by) {

    auto job = build_job_definition(def, job_id, performed_by);
    if (!job)
        return false;

    const ores::scheduler::messaging::schedule_job_request req{
        .definition = *job,
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

    // Resolve the "active" FSM state UUID once from the system context.
    const auto active_state = find_fsm_state_id(
        ctx_, lg(), "active", "ores_reporting_active_definition_state_fn");

    // Update the definition with the new scheduler_job_id and active state.
    auto updated = def;
    updated.scheduler_job_id = job_id;
    updated.fsm_state_id = active_state;
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

    // Resolve the "suspended" FSM state UUID from the system context.
    const auto suspended_state = find_fsm_state_id(
        ctx_, lg(), "suspended", "ores_reporting_suspended_definition_state_fn");

    // Clear scheduler_job_id and transition to suspended state.
    auto updated = def;
    updated.scheduler_job_id = std::nullopt;
    updated.fsm_state_id = suspended_state;
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

    const auto& performed_by = ctx_.service_account();

    // Step 1: ask the IAM service for all active tenants.
    BOOST_LOG_SEV(lg(), debug) << "Requesting active tenant list from IAM.";
    std::vector<ores::iam::domain::tenant> tenants;
    try {
        const ores::iam::messaging::get_tenants_request tenant_req{
            .include_deleted = false
        };
        const auto req_json = rfl::json::write(tenant_req);
        const auto* p = reinterpret_cast<const std::byte*>(req_json.data());
        const auto reply_msg = nats_.request_sync(
            ores::iam::messaging::get_tenants_request::nats_subject,
            std::span<const std::byte>(p, req_json.size()));

        const std::string_view sv(
            reinterpret_cast<const char*>(reply_msg.data.data()),
            reply_msg.data.size());
        auto resp = rfl::json::read<ores::iam::messaging::get_tenants_response>(sv);
        if (!resp) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to parse tenant list response; aborting reconciliation.";
            co_return;
        }
        tenants = std::move(resp->tenants);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to retrieve tenant list from IAM: " << e.what();
        co_return;
    }

    BOOST_LOG_SEV(lg(), debug)
        << "Received " << tenants.size() << " active tenant(s) from IAM.";

    if (tenants.empty()) {
        BOOST_LOG_SEV(lg(), info) << "Reconciliation complete. No active tenants.";
        co_return;
    }

    // Step 2: for each tenant, query unscheduled definitions and schedule them.
    int total_scheduled = 0;
    int total_failed = 0;

    for (const auto& tenant : tenants) {
        const auto tenant_id_result = utility::uuid::tenant_id::from_uuid(tenant.id);
        if (!tenant_id_result) {
            BOOST_LOG_SEV(lg(), error)
                << "Skipping tenant '" << tenant.name
                << "': invalid UUID: " << tenant_id_result.error();
            ++total_failed;
            continue;
        }
        const auto& tenant_id = *tenant_id_result;
        const auto tenant_id_str = tenant_id.to_string();
        BOOST_LOG_SEV(lg(), debug)
            << "Reconciling tenant: " << tenant_id_str
            << " [" << tenant.name << "]";

        const auto tenant_ctx = ctx_.with_tenant(tenant_id, performed_by);

        repository::report_definition_repository repo;
        std::vector<domain::report_definition> unscheduled;
        try {
            unscheduled = repo.read_latest_unscheduled(tenant_ctx);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to read unscheduled definitions for tenant "
                << tenant_id_str << ": " << e.what();
            continue;
        }

        BOOST_LOG_SEV(lg(), debug) << "Found " << unscheduled.size()
            << " unscheduled definition(s) for tenant: " << tenant_id_str;

        if (unscheduled.empty())
            continue;

        // Build a batch request for this tenant's unscheduled definitions.
        struct pending_entry {
            boost::uuids::uuid job_id;
            const domain::report_definition* def;
        };
        std::vector<pending_entry> pending;
        ores::scheduler::messaging::schedule_jobs_batch_request batch_req;
        batch_req.change_reason_code = "system.report_scheduled";
        batch_req.change_commentary = "Startup reconciliation by reporting service";

        for (const auto& def : unscheduled) {
            const auto job_id = gen_uuid();
            auto job = build_job_definition(def, job_id, performed_by);
            if (!job) {
                // Invalid cron — warning already logged in build_job_definition.
                ++total_failed;
                continue;
            }
            batch_req.definitions.push_back(std::move(*job));
            pending.push_back({job_id, &def});
        }

        if (pending.empty()) {
            BOOST_LOG_SEV(lg(), debug)
                << "No schedulable definitions for tenant: " << tenant_id_str;
            continue;
        }

        // Send the batch request to the scheduler.
        BOOST_LOG_SEV(lg(), debug) << "Sending batch of " << pending.size()
            << " job(s) to scheduler for tenant: " << tenant_id_str;
        const auto req_json = rfl::json::write(batch_req);
        const auto* p = reinterpret_cast<const std::byte*>(req_json.data());

        std::unordered_set<std::string> failed_ids;
        try {
            const auto reply_msg = nats_.request_sync(
                ores::scheduler::messaging::schedule_jobs_batch_request::nats_subject,
                std::span<const std::byte>(p, req_json.size()));

            const std::string_view sv(
                reinterpret_cast<const char*>(reply_msg.data.data()),
                reply_msg.data.size());
            auto resp = rfl::json::read<
                ores::scheduler::messaging::schedule_jobs_batch_response>(sv);
            if (!resp) {
                BOOST_LOG_SEV(lg(), error)
                    << "Failed to parse batch schedule response for tenant "
                    << tenant_id_str << "; skipping.";
                total_failed += static_cast<int>(pending.size());
                continue;
            }
            for (const auto& fid : resp->failed_ids)
                failed_ids.insert(fid);

            BOOST_LOG_SEV(lg(), debug)
                << "Scheduler accepted " << resp->scheduled_count
                << " job(s), " << resp->failed_ids.size()
                << " failed for tenant: " << tenant_id_str;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Batch schedule NATS call failed for tenant "
                << tenant_id_str << ": " << e.what();
            total_failed += static_cast<int>(pending.size());
            continue;
        }

        // Resolve "active" state once per tenant batch (avoids repeated DB calls).
        const auto active_state = find_fsm_state_id(
            ctx_, lg(), "active", "ores_reporting_active_definition_state_fn");

        // Persist the scheduler_job_id on each accepted definition.
        for (const auto& entry : pending) {
            const auto job_id_str = boost::uuids::to_string(entry.job_id);
            if (failed_ids.count(job_id_str)) {
                ++total_failed;
                continue;
            }

            auto def_updated = *entry.def;
            def_updated.scheduler_job_id = entry.job_id;
            def_updated.fsm_state_id = active_state;
            def_updated.modified_by = performed_by;
            def_updated.performed_by = performed_by;
            def_updated.change_reason_code = "system.report_scheduled";
            def_updated.change_commentary = "Linked to scheduler job by reconciliation";

            try {
                report_definition_service svc(tenant_ctx);
                svc.save_definition(def_updated);
                ++total_scheduled;
                BOOST_LOG_SEV(lg(), debug)
                    << "Persisted scheduler_job_id " << job_id_str
                    << " for definition: " << entry.def->id;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error)
                    << "Failed to persist scheduler_job_id for definition "
                    << entry.def->id << ": " << e.what();
                ++total_failed;
            }
        }
    }

    BOOST_LOG_SEV(lg(), info)
        << "Reconciliation complete. Tenants processed: " << tenants.size()
        << ", scheduled: " << total_scheduled
        << ", failed: " << total_failed << ".";
    co_return;
}

}
