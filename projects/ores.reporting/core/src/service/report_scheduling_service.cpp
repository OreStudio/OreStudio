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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam.api/domain/tenant_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.api/messaging/tenant_protocol.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.reporting.api/messaging/report_scheduling_protocol.hpp"
#include "ores.reporting.core/repository/report_definition_repository.hpp"
#include "ores.reporting.core/service/report_definition_service.hpp"
#include "ores.scheduler.api/messaging/job_definition_protocol.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/asio/use_awaitable.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>

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

std::optional<boost::uuids::uuid> find_fsm_state_id(const ores::database::context& ctx,
                                                    logging::logger_t& log,
                                                    const std::string& state_name,
                                                    const std::string& fn_name) {
    using ores::database::repository::execute_parameterized_string_query;
    const auto sql = "SELECT " + fn_name + "()::text";
    const auto rows = execute_parameterized_string_query(
        ctx, sql, {}, log, "Looking up report_definition_lifecycle " + state_name + " state");
    if (rows.empty() || rows.front().empty()) {
        BOOST_LOG_SEV(log, warn) << "report_definition_lifecycle " << state_name
                                 << " FSM state not found";
        return std::nullopt;
    }
    return boost::lexical_cast<boost::uuids::uuid>(rows.front());
}

} // anonymous namespace

report_scheduling_service::report_scheduling_service(context ctx,
                                                     ores::nats::service::nats_client svc_nats)
    : ctx_(std::move(ctx))
    , svc_nats_(std::move(svc_nats)) {}

std::optional<ores::scheduler::messaging::job_definition_change>
report_scheduling_service::build_job_change(const domain::report_definition& def,
                                            const boost::uuids::uuid& job_id) {

    auto cron = ores::scheduler::domain::cron_expression::from_string(def.schedule_expression);
    if (!cron) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid cron expression for definition " << def.id << ": "
                                  << cron.error();
        return std::nullopt;
    }

    const report_trigger_action_payload payload{
        .subject =
            std::string(ores::reporting::messaging::trigger_report_instance_message::nats_subject),
        .report_definition_id = boost::uuids::to_string(def.id),
        .tenant_id = def.tenant_id.to_string()};

    ores::scheduler::messaging::job_definition_change change;
    change.write.id = job_id;
    change.write.job_name = "report_definition." + boost::uuids::to_string(def.id);
    change.write.description = "Scheduler job for report: " + def.name;
    change.write.command = "";
    change.write.schedule_expression = *cron;
    change.write.action_type = "nats_publish";
    change.write.action_payload = rfl::json::write(payload);
    change.write.is_active = true;
    // The reporting service owns the job's identity and replaces whatever
    // holds that identity, because reconciliation re-runs on every restart.
    change.precondition.kind = ores::utility::domain::precondition_kind::any;
    return change;
}

std::expected<void, std::string>
report_scheduling_service::send_schedule_request(const domain::report_definition& def,
                                                 const boost::uuids::uuid& job_id) {

    auto change = build_job_change(def, job_id);
    if (!change)
        return std::unexpected("Invalid cron expression for definition " +
                               boost::uuids::to_string(def.id));

    const ores::scheduler::messaging::put_job_definition_request req{
        .change = *change,
        .intent = {.reason_code = std::string(ores::service::messaging::change_reasons::new_record),
                   .commentary = "Scheduled by reporting service"}};

    const auto& codec = ores::nats::default_wire_codec();
    try {
        const auto reply_msg = svc_nats_.authenticated_request(
            ores::scheduler::messaging::put_job_definition_request::nats_subject,
            codec.encode(req));

        auto resp =
            codec.decode<ores::scheduler::messaging::put_job_definition_response>(reply_msg.data);
        if (!resp) {
            const std::string err = "Scheduler returned unparseable response for definition " +
                                    boost::uuids::to_string(def.id);
            BOOST_LOG_SEV(lg(), error) << err;
            return std::unexpected(err);
        }
        if (resp->result.outcome != ores::utility::domain::outcome::ok) {
            const std::string err = "Scheduler rejected job for definition " +
                                    boost::uuids::to_string(def.id) + ": " + resp->result.message;
            BOOST_LOG_SEV(lg(), error) << err;
            return std::unexpected(err);
        }
    } catch (const std::exception& e) {
        const std::string err = std::string("Scheduler NATS call failed: ") + e.what();
        BOOST_LOG_SEV(lg(), error)
            << "Failed to call scheduler for definition " << def.id << ": " << e.what();
        return std::unexpected(err);
    }
    return {};
}

std::expected<bool, std::string>
report_scheduling_service::schedule_one(const domain::report_definition& def,
                                        const std::string& actor) {

    if (def.scheduler_job_id.has_value()) {
        BOOST_LOG_SEV(lg(), debug) << "Definition already scheduled: " << def.id;
        return false;
    }

    const auto job_id = gen_uuid();
    auto send_result = send_schedule_request(def, job_id);
    if (!send_result)
        return std::unexpected(send_result.error());

    // Resolve the "active" FSM state UUID once from the system context.
    const auto active_state =
        find_fsm_state_id(ctx_, lg(), "active", "ores_reporting_active_definition_state_fn");

    // Update the definition with the new scheduler_job_id and active state.
    auto updated = def;
    updated.scheduler_job_id = job_id;
    updated.fsm_state_id = active_state;
    updated.modified_by = actor;
    updated.performed_by = ctx_.service_account();
    updated.change_reason_code = std::string(ores::service::messaging::change_reasons::new_record);
    updated.change_commentary = "Linked to scheduler job";

    const auto tenant_ctx = ctx_.with_tenant(def.tenant_id, actor);
    report_definition_service svc(tenant_ctx);
    svc.save_definition(updated);

    BOOST_LOG_SEV(lg(), info) << "Scheduled definition " << def.id << " as job " << job_id;
    return true;
}

std::expected<bool, std::string>
report_scheduling_service::unschedule_one(const domain::report_definition& def,
                                          const std::string& actor) {

    if (!def.scheduler_job_id.has_value()) {
        BOOST_LOG_SEV(lg(), debug) << "Definition not scheduled: " << def.id;
        return false;
    }

    const auto job_id_str = boost::uuids::to_string(*def.scheduler_job_id);
    const ores::scheduler::messaging::delete_job_definition_request req{
        .removal = {.key = {.id = *def.scheduler_job_id}},
        .intent = {.reason_code = std::string(ores::service::messaging::change_reasons::new_record),
                   .commentary = "Unscheduled by reporting service"}};

    const auto& codec = ores::nats::default_wire_codec();
    try {
        const auto reply_msg = svc_nats_.authenticated_request(
            ores::scheduler::messaging::delete_job_definition_request::nats_subject,
            codec.encode(req));

        auto resp = codec.decode<ores::scheduler::messaging::delete_job_definition_response>(
            reply_msg.data);
        if (!resp) {
            const std::string err = "Scheduler returned unparseable response for job " + job_id_str;
            BOOST_LOG_SEV(lg(), error) << err;
            return std::unexpected(err);
        }
        if (resp->result.outcome != ores::utility::domain::outcome::ok) {
            const std::string err =
                "Scheduler failed to unschedule job " + job_id_str + ": " + resp->result.message;
            BOOST_LOG_SEV(lg(), error) << err;
            return std::unexpected(err);
        }
    } catch (const std::exception& e) {
        const std::string err = std::string("Scheduler NATS call failed: ") + e.what();
        BOOST_LOG_SEV(lg(), error)
            << "Failed to call scheduler to unschedule definition " << def.id << ": " << e.what();
        return std::unexpected(err);
    }

    // Resolve the "suspended" FSM state UUID from the system context.
    const auto suspended_state =
        find_fsm_state_id(ctx_, lg(), "suspended", "ores_reporting_suspended_definition_state_fn");

    // Clear scheduler_job_id and transition to suspended state.
    auto updated = def;
    updated.scheduler_job_id = std::nullopt;
    updated.fsm_state_id = suspended_state;
    updated.modified_by = actor;
    updated.performed_by = ctx_.service_account();
    updated.change_reason_code = std::string(ores::service::messaging::change_reasons::new_record);
    updated.change_commentary = "Scheduler job removed";

    const auto tenant_ctx = ctx_.with_tenant(def.tenant_id, actor);
    report_definition_service svc(tenant_ctx);
    svc.save_definition(updated);

    BOOST_LOG_SEV(lg(), info) << "Unscheduled definition " << def.id;
    return true;
}

boost::asio::awaitable<void> report_scheduling_service::reconcile() {
    BOOST_LOG_SEV(lg(), info) << "Starting scheduler reconciliation for report definitions.";


    // Step 1: ask the IAM service for all active tenants, paging until the
    // server-reported total is exhausted.
    BOOST_LOG_SEV(lg(), debug) << "Requesting active tenant list from IAM.";
    std::vector<ores::iam::domain::tenant> tenants;
    const auto& codec = ores::nats::default_wire_codec();
    constexpr std::uint32_t page_size = 100;
    try {
        std::uint32_t offset = 0;
        std::uint64_t total_available = 0;
        while (true) {
            ores::iam::messaging::list_tenants_request tenant_req;
            tenant_req.offset = offset;
            tenant_req.limit = page_size;

            const auto reply_msg = svc_nats_.authenticated_request(
                ores::iam::messaging::list_tenants_request::nats_subject, codec.encode(tenant_req));

            auto resp = codec.decode<ores::iam::messaging::list_tenants_response>(reply_msg.data);
            if (!resp) {
                BOOST_LOG_SEV(lg(), error)
                    << "Failed to parse tenant list response; aborting reconciliation.";
                co_return;
            }
            if (resp->result.outcome != ores::utility::domain::outcome::ok) {
                BOOST_LOG_SEV(lg(), error) << "IAM failed to list tenants: " << resp->result.message
                                           << "; aborting reconciliation.";
                co_return;
            }

            total_available = resp->total;
            auto& page = resp->tenants;
            for (auto& t : page)
                tenants.push_back(std::move(t));

            offset += static_cast<std::uint32_t>(page.size());
            const auto received_all = page.empty() || offset >= total_available;
            if (received_all)
                break;
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to retrieve tenant list from IAM: " << e.what();
        co_return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Received " << tenants.size() << " active tenant(s) from IAM.";

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
            BOOST_LOG_SEV(lg(), error) << "Skipping tenant '" << tenant.name
                                       << "': invalid UUID: " << tenant_id_result.error();
            ++total_failed;
            continue;
        }
        const auto& tenant_id = *tenant_id_result;
        const auto tenant_id_str = tenant_id.to_string();
        BOOST_LOG_SEV(lg(), debug)
            << "Reconciling tenant: " << tenant_id_str << " [" << tenant.name << "]";

        const auto tenant_ctx = ctx_.with_tenant(tenant_id, ctx_.service_account());

        repository::report_definition_repository repo;
        std::vector<domain::report_definition> unscheduled;
        try {
            auto all = repo.read_latest(tenant_ctx);
            for (auto& d : all)
                if (!d.scheduler_job_id)
                    unscheduled.push_back(std::move(d));
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to read unscheduled definitions for tenant "
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
        ores::scheduler::messaging::put_many_job_definitions_request batch_req;
        batch_req.intent = {.reason_code =
                                std::string(ores::service::messaging::change_reasons::new_record),
                            .commentary = "Startup reconciliation by reporting service"};

        for (const auto& def : unscheduled) {
            const auto job_id = gen_uuid();
            auto change = build_job_change(def, job_id);
            if (!change) {
                // Invalid cron — warning already logged in build_job_change.
                ++total_failed;
                continue;
            }
            batch_req.changes.push_back(std::move(*change));
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
        const auto& codec = ores::nats::default_wire_codec();

        try {
            const auto reply_msg = svc_nats_.authenticated_request(
                ores::scheduler::messaging::put_many_job_definitions_request::nats_subject,
                codec.encode(batch_req));

            auto resp = codec.decode<ores::scheduler::messaging::put_many_job_definitions_response>(
                reply_msg.data);
            if (!resp) {
                BOOST_LOG_SEV(lg(), error) << "Failed to parse batch schedule response for tenant "
                                           << tenant_id_str << "; skipping.";
                total_failed += static_cast<int>(pending.size());
                continue;
            }
            if (resp->result.outcome != ores::utility::domain::outcome::ok) {
                BOOST_LOG_SEV(lg(), error)
                    << "Scheduler rejected the batch for tenant " << tenant_id_str << ": "
                    << resp->result.message << "; skipping all " << pending.size() << " job(s).";
                total_failed += static_cast<int>(pending.size());
                continue;
            }
            BOOST_LOG_SEV(lg(), debug) << "Scheduler accepted " << pending.size()
                                       << " job(s) for tenant: " << tenant_id_str;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Batch schedule NATS call failed for tenant "
                                       << tenant_id_str << ": " << e.what();
            total_failed += static_cast<int>(pending.size());
            continue;
        }

        // Resolve "active" state once per tenant batch (avoids repeated DB calls).
        const auto active_state =
            find_fsm_state_id(ctx_, lg(), "active", "ores_reporting_active_definition_state_fn");

        // Persist the scheduler_job_id on each definition the scheduler kept.
        for (const auto& entry : pending) {
            const auto job_id_str = boost::uuids::to_string(entry.job_id);

            auto def_updated = *entry.def;
            def_updated.scheduler_job_id = entry.job_id;
            def_updated.fsm_state_id = active_state;
            def_updated.modified_by = ctx_.service_account();
            def_updated.performed_by = ctx_.service_account();
            def_updated.change_reason_code =
                std::string(ores::service::messaging::change_reasons::new_record);
            def_updated.change_commentary = "Linked to scheduler job by reconciliation";

            try {
                report_definition_service svc(tenant_ctx);
                svc.save_definition(def_updated);
                ++total_scheduled;
                BOOST_LOG_SEV(lg(), debug) << "Persisted scheduler_job_id " << job_id_str
                                           << " for definition: " << entry.def->id;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error) << "Failed to persist scheduler_job_id for definition "
                                           << entry.def->id << ": " << e.what();
                ++total_failed;
            }
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Reconciliation complete. Tenants processed: " << tenants.size()
                              << ", scheduled: " << total_scheduled << ", failed: " << total_failed
                              << ".";
    co_return;
}

}
