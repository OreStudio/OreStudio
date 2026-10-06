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
#include "ores.iam.api/messaging/run_grant_operations_protocol.hpp"
#include "ores.iam.api/messaging/tenant_protocol.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.core/repository/report_definition_repository.hpp"
#include "ores.reporting.core/service/report_definition_service.hpp"
#include "ores.reporting.core/service/scheduling_plan.hpp"
#include "ores.scheduler.api/messaging/job_definition_protocol.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/asio/use_awaitable.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <expected>
#include <map>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <set>

namespace ores::reporting::service {

using namespace ores::logging;

namespace {

// JSON payload stored in job_definition.action_payload for report trigger jobs.
struct report_trigger_action_payload {
    std::string subject;
    std::string report_definition_id;
    std::string tenant_id;
};

// The role a report run acts with, and the services that exchange its grant,
// named by the service role each one's account holds: reporting for collection
// and finalisation, ORE for the input, compute for submission. IAM's exchange
// reads the caller's service name from that role, so the audience carries role
// names and not host names.
constexpr std::string_view run_role = "ReportRun";
constexpr std::string_view run_audience = "ReportingService,OreService,ComputeService";

template <typename Request>
std::expected<typename Request::response_type, std::string>
call(ores::nats::service::nats_client& nats, const Request& request) {
    const auto& codec = ores::nats::default_wire_codec();
    const auto msg = nats.authenticated_request(Request::nats_subject, codec.encode(request));
    if (const auto it = msg.headers.find(std::string(ores::nats::headers::x_error));
        it != msg.headers.end())
        return std::unexpected(std::string(Request::nats_subject) + " refused: " + it->second);
    auto response = codec.decode<typename Request::response_type>(msg.data);
    if (!response)
        return std::unexpected(std::string(Request::nats_subject) + " answered unreadably");
    if (!response->success)
        return std::unexpected(response->message);
    return *response;
}

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

report_scheduling_service::report_scheduling_service(
    context ctx,
    ores::nats::service::nats_client svc_nats,
    std::optional<ores::nats::service::nats_client> person_nats)
    : ctx_(std::move(ctx))
    , svc_nats_(std::move(svc_nats))
    , person_nats_(std::move(person_nats)) {}

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
            std::string(ores::reporting::messaging::trigger_report_instance_request::nats_subject),
        .report_definition_id = boost::uuids::to_string(def.id),
        .tenant_id = def.tenant_id.to_string()};

    ores::scheduler::messaging::job_definition_change change;
    change.write.id = job_id;
    change.write.job_name = scheduler_job_name(def.id);
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

std::expected<void, std::string>
report_scheduling_service::send_delete_request(const boost::uuids::uuid& job_id,
                                               const std::string& commentary) {
    const auto job_id_str = boost::uuids::to_string(job_id);
    const ores::scheduler::messaging::delete_job_definition_request req{
        .removal = {.key = {.id = job_id}},
        .intent = {.reason_code = std::string(ores::service::messaging::change_reasons::new_record),
                   .commentary = commentary}};

    const auto& codec = ores::nats::default_wire_codec();
    try {
        const auto reply_msg = svc_nats_.authenticated_request(
            ores::scheduler::messaging::delete_job_definition_request::nats_subject,
            codec.encode(req));
        if (const auto it = reply_msg.headers.find(std::string(ores::nats::headers::x_error));
            it != reply_msg.headers.end())
            return std::unexpected("Scheduler refused to delete job " + job_id_str + ": " +
                                   it->second);
        auto resp = codec.decode<ores::scheduler::messaging::delete_job_definition_response>(
            reply_msg.data);
        if (!resp)
            return std::unexpected("Scheduler returned unparseable response for job " +
                                   job_id_str);
        // A job that is already gone is the state the caller asked for, so a
        // second delete of the same job converges instead of failing.
        if (resp->result.outcome == ores::utility::domain::outcome::missing)
            return {};
        if (resp->result.outcome != ores::utility::domain::outcome::ok)
            return std::unexpected("Scheduler failed to delete job " + job_id_str + ": " +
                                   resp->result.message);
    } catch (const std::exception& e) {
        return std::unexpected(std::string("Scheduler NATS call failed: ") + e.what());
    }
    return {};
}

std::expected<boost::uuids::uuid, std::string>
report_scheduling_service::grant_runs(const domain::report_definition& def) {
    if (!person_nats_)
        return std::unexpected("Scheduling needs a person to consent to the runs.");
    ores::iam::messaging::create_run_grant_request req;
    req.resource = "reporting.report_definition/" + boost::uuids::to_string(def.id);
    req.role = std::string(run_role);
    req.audience = std::string(run_audience);
    try {
        const auto granted = call(*person_nats_, req);
        if (!granted)
            return std::unexpected("IAM refused the run grant for definition " +
                                   boost::uuids::to_string(def.id) + ": " + granted.error());
        return boost::uuids::string_generator()(granted->grant_id);
    } catch (const std::exception& e) {
        return std::unexpected(std::string("IAM run grant call failed: ") + e.what());
    }
}

std::expected<void, std::string>
report_scheduling_service::revoke_runs(const boost::uuids::uuid& grant_id) {
    if (!person_nats_)
        return std::unexpected("Revoking a run grant needs the person who acts.");
    ores::iam::messaging::revoke_run_grant_request req;
    req.grant_id = boost::uuids::to_string(grant_id);
    req.reason = "unscheduled";
    try {
        if (const auto revoked = call(*person_nats_, req); !revoked)
            return std::unexpected(revoked.error());
        return {};
    } catch (const std::exception& e) {
        return std::unexpected(std::string("IAM run grant call failed: ") + e.what());
    }
}

std::expected<bool, std::string>
report_scheduling_service::schedule_one(const domain::report_definition& def,
                                        const std::string& actor) {

    if (def.scheduler_job_id.has_value()) {
        BOOST_LOG_SEV(lg(), debug) << "Definition already scheduled: " << def.id;
        return false;
    }

    if (!ores::scheduler::domain::cron_expression::from_string(def.schedule_expression))
        return std::unexpected("Invalid cron expression for definition " +
                               boost::uuids::to_string(def.id));

    const auto active_state =
        find_fsm_state_id(ctx_, lg(), "active", "ores_reporting_active_definition_state_fn");
    if (!active_state)
        return std::unexpected("The active report definition state is not seeded.");

    // The grant comes first: a job with no consent behind it would fire runs
    // that cannot act in the definition's party.
    const auto grant_id = grant_runs(def);
    if (!grant_id)
        return std::unexpected(grant_id.error());

    const auto withdraw_grant = [&] {
        if (const auto revoked = revoke_runs(*grant_id); !revoked)
            BOOST_LOG_SEV(lg(), warn) << "Run grant " << *grant_id << " of definition " << def.id
                                      << " stays active after a failed schedule: "
                                      << revoked.error();
    };

    const auto job_id = gen_uuid();
    auto send_result = send_schedule_request(def, job_id);
    if (!send_result) {
        withdraw_grant();
        return std::unexpected(send_result.error());
    }

    auto updated = def;
    updated.scheduler_job_id = job_id;
    updated.run_grant_id = *grant_id;
    updated.fsm_state_id = *active_state;
    updated.modified_by = actor;
    updated.performed_by = ctx_.service_account();
    updated.change_reason_code = std::string(ores::service::messaging::change_reasons::update);
    updated.change_commentary = "Linked to scheduler job";

    // A job the definition does not record would fire with no definition
    // behind it, so a failed save takes the job and the grant back.
    try {
        const auto tenant_ctx = ctx_.with_tenant(def.tenant_id, actor);
        report_definition_service svc(tenant_ctx);
        svc.save_definition(updated);
    } catch (const std::exception& e) {
        if (const auto deleted = send_delete_request(job_id, "Schedule not recorded"); !deleted)
            BOOST_LOG_SEV(lg(), warn) << deleted.error();
        withdraw_grant();
        return std::unexpected(std::string("Could not record the schedule: ") + e.what());
    }

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

    if (const auto deleted =
            send_delete_request(*def.scheduler_job_id, "Unscheduled by reporting service");
        !deleted) {
        BOOST_LOG_SEV(lg(), error) << deleted.error();
        return std::unexpected(deleted.error());
    }

    // Resolve the "suspended" FSM state UUID from the system context.
    const auto suspended_state =
        find_fsm_state_id(ctx_, lg(), "suspended", "ores_reporting_suspended_definition_state_fn");
    if (!suspended_state) {
        return std::unexpected("The suspended report definition state is not seeded: the "
                               "scheduler job was removed but the definition cannot record it.");
    }

    // A grant that cannot be revoked here, because the person unscheduling is
    // neither its grantor nor an administrator, serves no run once the job is
    // gone; the definition keeps its id so the grant can still be found.
    auto grant_to_keep = def.run_grant_id;
    if (def.run_grant_id) {
        if (const auto revoked = revoke_runs(*def.run_grant_id); revoked)
            grant_to_keep = std::nullopt;
        else
            BOOST_LOG_SEV(lg(), warn) << "Run grant " << *def.run_grant_id << " of definition "
                                      << def.id << " stays active: " << revoked.error();
    }

    // Clear scheduler_job_id and transition to suspended state.
    auto updated = def;
    updated.scheduler_job_id = std::nullopt;
    updated.run_grant_id = grant_to_keep;
    updated.fsm_state_id = *suspended_state;
    updated.modified_by = actor;
    updated.performed_by = ctx_.service_account();
    updated.change_reason_code = std::string(ores::service::messaging::change_reasons::update);
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

    // Step 2: read every definition and split the schedules that rest on a
    // person's consent from the records left by the service-account scheduling
    // this service used to do. A record with a job but no grant is cleared
    // here, so the definition can be scheduled again by a person.
    std::vector<domain::report_definition> consented;
    std::set<boost::uuids::uuid> accounted_job_ids;
    int total_cleared = 0;
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
        std::vector<domain::report_definition> all;
        try {
            all = repo.read_latest(tenant_ctx);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to read definitions for tenant "
                                       << tenant_id_str << ": " << e.what();
            ++total_failed;
            continue;
        }

        for (auto& def : all) {
            if (!def.scheduler_job_id)
                continue;
            if (def.run_grant_id) {
                accounted_job_ids.insert(*def.scheduler_job_id);
                consented.push_back(std::move(def));
                continue;
            }

            // No grant behind the job, so the runs cannot act in the party.
            // Clearing the record is what lets the person schedule again.
            auto updated = def;
            updated.scheduler_job_id = std::nullopt;
            updated.modified_by = ctx_.service_account();
            updated.performed_by = ctx_.service_account();
            updated.change_reason_code =
                std::string(ores::service::messaging::change_reasons::update);
            updated.change_commentary = "Cleared a scheduler job with no run grant";
            try {
                report_definition_service svc(tenant_ctx);
                svc.save_definition(updated);
                ++total_cleared;
                BOOST_LOG_SEV(lg(), info) << "Cleared the ungranted schedule of definition "
                                          << def.id;
            } catch (const std::exception& e) {
                ++total_failed;
                BOOST_LOG_SEV(lg(), error) << "Could not clear the schedule of definition "
                                           << def.id << ": " << e.what();
            }
        }
    }

    // Step 3: the report jobs the scheduler holds, by name. Reading is all or
    // nothing: if the list does not parse, the delete and restore steps below
    // are skipped rather than acting on a job list this pass cannot see.
    std::map<std::string, boost::uuids::uuid> report_jobs;
    bool scheduler_read_ok = false;
    try {
        ores::scheduler::messaging::list_job_definitions_request list_req;
        list_req.limit = 1000;
        const auto reply = svc_nats_.authenticated_request(
            ores::scheduler::messaging::list_job_definitions_request::nats_subject,
            ores::nats::default_wire_codec().encode(list_req));
        if (auto parsed =
                ores::nats::default_wire_codec()
                    .decode<ores::scheduler::messaging::list_job_definitions_response>(
                        reply.data)) {
            scheduler_read_ok = true;
            for (const auto& existing : parsed->definitions)
                if (is_report_definition_job_name(existing.job_name))
                    report_jobs.emplace(existing.job_name, existing.id);
            BOOST_LOG_SEV(lg(), debug)
                << "Scheduler holds " << report_jobs.size() << " report job(s)";
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Could not read the scheduler's jobs: " << e.what();
    }

    // A name that holds an accounted job is a definition's live schedule. Any
    // other report job is removed below, so it must not read as present when
    // the jobs that do have consent are put back.
    std::set<std::string> live_job_names;
    for (const auto& [name, id] : report_jobs)
        if (accounted_job_ids.contains(id))
            live_job_names.insert(name);

    int total_removed = 0;
    int total_restored = 0;
    if (scheduler_read_ok) {
        // Step 4: remove every report job no consented definition accounts for.
        for (const auto& job_id : unaccounted_report_jobs(report_jobs, accounted_job_ids)) {
            if (const auto deleted = send_delete_request(job_id, "No run grant behind the job");
                deleted) {
                ++total_removed;
                BOOST_LOG_SEV(lg(), info) << "Removed the unaccounted scheduler job " << job_id;
            } else {
                ++total_failed;
                BOOST_LOG_SEV(lg(), error) << deleted.error();
            }
        }

        // Step 5: a definition that holds a grant but whose job is gone is put
        // back with the recorded id. The grant is the person's consent, so no
        // person is needed for this.
        for (const auto& def : consented) {
            if (live_job_names.contains(scheduler_job_name(def.id)))
                continue;
            if (const auto sent = send_schedule_request(def, *def.scheduler_job_id); sent) {
                ++total_restored;
                BOOST_LOG_SEV(lg(), info)
                    << "Restored scheduler job " << *def.scheduler_job_id << " for definition "
                    << def.id;
            } else {
                ++total_failed;
                BOOST_LOG_SEV(lg(), error) << "Could not restore the job for definition " << def.id
                                           << ": " << sent.error();
            }
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Reconciliation complete. Tenants: " << tenants.size()
                              << ", removed: " << total_removed
                              << ", restored: " << total_restored
                              << ", cleared: " << total_cleared << ", failed: " << total_failed
                              << ".";
    co_return;
}

}
