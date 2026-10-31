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
#include "ores.reporting.core/service/report_run_admission_service.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.reporting.api/domain/report_instance.hpp"
#include "ores.reporting.api/domain/report_trigger_event.hpp"
#include "ores.reporting.core/repository/report_trigger_event_repository.hpp"
#include "ores.reporting.core/service/report_instance_service.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>

namespace ores::reporting::service {

using namespace ores::logging;

namespace {

/**
 * @brief The trigger types the lookup seeds, stated once so a reader sees the
 * exact set this service writes and the unknown branch is what catches a
 * third.
 */
constexpr std::string_view trigger_type_on_demand = "on_demand";
constexpr std::string_view trigger_type_scheduled = "scheduled";

/**
 * @brief Whether the plan describes a scheduled trigger.
 *
 * The type is the caller's statement, but the scheduler's own fields say what
 * the caller meant, and a plan that disagrees with itself is refused before the
 * database's check has to catch it.
 */
bool is_scheduled(const std::string& trigger_type) {
    return trigger_type == trigger_type_scheduled;
}

}

report_run_admission_service::report_run_admission_service(context ctx)
    : ctx_(std::move(ctx)) {}

report_run_admission_service::receipt report_run_admission_service::admit(const plan& p) {
    // The instance's foreign key carries the tenant as well as the id, so the
    // trigger and the instance must land in one tenant. The context is the
    // caller's because it also carries the party scope the isolation policies
    // read; a context scoped to another tenant would write the trigger where
    // the instance is not, and the foreign key would refuse it.
    if (ctx_.tenant_id() != p.tenant_id)
        throw std::invalid_argument(
            "A report run is admitted in the tenant the context is scoped to, and this plan names "
            "another.");
    if (p.trigger_type != trigger_type_on_demand && !is_scheduled(p.trigger_type))
        throw std::invalid_argument("A report trigger is on_demand or scheduled, not '" +
                                    p.trigger_type + "'.");
    if (is_scheduled(p.trigger_type) && !p.scheduler_job_id)
        throw std::invalid_argument(
            "A scheduled report trigger names the scheduler job that fired, and this plan names "
            "none.");
    if (!is_scheduled(p.trigger_type) && (p.scheduler_job_id || p.schedule_job_run_id))
        throw std::invalid_argument(
            "An on-demand report trigger names no scheduler job and no firing, and this plan "
            "names one.");

    boost::uuids::random_generator rg;
    const auto trigger_id = rg();
    const auto now = std::chrono::system_clock::now();

    domain::report_trigger_event trigger;
    trigger.id = trigger_id;
    trigger.tenant_id = p.tenant_id;
    trigger.report_definition_id = p.definition_id;
    trigger.trigger_type = p.trigger_type;
    trigger.scheduler_job_id = p.scheduler_job_id;
    trigger.schedule_job_run_id = p.schedule_job_run_id;
    // A service account is the actor when no person's token resolved one, which
    // is the scheduler's case: its trigger records the account the schedule
    // acts under.
    trigger.triggered_by = ctx_.actor().empty() ? ctx_.service_account() : ctx_.actor();
    trigger.triggered_at = now;
    trigger.party_id = p.party_id;

    repository::report_trigger_event_repository().write(ctx_, trigger);
    BOOST_LOG_SEV(lg(), info) << "Recorded " << p.trigger_type << " report trigger "
                              << boost::uuids::to_string(trigger_id) << " for definition "
                              << boost::uuids::to_string(p.definition_id);

    domain::report_instance inst;
    inst.id = rg();
    inst.tenant_id = p.tenant_id;
    inst.party_id = p.party_id;
    inst.definition_id = p.definition_id;
    inst.trigger_id = trigger_id;
    inst.run_grant_id = p.run_grant_id;
    inst.engine_settings_id = p.engine_settings_id;
    inst.engine_settings_version = p.engine_settings_version;
    inst.cut_id = p.cut_id;
    inst.cut_label = p.cut_label;
    inst.cut_kind_code = p.cut_kind_code;
    inst.cut_timepoint = p.cut_timepoint;
    inst.name = p.name;
    inst.kind = p.kind;
    inst.description = p.description;
    inst.fsm_state_id = p.fsm_state_id;
    inst.output_message = p.output_message;
    inst.started_at = p.started_at;
    inst.modified_by = ctx_.service_account();
    inst.performed_by = ctx_.service_account();
    inst.change_reason_code = "system.new_record";
    inst.change_commentary =
        p.output_message.empty() ? "Created by report trigger" : p.output_message;

    report_instance_service(ctx_).save_instance(inst);

    return receipt{.trigger_id = trigger_id, .instance_id = inst.id};
}

}
