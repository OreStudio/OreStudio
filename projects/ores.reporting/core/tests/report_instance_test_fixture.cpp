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
#include "report_instance_test_fixture.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.reporting.api/generators/report_instance_generator.hpp"
#include "ores.reporting.api/generators/report_trigger_event_generator.hpp"
#include "ores.reporting.core/repository/report_instance_kind_repository.hpp"
#include "ores.reporting.core/repository/report_instance_repository.hpp"
#include "ores.reporting.core/repository/report_trigger_event_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>

namespace ores::reporting::tests {

using ores::reporting::generators::generate_synthetic_report_trigger_event;

domain::report_trigger_event write_trigger_for(ores::database::context ctx,
                                               ores::utility::generation::generation_context& gen,
                                               const domain::report_instance& instance) {
    auto trigger = generate_synthetic_report_trigger_event(gen);
    trigger.id = instance.id;
    trigger.report_definition_id = instance.definition_id;
    trigger.trigger_type = "on_demand";
    trigger.party_id = instance.party_id;
    repository::report_trigger_event_repository().write(ctx, trigger);
    return trigger;
}


ores::database::context
write_party_and_scope_context(ores::testing::scoped_database_helper& h,
                              ores::utility::generation::generation_context& gen) {
    using ores::refdata::repository::party_repository;
    party_repository parties;
    auto party = ores::refdata::generators::generate_synthetic_party(gen);
    party.change_reason_code = "system.test";
    for (const auto& existing : parties.read_latest(h.context())) {
        if (existing.tenant_id == party.tenant_id) {
            party.parent_party_id = existing.id;
            break;
        }
    }
    parties.write(h.context(), party);
    return h.context().with_party(h.tenant_id(), party.id, {party.id}, h.db_user());
}

std::map<std::string, boost::uuids::uuid> lifecycle_states(const ores::database::context& ctx,
                                                           ores::logging::logger_t& lg) {
    const auto rows = ores::database::repository::execute_parameterized_multi_column_query(
        ctx,
        "SELECT s.name, s.id::text"
        " FROM ores_dq_fsm_states_tbl s"
        " JOIN ores_dq_fsm_machines_tbl m ON m.id = s.machine_id"
        " WHERE m.name = 'report_instance_lifecycle'"
        "   AND s.valid_to = ores_utility_infinity_timestamp_fn()"
        "   AND m.valid_to = ores_utility_infinity_timestamp_fn()",
        {},
        lg,
        "Reading the report instance lifecycle states");

    std::map<std::string, boost::uuids::uuid> states;
    for (const auto& row : rows) {
        if (row.size() >= 2 && row[0] && row[1])
            states.emplace(*row[0], boost::uuids::string_generator()(*row[1]));
    }
    return states;
}

boost::uuids::uuid write_run_in_state(const ores::database::context& party_ctx,
                                      const std::string& db_user,
                                      ores::utility::generation::generation_context& gen,
                                      const boost::uuids::uuid& state_id,
                                      std::optional<boost::uuids::uuid> definition_id) {
    auto instance = ores::reporting::generators::generate_synthetic_report_instance(gen);
    instance.party_id = *party_ctx.party_id();
    instance.fsm_state_id = state_id;
    if (definition_id)
        instance.definition_id = *definition_id;

    ores::reporting::repository::report_instance_kind_repository kinds;
    const auto catalogue =
        kinds.read_latest(party_ctx.with_tenant(ores::utility::uuid::tenant_id::system(), db_user));
    REQUIRE_FALSE(catalogue.empty());
    instance.kind = catalogue.front().code;
    instance.change_reason_code = "system.test";
    instance.trigger_id = write_trigger_for(party_ctx, gen, instance).id;

    ores::reporting::repository::report_instance_repository().write(party_ctx, instance);
    return instance.id;
}

void move_to(ores::reporting::service::report_instance_service& service,
             const boost::uuids::uuid& instance_id,
             const boost::uuids::uuid& state_id) {
    auto instance = service.get_instance(instance_id);
    REQUIRE(instance.has_value());
    instance->fsm_state_id = state_id;
    service.save_instance(*instance);
}

}
