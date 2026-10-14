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
#include "ores.reporting.api/generators/report_trigger_event_generator.hpp"
#include "ores.reporting.core/repository/report_trigger_event_repository.hpp"

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

}
