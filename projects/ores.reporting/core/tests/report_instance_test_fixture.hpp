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
#ifndef ORES_REPORTING_CORE_TESTS_REPORT_INSTANCE_TEST_FIXTURE_HPP
#define ORES_REPORTING_CORE_TESTS_REPORT_INSTANCE_TEST_FIXTURE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.reporting.api/domain/report_instance.hpp"
#include "ores.reporting.api/domain/report_trigger_event.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace ores::reporting::tests {

/**
 * @brief Writes the trigger row a generated report instance names.
 *
 * report_instance.trigger_id is a real foreign key, so an instance cannot be
 * written without the trigger it points at. A generated instance names its own
 * uuid as its trigger, so a fixture that has one writes it from the instance
 * alone, and the instance it points at is the one the caller holds.
 *
 * The trigger is on demand, because a fixture asks for the run rather than a
 * schedule firing it, and it carries the instance's own definition and party so
 * the trigger's checks and the instance's foreign key both pass.
 *
 * @return The trigger that was written.
 */
ores::reporting::domain::report_trigger_event
write_trigger_for(ores::database::context ctx,
                  ores::utility::generation::generation_context& gen,
                  const ores::reporting::domain::report_instance& instance);

}

#endif
