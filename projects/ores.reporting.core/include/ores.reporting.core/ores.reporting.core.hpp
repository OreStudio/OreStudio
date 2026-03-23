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
#pragma once

// Domain types
#include "ores.reporting.api/domain/report_type.hpp"
#include "ores.reporting.api/domain/concurrency_policy.hpp"
#include "ores.reporting.api/domain/report_definition.hpp"
#include "ores.reporting.api/domain/report_instance.hpp"

// Repository
#include "ores.reporting.core/repository/report_type_repository.hpp"
#include "ores.reporting.core/repository/concurrency_policy_repository.hpp"
#include "ores.reporting.core/repository/report_definition_repository.hpp"
#include "ores.reporting.core/repository/report_instance_repository.hpp"

// Service
#include "ores.reporting.core/service/report_type_service.hpp"
#include "ores.reporting.core/service/concurrency_policy_service.hpp"
#include "ores.reporting.core/service/report_definition_service.hpp"
#include "ores.reporting.core/service/report_instance_service.hpp"

// Generators
#include "ores.reporting.core/generators/report_type_generator.hpp"
#include "ores.reporting.core/generators/concurrency_policy_generator.hpp"
#include "ores.reporting.core/generators/report_definition_generator.hpp"
#include "ores.reporting.core/generators/report_instance_generator.hpp"
