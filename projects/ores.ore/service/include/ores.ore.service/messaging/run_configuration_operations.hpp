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
#ifndef ORES_ORE_SERVICE_MESSAGING_RUN_CONFIGURATION_OPERATIONS_HPP
#define ORES_ORE_SERVICE_MESSAGING_RUN_CONFIGURATION_OPERATIONS_HPP

#include "ores.nats/service/nats_client.hpp"
#include "ores.ore.api/messaging/run_configuration_protocol.hpp"
#include <map>
#include <string>
#include <vector>

/**
 * @file run_configuration_operations.hpp
 * @brief A run's configuration, imported and exported through its owners.
 *
 * The ORE service maps ORE XML to the owners' documents and back, and reaches
 * the documents only through the owners' operations: reporting for the run
 * document and its bindings, refdata for the curve configuration and the
 * conventions, analytics for pricing engines and today's market. It never
 * touches their tables.
 */
namespace ores::ore::service::messaging {

/**
 * @brief The files of an ORE input directory, by the name the run document
 * gives each. The run document itself is ore.xml.
 */
using input_files = std::map<std::string, std::string>;

/**
 * @brief Stores a run's configuration against a report definition.
 *
 * Every file is parsed and mapped before the first write. A failure after it
 * deletes what the import had stored, then throws.
 */
ores::ore::messaging::run_configuration_import_execute_result
import_run(ores::nats::service::nats_client& owners,
           const std::string& report_definition_id,
           const std::string& name,
           const input_files& files);

/**
 * @brief Deletes what an import stored: each document through its owner, then
 * the run document, its bindings and their configuration rows through
 * reporting. Throws when an owner refuses.
 */
void undo_import(ores::nats::service::nats_client& owners,
                 const std::string& report_definition_id,
                 bool run_document_saved,
                 const std::vector<ores::ore::messaging::saved_document>& saved);

/**
 * @brief Rebuilds a report definition's ORE input from its owners.
 */
input_files export_run(ores::nats::service::nats_client& owners,
                       const std::string& report_definition_id);

}

#endif
