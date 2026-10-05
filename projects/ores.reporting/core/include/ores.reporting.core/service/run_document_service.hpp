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
#ifndef ORES_REPORTING_CORE_SERVICE_RUN_DOCUMENT_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_RUN_DOCUMENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.reporting.api/domain/configuration.hpp"
#include "ores.reporting.api/domain/report_configuration.hpp"
#include "ores.reporting.api/domain/run_document.hpp"
#include "ores.reporting.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Stores a report definition's run document and binds the
 * configurations it names.
 *
 * The run document is the setup, the analytics with their parameters, and the
 * market bindings. Each configuration document the run names lives with the
 * component that owns it; reporting holds only the configuration row that
 * names it and the binding that puts it in the definition's slot.
 */
class ORES_REPORTING_CORE_EXPORT run_document_service {
public:
    using context = ores::database::context;

    explicit run_document_service(context ctx);

    /**
     * @brief Stores a run document against a report definition.
     *
     * Throws when the definition already holds a run document, or when an
     * analytic sets a parameter no definition describes; nothing is written
     * then.
     */
    void save(const boost::uuids::uuid& report_definition_id, domain::run_document v);

    /**
     * @brief The definition's run document, if it holds one.
     */
    std::optional<domain::run_document> get(const boost::uuids::uuid& report_definition_id);

    /**
     * @brief Creates a configuration of a type and binds it to the definition
     * in that type's slot.
     *
     * The configuration's owning component is the one its type names.
     */
    domain::configuration bind(const boost::uuids::uuid& report_definition_id,
                               const std::string& configuration_type_code,
                               const std::string& name);

    /**
     * @brief Deletes the definition's run document, its bindings and the
     * configuration rows they name. The documents those configurations name
     * belong to their own components, which delete them.
     */
    void remove(const boost::uuids::uuid& report_definition_id);

    /**
     * @brief The definition's configuration bindings, one per slot it fills.
     */
    std::vector<domain::report_configuration>
    bindings(const boost::uuids::uuid& report_definition_id);

private:
    context ctx_;
};

}

#endif
