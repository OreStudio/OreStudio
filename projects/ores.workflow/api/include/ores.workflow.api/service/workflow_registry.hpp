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
#ifndef ORES_WORKFLOW_API_SERVICE_WORKFLOW_REGISTRY_HPP
#define ORES_WORKFLOW_API_SERVICE_WORKFLOW_REGISTRY_HPP

#include "ores.workflow.api/export.hpp"
#include "ores.workflow.api/service/workflow_definition.hpp"
#include <string>
#include <unordered_map>

namespace ores::workflow::service {

/**
 * @brief Registry of all known workflow definitions.
 *
 * Populated at startup before the NATS subscriptions are registered.
 * The workflow engine looks up definitions by type_name to drive execution.
 */
class ORES_WORKFLOW_API_EXPORT workflow_registry {
public:
    /**
     * @brief Register a workflow definition.
     *
     * Overwrites any existing definition with the same type_name.
     */
    void register_definition(workflow_definition def) {
        definitions_.insert_or_assign(def.type_name, std::move(def));
    }

    /**
     * @brief Look up a workflow definition by type name.
     *
     * @return Pointer to the definition, or nullptr if not registered.
     */
    [[nodiscard]] const workflow_definition* find(const std::string& type_name) const {
        const auto it = definitions_.find(type_name);
        return it != definitions_.end() ? &it->second : nullptr;
    }

    /**
     * @brief Returns all registered workflow definitions.
     */
    [[nodiscard]] const std::unordered_map<std::string, workflow_definition>& all() const {
        return definitions_;
    }

private:
    std::unordered_map<std::string, workflow_definition> definitions_;
};

}

#endif
