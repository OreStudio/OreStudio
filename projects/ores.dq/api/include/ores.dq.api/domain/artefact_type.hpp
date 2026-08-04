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
#ifndef ORES_DQ_API_DOMAIN_ARTEFACT_TYPE_HPP
#define ORES_DQ_API_DOMAIN_ARTEFACT_TYPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::dq::domain {

/**
 * @brief Maps artefact type codes to their NATS dispatch subjects.
 *
 * Artefact types define the mapping between dataset artefact codes and the
 * database infrastructure needed to publish them: the artefact table where
 * staging data is stored, the target production table where data is
 * published, and the NATS subject dispatched by the workflow engine to
 * perform the publication. Rows are authored directly (not mirrored from
 * an external source).
 */
struct artefact_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique code identifying this artefact type.
     *
     * Examples: "countries", "currencies", "images".
     */
    std::string code;

    /**
     * @brief Human-readable name for display purposes.
     */
    std::string name;

    /**
     * @brief Detailed description of this artefact type.
     */
    std::optional<std::string> description;

    /**
     * @brief Name of the artefact (staging) table for this type. This is where data is staged
     * before publication. Example: "dq_countries_artefact_tbl".
     */
    std::optional<std::string> artefact_table;

    /**
     * @brief Name of the target (production) table for this type. This is where data is published
     * to. Example: "refdata_countries_tbl".
     */
    std::optional<std::string> target_table;

    /**
     * @brief NATS subject dispatched by the workflow engine to publish data from the DQ artefact
     * table to the target service table. Example: "refdata.v1.countries.publish-from-dq".
     */
    std::optional<std::string> target_subject;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this artefact type.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

/**
 * @brief Dispatch-key identifier for artefact_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const artefact_type&) {
    return "ores.dq.artefact_type";
}

}

#endif
