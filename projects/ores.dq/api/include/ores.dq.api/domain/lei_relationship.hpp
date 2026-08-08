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
#ifndef ORES_DQ_API_DOMAIN_LEI_RELATIONSHIP_HPP
#define ORES_DQ_API_DOMAIN_LEI_RELATIONSHIP_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::dq::domain {

/**
 * @brief GLEIF LEI corporate hierarchy relationships from RR golden copy files
 *
 * GLEIF LEI corporate hierarchy relationships from RR golden copy files
 */
struct lei_relationship final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Legal Entity Identifier of the relationship's start node — the natural key for this
     * entity.
     */
    std::string relationship_start_node_node_id;

    /**
     * @brief GLEIF LEI corporate hierarchy relationships from RR golden copy files
     */
    std::string relationship_start_node_node_id_type;

    /**
     * @brief GLEIF LEI corporate hierarchy relationships from RR golden copy files
     */
    std::string relationship_end_node_node_id;

    /**
     * @brief GLEIF LEI corporate hierarchy relationships from RR golden copy files
     */
    std::string relationship_end_node_node_id_type;

    /**
     * @brief GLEIF LEI corporate hierarchy relationships from RR golden copy files
     */
    std::string relationship_relationship_type;

    /**
     * @brief GLEIF LEI corporate hierarchy relationships from RR golden copy files
     */
    std::string relationship_relationship_status;

    /**
     * @brief GLEIF LEI corporate hierarchy relationships from RR golden copy files
     */
    std::optional<std::chrono::system_clock::time_point> relationship_period_1_start_date;

    /**
     * @brief GLEIF LEI corporate hierarchy relationships from RR golden copy files
     */
    std::optional<std::chrono::system_clock::time_point> relationship_period_1_end_date;

    /**
     * @brief GLEIF LEI corporate hierarchy relationships from RR golden copy files
     */
    std::optional<std::chrono::system_clock::time_point> registration_initial_registration_date;

    /**
     * @brief GLEIF LEI corporate hierarchy relationships from RR golden copy files
     */
    std::optional<std::chrono::system_clock::time_point> registration_last_update_date;

    /**
     * @brief GLEIF LEI corporate hierarchy relationships from RR golden copy files
     */
    std::optional<std::string> registration_registration_status;

    /**
     * @brief GLEIF LEI corporate hierarchy relationships from RR golden copy files
     */
    std::optional<std::string> registration_validation_sources;

    /**
     * @brief Username of the person who last modified this LEI relationship.
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
 * @brief Dispatch-key identifier for lei_relationship, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const lei_relationship&) {
    return "ores.dq.lei_relationship";
}

}

#endif
