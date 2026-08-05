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
#ifndef ORES_DQ_API_DOMAIN_LEI_ENTITY_HPP
#define ORES_DQ_API_DOMAIN_LEI_ENTITY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::dq::domain {

/**
 * @brief GLEIF LEI entity master data from LEI2 golden copy files
 *
 * GLEIF LEI entity master data from LEI2 golden copy files
 */
struct lei_entity final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Legal Entity Identifier — the natural key for this entity.
     */
    std::string lei;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::string entity_legal_name;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::string entity_entity_category;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::optional<std::string> entity_entity_sub_category;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::string entity_entity_status;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::optional<std::string> entity_legal_form_entity_legal_form_code;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::optional<std::string> entity_legal_form_other_legal_form;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::optional<std::string> entity_legal_jurisdiction;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::optional<std::string> entity_legal_address_first_address_line;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::optional<std::string> entity_legal_address_city;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::optional<std::string> entity_legal_address_region;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::string entity_legal_address_country;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::optional<std::string> entity_legal_address_postal_code;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::optional<std::string> entity_headquarters_address_first_address_line;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::optional<std::string> entity_headquarters_address_city;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::optional<std::string> entity_headquarters_address_region;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::optional<std::string> entity_headquarters_address_country;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::optional<std::string> entity_headquarters_address_postal_code;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::optional<std::chrono::system_clock::time_point> entity_entity_creation_date;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::optional<std::chrono::system_clock::time_point> registration_initial_registration_date;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::optional<std::chrono::system_clock::time_point> registration_last_update_date;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::optional<std::chrono::system_clock::time_point> registration_next_renewal_date;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::optional<std::string> registration_registration_status;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::optional<std::string> entity_transliterated_name_1;

    /**
     * @brief GLEIF LEI entity master data from LEI2 golden copy files
     */
    std::optional<std::string> entity_transliterated_name_1_type;

    /**
     * @brief Username of the person who last modified this LEI entity.
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
 * @brief Dispatch-key identifier for lei_entity, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const lei_entity&) {
    return "ores.dq.lei_entity";
}

}

#endif
