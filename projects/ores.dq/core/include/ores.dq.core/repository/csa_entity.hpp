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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_entity.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_DQ_CORE_REPOSITORY_CSA_ENTITY_HPP
#define ORES_DQ_CORE_REPOSITORY_CSA_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::dq::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a csa in the database.
 */
struct csa_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_dq_csas_artefact_tbl";

    sqlgen::PrimaryKey<std::string> netting_set_code;
    std::string tenant_id;
    bool is_active = false;
    std::optional<std::string> bilateral;
    std::optional<std::string> csa_currency;
    std::optional<std::string> index_name;
    std::optional<double> threshold_pay;
    std::optional<double> threshold_receive;
    std::optional<double> minimum_transfer_amount_pay;
    std::optional<double> minimum_transfer_amount_receive;
    std::optional<double> independent_amount_held;
    std::optional<std::string> independent_amount_type;
    std::optional<std::string> call_frequency;
    std::optional<std::string> post_frequency;
    std::optional<std::string> margin_period_of_risk;
    std::optional<double> collateral_compounding_spread_receive;
    std::optional<double> collateral_compounding_spread_pay;
    std::optional<bool> apply_initial_margin;
    std::optional<std::string> initial_margin_type;
    std::optional<bool> calculate_im_amount;
    std::optional<bool> calculate_vm_amount;
    std::optional<std::string> non_exempt_im_regulations;
    std::optional<std::string> eligible_currencies;
};

std::ostream& operator<<(std::ostream& s, const csa_entity& v);

}

#endif
