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
#ifndef ORES_REFDATA_CORE_REPOSITORY_CSA_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_CSA_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::refdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a csa in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct csa_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_csas_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    boost::uuids::uuid netting_set_id;
    boost::uuids::uuid party_id;
    bool is_active = false;
    std::optional<std::string> bilateral;
    std::optional<std::string> csa_currency;
    std::optional<std::string> index_name;
    std::optional<ores::utility::decimal::decimal> threshold_pay;
    std::optional<ores::utility::decimal::decimal> threshold_receive;
    std::optional<ores::utility::decimal::decimal> minimum_transfer_amount_pay;
    std::optional<ores::utility::decimal::decimal> minimum_transfer_amount_receive;
    std::optional<ores::utility::decimal::decimal> independent_amount_held;
    std::optional<std::string> independent_amount_type;
    std::optional<std::string> call_frequency;
    std::optional<std::string> post_frequency;
    std::optional<std::string> margin_period_of_risk;
    std::optional<ores::utility::decimal::decimal> collateral_compounding_spread_receive;
    std::optional<ores::utility::decimal::decimal> collateral_compounding_spread_pay;
    std::optional<bool> apply_initial_margin;
    std::optional<std::string> initial_margin_type;
    std::optional<bool> calculate_im_amount;
    std::optional<bool> calculate_vm_amount;
    std::optional<std::string> non_exempt_im_regulations;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = db_timestamp::max();
    db_timestamp valid_to = db_timestamp::max();

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = csa_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("netting_set_id", &e::netting_set_id) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("is_active", &e::is_active) |
               query_builder::bind("bilateral", &e::bilateral) |
               query_builder::bind("csa_currency", &e::csa_currency) |
               query_builder::bind("index_name", &e::index_name) |
               query_builder::bind("threshold_pay", &e::threshold_pay) |
               query_builder::bind("threshold_receive", &e::threshold_receive) |
               query_builder::bind("minimum_transfer_amount_pay", &e::minimum_transfer_amount_pay) |
               query_builder::bind("minimum_transfer_amount_receive",
                                   &e::minimum_transfer_amount_receive) |
               query_builder::bind("independent_amount_held", &e::independent_amount_held) |
               query_builder::bind("independent_amount_type", &e::independent_amount_type) |
               query_builder::bind("call_frequency", &e::call_frequency) |
               query_builder::bind("post_frequency", &e::post_frequency) |
               query_builder::bind("margin_period_of_risk", &e::margin_period_of_risk) |
               query_builder::bind("collateral_compounding_spread_receive",
                                   &e::collateral_compounding_spread_receive) |
               query_builder::bind("collateral_compounding_spread_pay",
                                   &e::collateral_compounding_spread_pay) |
               query_builder::bind("apply_initial_margin", &e::apply_initial_margin) |
               query_builder::bind("initial_margin_type", &e::initial_margin_type) |
               query_builder::bind("calculate_im_amount", &e::calculate_im_amount) |
               query_builder::bind("calculate_vm_amount", &e::calculate_vm_amount) |
               query_builder::bind("non_exempt_im_regulations", &e::non_exempt_im_regulations) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const csa_entity& v);

}

#endif
