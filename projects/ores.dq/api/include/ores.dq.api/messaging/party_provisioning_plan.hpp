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
#ifndef ORES_DQ_API_MESSAGING_PARTY_PROVISIONING_PLAN_HPP
#define ORES_DQ_API_MESSAGING_PARTY_PROVISIONING_PLAN_HPP

#include <string>
#include <vector>

namespace ores::dq::messaging {

/**
 * @brief One party-scoped bundle published, in full, during party
 * provisioning.
 */
struct party_bundle_publish_step {
    std::string bundle_code;
    std::string label;
};

/**
 * @brief The party-scoped bundles published, in order, when provisioning a
 * party. Each is published whole -- no dataset-level opted_in_datasets
 * filter -- so a new bundle member (e.g. a new asset class added to
 * synthetic_realistic) never requires a client code change in either
 * provisioner, only a new row in dq_dataset_bundle_member_populate.sql.
 */
inline const std::vector<party_bundle_publish_step>& party_provisioning_bundle_plan() {
    static const std::vector<party_bundle_publish_step> plan{
        {"risk_management", "organisation structure and risk reporting"},
        {"synthetic_realistic", "synthetic market data configuration"},
        {"marketdata.reference_vintage_2016_02_05", "FX driver rates"},
    };
    return plan;
}

}

#endif
