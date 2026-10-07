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
#include "ores.testing/publish_helper.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"
#include <string>
#include <vector>

namespace ores::testing {
namespace {

inline static std::string_view logger_name = "ores.testing.publish_helper";

static auto& lg() {
    using namespace ores::logging;
    static auto instance = make_logger(logger_name);
    return instance;
}

}

bool publish_dataset(const ores::database::context& ctx,
                     const std::string& tenant,
                     const std::string& dataset_code,
                     const std::string& publish_function) {
    // The dataset row and the function's result rows are cross-joined, so the
    // count is zero exactly when the tree carries no such dataset. The function
    // itself runs as a side effect whether or not it returns rows.
    const auto counts = ores::database::repository::execute_parameterized_string_query(
        ctx,
        "SELECT count(*)::text FROM ores_dq_datasets_tbl d, " + publish_function +
            "(d.id, $1::uuid) p WHERE d.code = $2"
            " AND d.valid_to = ores_utility_infinity_timestamp_fn()",
        {tenant, dataset_code},
        lg(),
        "Publishing " + dataset_code);

    return !counts.empty() && counts.front() != "0";
}

bool publish_refdata_dataset(const ores::database::context& ctx,
                             const std::string& tenant,
                             const std::string& dataset_code,
                             const std::string& entity) {
    return publish_dataset(
        ctx, tenant, dataset_code, "ores_refdata_publish_" + entity + "_from_dq_fn");
}

}
