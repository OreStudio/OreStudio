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
#include "ores.analytics.api/generators/pricing_model_product_parameter_generator.hpp"

#include <boost/uuid/random_generator.hpp>
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::analytics::generators {

using ores::utility::generation::generation_keys;

std::vector<domain::pricing_model_product_parameter>
generate_fictional_pricing_model_product_parameters(
    std::size_t n,
    const boost::uuids::uuid& config_id,
    const boost::uuids::uuid& product_id,
    utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(
        generation_keys::modified_by, "system");
    const auto now = ctx.past_timepoint();
    boost::uuids::random_generator gen;

    std::vector<domain::pricing_model_product_parameter> all;
    all.reserve(12);

    // model-scoped parameters
    all.push_back({
        .id = gen(),
        .pricing_model_config_id = config_id,
        .pricing_model_product_id = product_id,
        .parameter_scope = "model",
        .parameter_name = "TestCalibration",
        .parameter_value = "Bootstrap",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });
    all.push_back({
        .id = gen(),
        .pricing_model_config_id = config_id,
        .pricing_model_product_id = product_id,
        .parameter_scope = "model",
        .parameter_name = "TestTolerance",
        .parameter_value = "0.0001",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });
    all.push_back({
        .id = gen(),
        .pricing_model_config_id = config_id,
        .pricing_model_product_id = product_id,
        .parameter_scope = "model",
        .parameter_name = "TestReversionSpeed",
        .parameter_value = "0.03",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });
    // engine-scoped parameters
    all.push_back({
        .id = gen(),
        .pricing_model_config_id = config_id,
        .pricing_model_product_id = product_id,
        .parameter_scope = "engine",
        .parameter_name = "TestGridPoints",
        .parameter_value = "100",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });
    all.push_back({
        .id = gen(),
        .pricing_model_config_id = config_id,
        .pricing_model_product_id = product_id,
        .parameter_scope = "engine",
        .parameter_name = "TestIntegrationScheme",
        .parameter_value = "Trapezoidal",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });
    // global config-scoped parameters (product_id is null)
    all.push_back({
        .id = gen(),
        .pricing_model_config_id = config_id,
        .pricing_model_product_id = std::nullopt,
        .parameter_scope = "global",
        .parameter_name = "TestDiscountCurve",
        .parameter_value = "EUR-EONIA",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });
    all.push_back({
        .id = gen(),
        .pricing_model_config_id = config_id,
        .pricing_model_product_id = std::nullopt,
        .parameter_scope = "global",
        .parameter_name = "TestCurrency",
        .parameter_value = "EUR",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });

    if (n == 0 || n >= all.size())
        return all;

    return std::vector<domain::pricing_model_product_parameter>(
        all.begin(), all.begin() + n);
}

}
