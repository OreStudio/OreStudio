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
#ifndef ORES_ANALYTICS_SERVICE_PRICING_MODEL_PRODUCT_PARAMETER_SERVICE_HPP
#define ORES_ANALYTICS_SERVICE_PRICING_MODEL_PRODUCT_PARAMETER_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.analytics.api/domain/pricing_model_product_parameter.hpp"
#include "ores.analytics.core/repository/pricing_model_product_parameter_repository.hpp"

namespace ores::analytics::service {

/**
 * @brief Service for managing pricing model product parameters.
 */
class pricing_model_product_parameter_service {
private:
    inline static std::string_view logger_name =
        "ores.analytics.service.pricing_model_product_parameter_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit pricing_model_product_parameter_service(context ctx);

    std::vector<domain::pricing_model_product_parameter>
    list_parameters(const std::string& config_id);

    std::vector<domain::pricing_model_product_parameter>
    list_parameters_for_product(const std::string& product_id);

    std::optional<domain::pricing_model_product_parameter>
    find_parameter(const std::string& id);

    void save_parameter(const domain::pricing_model_product_parameter& v);

    void save_parameters(
        const std::vector<domain::pricing_model_product_parameter>& v);

    void remove_parameter(const std::string& id);

    void remove_parameters_for_config(const std::string& config_id);

    void remove_parameters_for_product(const std::string& product_id);

private:
    context ctx_;
    repository::pricing_model_product_parameter_repository repo_;
};

}

#endif
