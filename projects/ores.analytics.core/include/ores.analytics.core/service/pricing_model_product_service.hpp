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
#ifndef ORES_ANALYTICS_SERVICE_PRICING_MODEL_PRODUCT_SERVICE_HPP
#define ORES_ANALYTICS_SERVICE_PRICING_MODEL_PRODUCT_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.analytics.api/domain/pricing_model_product.hpp"
#include "ores.analytics.core/repository/pricing_model_product_repository.hpp"

namespace ores::analytics::service {

/**
 * @brief Service for managing pricing model products.
 */
class pricing_model_product_service {
private:
    inline static std::string_view logger_name =
        "ores.analytics.service.pricing_model_product_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit pricing_model_product_service(context ctx);

    std::vector<domain::pricing_model_product>
    list_products(const std::string& config_id);

    std::optional<domain::pricing_model_product>
    find_product(const std::string& id);

    void save_product(const domain::pricing_model_product& v);

    void save_products(const std::vector<domain::pricing_model_product>& v);

    void remove_product(const std::string& id);

    void remove_products_for_config(const std::string& config_id);

    std::vector<domain::pricing_model_product>
    get_product_history(const std::string& id);

private:
    context ctx_;
    repository::pricing_model_product_repository repo_;
};

}

#endif
