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
#ifndef ORES_SYNTHETIC_SERVICE_TESTS_PROVIDER_SEED_HPP
#define ORES_SYNTHETIC_SERVICE_TESTS_PROVIDER_SEED_HPP

#include "ores.database/service/tenant_context.hpp"
#include "ores.synthetic.api/domain/market_data_provider.hpp"
#include "ores.synthetic.api/generators/market_data_provider_generator.hpp"
#include "ores.synthetic.core/repository/market_data_provider_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <string>

namespace ores::synthetic::service::tests {

/**
 * @brief Makes the system tenant hold the named provider, so a configuration
 *        may name it.
 *
 * The insert trigger validates a configuration's =provider_code= against the
 * system tenant's providers. Those rows reach a database through the DQ
 * publish step, which a test database never runs, so a suite that writes a
 * configuration seeds the provider it names instead of assuming the catalogue.
 *
 * It also stops a suite from depending on the catalogue being empty. The
 * validation passes any code through while the system tenant holds no
 * providers, so a probe another suite writes into the system tenant turns that
 * pass-through off for as long as it lives, and a concurrent suite naming a
 * real provider then fails.
 */
inline void ensure_system_market_data_provider(ores::testing::scoped_database_helper& h,
                                               const std::string& code) {
    namespace repo = ores::synthetic::repository;
    using tenant_context = ores::database::service::tenant_context;

    auto system_ctx = tenant_context::with_system_tenant(h.context());
    repo::market_data_provider_repository providers;
    if (!providers.read_latest(system_ctx, code).empty()) {
        return;
    }

    auto gen = ores::testing::make_generation_context(h);
    auto provider = ores::synthetic::generators::generate_synthetic_market_data_provider(gen);
    provider.code = code;
    provider.tenant_id =
        ores::utility::uuid::tenant_id::from_string(tenant_context::system_tenant_id).value();
    provider.change_reason_code = "system.test";
    provider.change_commentary = "provider seed for a configuration test";
    providers.write(system_ctx, provider);
}

}

#endif
