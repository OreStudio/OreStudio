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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_INBOX_CORE_TESTS_APPROVAL_FIXTURE_HPP
#define ORES_INBOX_CORE_TESTS_APPROVAL_FIXTURE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/account.hpp"
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"

namespace ores::inbox::tests {

/**
 * @brief Writes a synthetic account, so a test has a person to raise or
 * decide a request.
 */
inline ores::iam::domain::account seed_account(ores::testing::scoped_database_helper& h) {
    auto ctx = ores::testing::make_generation_context(h);
    auto a = ores::iam::generators::generate_synthetic_account(ctx);
    a.change_reason_code = "system.test";
    ores::iam::repository::account_repository repo;
    repo.write(h.context(), a);
    return a;
}

/**
 * @brief The context of a person acting in the test tenant.
 *
 * The database stamps every row with the account that wrote it, and refuses a
 * write that names nobody, as production's signed-in context always names one.
 */
inline ores::database::context acting(ores::testing::scoped_database_helper& h) {
    return h.context().with_tenant(h.tenant_id(), h.db_user());
}

/**
 * @brief The context the expiry sweep runs under.
 *
 * It comes from the scheduler, so nobody is signed in and there is no actor;
 * the service account is the only name the write has. A sweep that reached
 * for the actor instead of the service account would name nobody and be
 * refused by the store.
 */
inline ores::database::context sweeping(ores::testing::scoped_database_helper& h) {
    return h.context().with_tenant(h.tenant_id(), "");
}

}

#endif
