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
#ifndef ORES_TESTING_PUBLISH_HELPER_HPP
#define ORES_TESTING_PUBLISH_HELPER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.testing/export.hpp"
#include <string>

namespace ores::testing {

/**
 * @brief Publishes a staged dataset to a tenant, the way the runtime
 *        provisioning does.
 *
 * The database build stages the DQ artefacts and publishes nothing, because
 * publication is a runtime act driven by the ACME provisioning. A test that
 * reads published reference data therefore publishes the datasets it needs in
 * its own setup, rather than relying on a build-time bootstrap that no longer
 * runs.
 *
 * The call is the one the publish service makes: it resolves the dataset by
 * code and invokes the function the dataset's artefact type names, against the
 * tenant. A dataset the tree does not carry is not an error; the call reports
 * false so the test can say what is missing.
 *
 * @param ctx The database context.
 * @param tenant The tenant to publish into.
 * @param dataset_code The dataset to publish, such as "iso.countries".
 * @param publish_function The SQL function that publishes it, such as
 *        "ores_refdata_publish_countries_from_dq_fn".
 * @return True when the dataset exists and the function ran.
 */
ORES_TESTING_EXPORT bool publish_dataset(const ores::database::context& ctx,
                                         const std::string& tenant,
                                         const std::string& dataset_code,
                                         const std::string& publish_function);

/**
 * @brief Publishes a refdata dataset by its entity name.
 *
 * Shorthand for publish_dataset() with the refdata function-name convention:
 * the entity "countries" calls ores_refdata_publish_countries_from_dq_fn.
 */
ORES_TESTING_EXPORT bool publish_refdata_dataset(const ores::database::context& ctx,
                                                 const std::string& tenant,
                                                 const std::string& dataset_code,
                                                 const std::string& entity);

}

#endif
