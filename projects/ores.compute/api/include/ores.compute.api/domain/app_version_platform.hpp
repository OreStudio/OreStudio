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
#ifndef ORES_COMPUTE_DOMAIN_APP_VERSION_PLATFORM_HPP
#define ORES_COMPUTE_DOMAIN_APP_VERSION_PLATFORM_HPP

#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::compute::domain {

/**
 * @brief Association between an app version and a supported target platform,
 * carrying the URI of the per-platform packaged bundle.
 *
 * Each row represents one .tar.gz built for a specific triplet — the wrapper
 * downloads the URI matching its own @c ORES_PLATFORM_TRIPLET at dispatch
 * time.
 */
struct app_version_platform final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief FK reference to ores_compute_app_versions_tbl.
     */
    boost::uuids::uuid app_version_id;

    /**
     * @brief FK reference to ores_compute_platforms_tbl.
     */
    boost::uuids::uuid platform_id;

    /**
     * @brief vcpkg target triplet of the packaged bundle (e.g. 'x64-linux').
     *
     * Denormalised from ores_compute_platforms_tbl.code for dispatch paths
     * that need the code without a second join.
     */
    std::string platform_code;

    /**
     * @brief URI of the per-platform packaged bundle in object storage.
     */
    std::string package_uri;
};

}

#endif
