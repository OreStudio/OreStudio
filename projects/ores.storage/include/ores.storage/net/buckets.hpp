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
#ifndef ORES_STORAGE_NET_BUCKETS_HPP
#define ORES_STORAGE_NET_BUCKETS_HPP

#include <string_view>

namespace ores::storage::net {

/**
 * @brief Well-known bucket names for the object storage API.
 *
 * Each bucket is a logical namespace within the storage server. The
 * filesystem backend maps each bucket to a subdirectory of the configured
 * storage root. Bucket names use kebab-case and are the single source of
 * truth for all clients.
 */
struct buckets {
    /**
     * @brief Compute application bundles (wrapper + engine tarballs).
     *
     * Objects are keyed by app_version_id (typically a UUID). Uploaded
     * once by administrators; downloaded by compute nodes before each job.
     */
    static constexpr std::string_view compute_packages = "compute-packages";

    /**
     * @brief Compute workunit input artifacts.
     *
     * Objects are keyed by "{workunit_id}/{artifact}" where {artifact}
     * may include an extension (e.g. "input.tar.gz") to preserve the
     * original filename for local inspection on the node.
     */
    static constexpr std::string_view compute_inputs = "compute-inputs";

    /**
     * @brief Compute result outputs uploaded by wrappers after job completion.
     *
     * Objects are keyed by result_id (a UUID). Downloaded by clients
     * when retrieving job results.
     */
    static constexpr std::string_view compute_outputs = "compute-outputs";

    /**
     * @brief ORE directory import tarballs.
     *
     * Objects are keyed by import_id (a UUID). Uploaded by clients
     * before triggering a server-side import; deleted by the server
     * once the import completes or fails.
     */
    static constexpr std::string_view ore_imports = "ore-imports";
};

}

#endif
