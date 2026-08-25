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

#include <boost/uuid/uuid.hpp>
#include <string>
#include <string_view>

namespace ores::compute::domain {

/**
 * @brief Links an app version to a supported platform and its package URI.
 *
 * Associates app versions with the platforms they support, and carries the URI
 * of the per-platform packaged bundle (a .tar.gz containing the wrapper+engine
 * binaries built for that target triplet). Each (app_version, platform) row
 * owns its own package_uri, so the orchestrator can dispatch per-triplet
 * assignments without the wrapper needing to negotiate package selection.
 *
 * The junction uses the full generated stack: NATS protocol, handler
 * and registrar. The generated list_by_app_version op returns the
 * platform rows enriched with the platform code, and
 * replace_by_app_version replaces the active platform set of an app
 * version. Both are consumed by the desktop console and by the
 * repository tests.
 */
struct app_version_platform final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief FK reference to ores_compute_app_versions_tbl.
     *
     * References ores_compute_app_versions_tbl.id (soft FK).
     */
    boost::uuids::uuid app_version_id;

    /**
     * @brief FK reference to ores_compute_platforms_tbl.
     *
     * References ores_compute_platforms_tbl.id (soft FK).
     */
    boost::uuids::uuid platform_id;

    /**
     * @brief URI of the per-platform packaged bundle in object storage.
     *
     * One .tar.gz per (app_version, platform). Wrappers download the URI matching their own
     * ORES_PLATFORM_TRIPLET at dispatch time.
     */
    std::string package_uri;

    /**
     * @brief SHA256 checksum of the per-platform packaged bundle, computed server-side by the
     * storage API when the bundle was uploaded.
     *
     * Verified by ores.compute.wrapper against the downloaded archive before extraction, to detect
     * a truncated download, a corrupted upload, or a tampered package.
     */
    std::string sha256;

    /**
     * @brief Username of the person who last modified this app version platform.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Code of the joined platform row.
     *
     * Populated by the generated list_by_app_version op via the
     * enrichment query. Never written to the junction table -- the
     * table has no code column.
     */
    std::string platform_code;
};

/**
 * @brief Dispatch-key identifier for app_version_platform, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const app_version_platform&) {
    return "ores.compute.app_version_platform";
}

}

#endif
