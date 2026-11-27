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
#ifndef ORES_COMPUTE_WRAPPER_APP_PACKAGE_CACHE_HPP
#define ORES_COMPUTE_WRAPPER_APP_PACKAGE_CACHE_HPP

#include "ores.compute.api/messaging/work_protocol.hpp"
#include "ores.compute.wrapper/export.hpp"
#include <filesystem>
#include <string>

namespace ores::compute::wrapper::app {

/**
 * @brief The name a package is cached under.
 *
 * Two app versions whose bytes are the same are one package, so the name is the
 * package's digest and not the version that asked for it. A message that states
 * no digest falls back to the app version, which is the name the cache had
 * before it was keyed this way.
 */
[[nodiscard]] ORES_COMPUTE_WRAPPER_EXPORT std::string
package_cache_key(const messaging::work_assignment_event& evt);

/**
 * @brief The directory a package is unpacked into, under the packages root.
 *
 * Every wrapper on a host is given the same root, so the directory is the one
 * copy of a package the host holds.
 */
[[nodiscard]] ORES_COMPUTE_WRAPPER_EXPORT std::filesystem::path
package_cache_dir(const std::filesystem::path& packages_root,
                  const messaging::work_assignment_event& evt);

/**
 * @brief Unpacks an archive into the cache directory and removes the archive.
 *
 * The archive is the transport and the tree is the cache, so nothing keeps both.
 * The tree is unpacked beside its final name and renamed into place, so its
 * manifest exists only in a finished package: a wrapper that reads no manifest
 * downloads again rather than running half a tree.
 */
ORES_COMPUTE_WRAPPER_EXPORT void unpack_package(const std::filesystem::path& pkg_cache_dir,
                                                const std::filesystem::path& pkg_archive);

}

#endif
